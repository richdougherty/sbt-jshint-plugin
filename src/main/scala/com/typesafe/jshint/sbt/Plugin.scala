package com.typesafe.jshint.sbt

import sbt._
import sbt.Keys._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import spray.json.{ JsObject, pimpString }
import com.typesafe.web.sbt.WebPlugin.WebKeys
import com.typesafe.web.sbt.incremental._
import com.typesafe.jse.sbt.JsEnginePlugin.JsEngineKeys
import com.typesafe.jse.{Rhino, PhantomJs, Node, CommonNode}
import com.typesafe.jshint.Jshinter
import com.typesafe.web.sbt._
import sbt.File
import scala.Some
import xsbti.{CompileFailed, Severity, Problem}


/**
 * The sbt plugin plumbing around the JSHint library.
 */
object JSHintPlugin extends sbt.Plugin {

  object JshintKeys {

    val jshint = TaskKey[Unit]("jshint", "Perform JavaScript linting.")
    val jshintTest = TaskKey[Unit]("jshint-test", "Perform JavaScript linting for tests.")

    val jshintOptions = TaskKey[JsObject]("jshint-options", "An array of jshint options to pass to the linter. This options are found via jshint-resolved-config. If there is no config then the options will be specified such that the JSHint defaults are used.")

    val config = SettingKey[Option[File]]("jshint-config", "The location of a JSHint configuration file.")
    val resolvedConfig = TaskKey[Option[File]]("jshint-resolved-config", "The actual location of a JSHint configuration file if present. If jshint-config is none then the task will seek a .jshintrc in the project folder. If that's not found then .jshintrc will be searched for in the user's home folder. This behaviour is consistent with other JSHint tooling.")

    val jsFiles = TaskKey[Seq[File]]("jshint-modified-js-files", "Determine the Js files that are modified.")
    val jsTestFiles = TaskKey[Seq[File]]("jshint-modified-js-test-files", "Determine the Js files that are modified for test.")

    val shellSource = SettingKey[File]("jshint-shelljs-source", "The target location of the js shell script to use.")
    val jshintSource = SettingKey[File]("jshint-jshintjs-source", "The target location of the jshint script to use.")

  }

  import WebKeys._
  import JshintKeys._
  import JsEngineKeys._

  def jshintSettings = Seq(

    config := None,
    resolvedConfig <<= (config, baseDirectory) map resolveConfigTask,
    jshintOptions <<= resolvedConfig map jshintOptionsTask,

    jsFiles := ((unmanagedSources in Assets).value ** (jsFilter in Assets).value).get,
    jsTestFiles := ((unmanagedSources in TestAssets).value ** (jsFilter in TestAssets).value).get,

    shellSource := getFileInTarget(target.value, "shell.js"),
    // FIXME: This resource will eventually be located from its webjar. For now
    // we use a webjar until the webjar is updated with my fix (waiting for a
    // new release of jshint).
    jshintSource := getFileInTarget(target.value, "jshint.js"),

    jshint <<= (
      state,
      shellSource,
      jshintSource,
      jsFiles,
      jshintOptions,
      engineType,
      parallelism,
      streams,
      reporter
      ) map (jshintTask(_, _, _, _, _, _, _, _, _, testing = false)),
    jshintTest <<= (
      state,
      shellSource,
      jshintSource,
      jsTestFiles,
      jshintOptions,
      engineType,
      parallelism,
      streams,
      reporter
      ) map (jshintTask(_, _, _, _, _, _, _, _, _, testing = true)),

    compile in Compile <<= (compile in Compile).dependsOn(jshint),
    test in Test <<= (test in Test).dependsOn(jshint, jshintTest)

  )

  private def resolveConfigTask(someConfig: Option[File], baseDirectory: File): Option[File] = {
    someConfig.orElse {
      val JsHintRc = ".jshintrc"
      val projectRc = baseDirectory / JsHintRc
      if (projectRc.exists()) {
        Some(projectRc)
      } else {
        val homeRc = file(System.getProperty("user.home")) / JsHintRc
        if (homeRc.exists()) {
          Some(homeRc)
        } else {
          None
        }
      }
    }
  }

  private def jshintOptionsTask(someConfig: Option[File]): JsObject = {
    someConfig
      .map(config => IO.read(config).asJson.asJsObject)
      .getOrElse(JsObject())
  }

  // FIXME: Abstract this into sbt-web?
  private def getFileInTarget(target: File, name: String): File = {
    val is = this.getClass.getClassLoader.getResourceAsStream(name)
    try {
      val f = target / this.getClass.getSimpleName / name
      IO.transfer(is, f)
      f
    } finally {
      is.close()
    }
  }

  private def jshintTask(
                          state: State,
                          shellSource: File,
                          jshintSource: File,
                          jsSources: Seq[File],
                          jshintOptions: JsObject,
                          engineType: EngineType.Value,
                          parallelism: Int,
                          streams: TaskStreams,
                          reporter: LoggerReporter,
                          testing: Boolean
                          ): Unit = {

    import WebPlugin._

    val engineProps = engineType match {
      case EngineType.CommonNode => CommonNode.props()
      case EngineType.Node => Node.props()
      case EngineType.PhantomJs => PhantomJs.props()
      case EngineType.Rhino => Rhino.props()
    }

    implicit val opInputHasher = OpInputHasher[File](source => OpInputHash.hashString(source + "|" + jshintOptions.compactPrint))
    val problems = runIncremental(streams, jsSources.to[Seq]) { neededSources: Seq[File] =>
      val testKeyword = if (testing) "test " else ""
      if (neededSources.size > 0) {
        streams.log.info(s"JavaScript linting on ${neededSources.size} ${testKeyword}source(s)")
      }

      def lintBatch(sourceBatch: Seq[File]): Future[(Map[File,OpResult], Seq[Problem])] = {
        withActorRefFactory(state, this.getClass.getName) { implicit arf =>
          val engine = arf.actorOf(engineProps)
          val jshinter = Jshinter(engine, shellSource, jshintSource)
          val futureResults = jshinter.lint(sourceBatch, jshintOptions)
          futureResults.map { lintResults: Seq[Jshinter.Result] =>
            lintResults.foldLeft[(Map[File,OpResult], Seq[Problem])]((Map.empty, Seq.empty)) {
              case ((resultMap, problemSeq), Jshinter.Result(source, None)) =>
                val newResult = OpSuccess(filesRead = Set(source), filesWritten = Set.empty)
                (resultMap.updated(source, newResult), problemSeq)
              case ((resultMap, problemSeq), Jshinter.Result(source, Some(lintProblems))) =>
                val newResult = OpFailure
                val newProblems: Seq[Problem] = lintProblems.map { lp =>
                  new LineBasedProblem(
                    lp.message,
                    lp.severity match {
                      case Jshinter.Info => Severity.Info
                      case Jshinter.Warn => Severity.Warn
                      case Jshinter.Error => Severity.Error
                    },
                    lp.lineNumber,
                    lp.characterOffset,
                    lp.lineContent,
                    source
                  )
                }
                (resultMap.updated(source, newResult), problemSeq ++ newProblems)
            }
          }
        }
      }

      val sourceBatches = (neededSources grouped Math.max(neededSources.size / parallelism, 1)).to[Seq]
      val batchResults: Seq[Future[(Map[File,OpResult], Seq[Problem])]] = sourceBatches.map(lintBatch)
      val unbatchedResults: Future[Seq[(Map[File,OpResult], Seq[Problem])]] = Future.sequence(batchResults)
      val combinedResults: Future[(Map[File,OpResult], Seq[Problem])] = unbatchedResults.map { seq: Seq[(Map[File,OpResult], Seq[Problem])] =>
        seq.foldLeft[(Map[File,OpResult], Seq[Problem])]((Map.empty, Seq.empty)) {
          case ((combinedResultMap, combinedProblemSeq), (batchResultMap, batchProblemSeq)) =>
            (combinedResultMap ++ batchResultMap, combinedProblemSeq ++ batchProblemSeq)
        }
      }
      Await.result(combinedResults, 10.seconds): (Map[File,OpResult], Seq[Problem])
    }
    CompileProblems.report(reporter, problems)
  }

}