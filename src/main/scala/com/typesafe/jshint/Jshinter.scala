package com.typesafe.jshint

import java.io._
import akka.util.Timeout
import scala.concurrent.{ExecutionContext, Future}
import spray.json._
import DefaultJsonProtocol._
import akka.actor.ActorRef
import akka.pattern.ask
import com.typesafe.jse.Engine
import com.typesafe.jse.Engine.JsExecutionResult
import scala.collection.immutable

/**
 * This is the main service that performs the linting functionality of the plugin.  Linting is performed
 * using the js-engine service. There is no dependency on sbt and so there is the potential for this to be factored
 * out into its own library and used with other build tools such as Gradle or Maven.
 */
class Jshinter(engine: ActorRef, shellSource: File, jshint: File) {

  /**
   * Perform Jshint on a sequence of files
   * @param filesToLint the file to lint.
   * @param options The Jshint options as a JSONObject structure.
   * @return a Json array of objects structure as per JSHINT.errors i.e.:
   *         line      : The line (relative to 0) at which the lint was found
   *         character : The character (relative to 0) at which the lint was found
   *         reason    : The problem
   *         evidence  : The text line in which the problem occurred
   *         raw       : The raw message before the details were inserted
   *         a         : The first detail
   *         b         : The second detail
   *         c         : The third detail
   *         d         : The fourth detail
   */
  def lint(filesToLint: Seq[File], options: JsObject)(
    implicit timeout: Timeout, ec: ExecutionContext): Future[Seq[Jshinter.Result]] = {

    val args = Seq(
      jshint.getCanonicalPath,
      JsArray(filesToLint.map(x => JsString(x.getCanonicalPath)).toList).toString(),
      options.toString()
    )
    (engine ? Engine.ExecuteJs(shellSource, args.to[immutable.Seq])).mapTo[JsExecutionResult].map {
      result =>
        if (result.exitValue != 0) {
          throw new JshinterFailure(new String(result.error.toArray, "UTF-8"))
        }

        val json = JsonParser(new String(result.output.toArray, "UTF-8"))
        json.convertTo[JsArray].elements.map(tup => parseResultTuple(tup.convertTo[JsArray]))
    }
  }

  import Jshinter._

  private def parseResultTuple(resultTuple: JsArray): Result = {
    val source = new File(resultTuple.elements(0).convertTo[String])
    resultTuple.elements(1) match {
      case JsNull =>
        Result(source, None)
      case errors: JsArray =>
        val problems = errors.elements.map(_.asInstanceOf[JsObject]).map { o =>
          Problem(
            message = o.fields("reason").convertTo[String],
            severity = o.fields("id") match {
              case JsString("(info)") => Info
              case JsString("(warn)") => Warn
              case JsString("(error)") => Error
              case invalid => deserializationError(s"Invalid problem severity: $invalid")
            },
            lineContent = o.fields.get("evidence") match {
              case Some(JsString(line)) => line
              case _ => ""
            },
            lineNumber = o.fields("line").convertTo[Int],
            characterOffset = o.fields("character").convertTo[Int]
          )
        }
        Result(source, Some(problems.to[Seq]))
      case invalid =>
        deserializationError(s"Invalid problem: $invalid")
    }
  }
}

object Jshinter {

  /**
   * @param engine the JS engine actor properties.
   * @param shellSource the location of js that will process the arguments provided and output results.
   * @param jshintFile The jshint.js file.
   * @return a new linter.
   */
  def apply(engine: ActorRef, shellSource: File, jshintFile: File): Jshinter = {
    new Jshinter(engine, shellSource, jshintFile)
  }

  final case class Result(file: File, problems: Option[Seq[Problem]])
  final case class Problem(
    message: String,
    severity: Severity,
    lineContent: String,
    lineNumber: Int,
    characterOffset: Int
  )
  sealed trait Severity
  final case object Info extends Severity
  final case object Warn extends Severity
  final case object Error extends Severity

}

class JshinterFailure(m: String) extends RuntimeException(m)