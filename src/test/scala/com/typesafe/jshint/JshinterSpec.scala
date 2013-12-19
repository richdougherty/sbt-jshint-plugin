package com.typesafe.jshint

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mutable.Specification
import akka.util.Timeout
import scala.concurrent.duration._
import org.specs2.time.NoTimeConversions
import spray.json.{JsBoolean, JsArray, JsObject}
import java.io.File
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Success
import scala.collection.immutable.Seq
import com.typesafe.jse.Rhino
import akka.actor.ActorSystem

@RunWith(classOf[JUnitRunner])
class JshinterSpec extends Specification with NoTimeConversions {

  import ExecutionContext.Implicits.global

  def jshinterTester(implicit system: ActorSystem): Jshinter = {
    val shellSource = new File(this.getClass.getClassLoader.getResource("shell.js").toURI)
    val jshintSource = new File(this.getClass.getClassLoader.getResource("jshint.js").toURI)
    val engine = system.actorOf(Rhino.props(), "engine")
    Jshinter(engine, shellSource, jshintSource)
  }

  implicit val timeout = Timeout(5.seconds)

  sequential

  "the jshinter" should {
    "receive source with no options and find an error" in new TestActorSystem {
      val fileToLint = new File(this.getClass.getResource("some.js").toURI)
      val filesToLint = Seq(fileToLint)
      val options = JsObject()

      val futureResult: Future[Seq[Jshinter.Result]] = jshinterTester.lint(filesToLint, options)

      val result = Await.result(futureResult, timeout.duration)

      result.to[Vector] must_== Vector(
        Jshinter.Result(fileToLint, Some(Vector(Jshinter.Problem(
          message = "Missing semicolon.",
          severity = Jshinter.Error,
          lineContent = "var a = 1",
          lineNumber = 1,
          characterOffset = 10
        ))))
      )
    }

    "receive source and options and not find an error" in new TestActorSystem {
      val fileToLint = new File(this.getClass.getResource("some.js").toURI)
      val filesToLint = Seq(fileToLint)
      val options = JsObject("asi" -> JsBoolean(true))

      val futureResult: Future[Seq[Jshinter.Result]] = jshinterTester.lint(filesToLint, options)

      val result = Await.result(futureResult, timeout.duration)

      result.to[Vector] must_== Vector(Jshinter.Result(fileToLint, None))
    }
  }
}
