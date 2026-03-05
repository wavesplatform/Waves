package com.wavesplatform.it

import com.typesafe.scalalogging.Logger
import com.wavesplatform.api.http.DebugMessage
import com.wavesplatform.it.ReportingTestName.CaptureCancel
import com.wavesplatform.it.api.AsyncHttpApi.*
import org.scalatest.events.*
import org.scalatest.*
import org.slf4j.LoggerFactory

import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

trait ReportingTestName extends SuiteMixin {
  th: Suite & Nodes =>
  private lazy val log = Logger(LoggerFactory.getLogger("Test"))

  abstract override protected def runTest(testName: String, args: Args): Status = {
    printTestWorkflow(s"Test '$testName' started")
    val reporter = new CaptureCancel(testName, args.reporter)
    val r        = super.runTest(testName, args.copy(reporter = reporter))
    printTestWorkflow(s"Test '$testName' ${reporter.status.toString.toUpperCase}")
    r
  }

  def step(text: String): Unit = {
    val formatted = s"---------- $text ----------"
    log.debug(formatted)
    printDebugMessage(formatted)
  }

  private def printTestWorkflow(text: String): Unit = {
    val formatted = s"========== $text =========="
    log.debug(formatted)
    printDebugMessage(formatted)
  }

  private def printDebugMessage(text: String): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    try {
      Await.result(Future.traverse(nodes)(_.printDebugMessage(DebugMessage(text))), 10.seconds)
    } catch {
      case _: Throwable => ()
    }
  }
}

object ReportingTestName {
  enum Status {
    case Unknown, Succeeded, Failed, Ignored, Canceled
  }
  class CaptureCancel(testName: String, inner: Reporter) extends Reporter {
    @volatile var status: Status = Status.Unknown
    override def apply(event: Event): Unit = {
      event match {
        case TestSucceeded(testName = `testName`) => status = Status.Succeeded
        case TestFailed(testName = `testName`)    => status = Status.Failed
        case TestIgnored(testName = `testName`)   => status = Status.Ignored
        case TestCanceled(testName = `testName`)  => status = Status.Canceled
        case _                                    =>
      }
      inner.apply(event)
    }
  }
}
