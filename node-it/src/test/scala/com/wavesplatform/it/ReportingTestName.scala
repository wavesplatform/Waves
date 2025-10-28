package com.wavesplatform.it

import com.wavesplatform.api.http.DebugMessage
import com.wavesplatform.it.ReportingTestName.CaptureCancel
import com.wavesplatform.it.api.AsyncHttpApi.*
import com.wavesplatform.utils.ScorexLogging
import org.scalatest.events.*
import org.scalatest.*

import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

trait ReportingTestName extends SuiteMixin with ScorexLogging {
  th: Suite & Nodes =>

  abstract override protected def runTest(testName: String, args: Args): Status = {
    print(s"Test '$testName' started")
    val reporter = new CaptureCancel(testName, args.reporter)
    val r        = super.runTest(testName, args.copy(reporter = reporter))
    print(s"Test '$testName' ${reporter.status.toString.toUpperCase}")
    r
  }

  private def print(text: String): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val formatted = s"---------- $text ----------"
    log.debug(formatted)
    try {
      Await.result(Future.traverse(nodes)(_.printDebugMessage(DebugMessage(formatted))), 10.seconds)
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
