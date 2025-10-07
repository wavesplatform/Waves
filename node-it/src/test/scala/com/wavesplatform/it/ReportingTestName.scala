package com.wavesplatform.it

import com.wavesplatform.api.http.DebugMessage
import com.wavesplatform.it.api.AsyncHttpApi.*
import com.wavesplatform.utils.{LoggerFacade, ScorexLogging}
import org.scalatest.{Args, Status, Suite, SuiteMixin}
import org.slf4j.LoggerFactory

import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

trait ReportingTestName extends SuiteMixin with ScorexLogging {
  th: Suite & Nodes =>
  override protected lazy val log = LoggerFacade(LoggerFactory.getLogger("Test"))

  abstract override protected def runTest(testName: String, args: Args): Status = {
    print(s"Test '$testName' started")
    val r = super.runTest(testName, args)
    print(s"Test '$testName' ${if (r.succeeds()) "SUCCEEDED" else "FAILED"}")
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
