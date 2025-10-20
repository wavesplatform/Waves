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
    printTestWorkflow(s"Test '$testName' started")
    val r = super.runTest(testName, args)
    printTestWorkflow(s"Test '$testName' ${if (r.succeeds()) "SUCCEEDED" else "FAILED"}")
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
