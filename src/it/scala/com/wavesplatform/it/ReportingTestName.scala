package com.wavesplatform.it

import org.scalatest.{Args, Status, Suite, SuiteMixin}
import scorex.utils.ScorexLogging
import scorex.waves.http.DebugMessage

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

trait ReportingTestName extends SuiteMixin with ScorexLogging {
  th: Suite =>
  def nodes: Seq[Node]

  abstract override protected def runTest(testName: String, args: Args): Status = {
    print(s"Test '$testName' started")
    val r = super.runTest(testName, args)
    print(s"Test `$testName` finished with $r")
    r
  }

  private def print(text: String): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val formatted = s"---------- $text ----------"
    log.debug(formatted)
    Await.result(Future.traverse(nodes)(_.printDebugMessage(DebugMessage(formatted))), 10.seconds)
  }
}
