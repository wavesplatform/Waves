package com.wavesplatform.it

import org.scalatest.{Args, Status, Suite, SuiteMixin}
import scorex.waves.http.DebugMessage

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._


trait ReportingTestName extends SuiteMixin {
  th: Suite =>
  def nodes: Seq[Node]

  abstract override def runTest(testName: String, args: Args): Status = {
    import scala.concurrent.ExecutionContext.Implicits.global

    Await.result(Future.traverse(nodes)(_.printDebugMessage(DebugMessage(s"---------- Test '$testName' started ----------"))), 10.seconds)
    val r = super.runTest(testName, args)
    Await.result(Future.traverse(nodes)(_.printDebugMessage(DebugMessage(s"---------- Test `$testName` finished ----------"))), 10.seconds)
    r
  }
}
