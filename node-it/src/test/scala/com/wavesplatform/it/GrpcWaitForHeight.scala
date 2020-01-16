package com.wavesplatform.it

import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.it.api.AsyncGrpcApi._
import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.concurrent.{Await, Future}
import scala.concurrent.Future.traverse
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

trait GrpcWaitForHeight extends BeforeAndAfterAll with ScorexLogging with ReportingTestName with Nodes {
  this: Suite =>

  abstract protected override def beforeAll(): Unit = {
    super.beforeAll()
    Await.result(traverse(nodes)(_.waitForHeight(2)), 1.minute)
  }

  def waitForTxsToReachAllNodes(nodes: Seq[Node] = nodes, txIds: Seq[String]): Future[_] = {
    val txNodePairs = for {
      txId <- txIds
      node <- nodes
    } yield (node, txId)
    traverse(txNodePairs) { case (node, tx) => node.waitForTransaction(tx) }
  }

}
