package com.wavesplatform.it

import com.wavesplatform.it.api.AsyncHttpApi._
import org.scalatest._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Future.traverse

trait WaitForHeight2 extends ReportingTestName with Nodes {
  this: Suite =>

  def waitForTxsToReachAllNodes(nodes: Seq[Node] = nodes, txIds: Seq[String]): Future[_] = {
    val txNodePairs = for {
      txId <- txIds
      node <- nodes
    } yield (node, txId)
    traverse(txNodePairs) { case (node, tx) => node.waitForTransaction(tx) }
  }

}
