package com.wavesplatform.it

import com.wavesplatform.it.api.MultipleNodesApi
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class NetworkSeparationTestSuite extends FreeSpec with Matchers with IntegrationNodesInitializationAndStopping
  with CancelAfterFailure with ReportingTestName with MultipleNodesApi {

  override lazy val nodes: Seq[Node] = docker.startNodes(
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(3))
      .withDefault(3)
      .withSpecial(_.quorum(0))
      .build
  )

  "node should grow up to 10 blocks together" in Await.result(richestNode.flatMap(_.waitForHeight(10)), 5.minutes)

  "then we disconnect nodes from the network" in nodes.foreach(docker.disconnectFromNetwork)

  "and wait for another 10 blocks on one node" in Await.result(richestNode.flatMap(_.waitForHeight(20)), 5.minutes)

  "after that we connect nodes back to the network" in nodes.foreach(docker.connectToNetwork)

  "nodes should sync" in Await.result(
    for {
      height <- traverse(nodes)(_.height).map(_.max)
      _ <- waitForSameBlocksAt(nodes, 5.seconds, height + 8)
    } yield (),
    5.minutes
  )

  def richestNode: Future[Node] = traverse(nodes)(n => n.balance(n.address).map(r => n -> r.balance)).map { xs =>
    xs
      .maxBy { case (_, balance) => balance}
      ._1
  }

}
