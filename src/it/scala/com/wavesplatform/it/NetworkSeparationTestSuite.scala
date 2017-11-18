package com.wavesplatform.it

import com.wavesplatform.it.api.MultipleNodesApi
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.Await
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

  private val primaryMiner = nodes.last

  "node should grow up to 10 blocks together and sync" in Await.result(
    for {
      maxHeight <- traverse(nodes)(_.height).map(_.max)
      _ <- Await.ready(waitForSameBlocksAt(nodes, 5.seconds, maxHeight), 3.minutes)
    } yield (),
    5.minutes
  )

  "then we disconnect nodes from the network" in nodes.foreach(docker.disconnectFromNetwork)

  "and wait for another 10 blocks on one node" in Await.result(
    for {
      height <- primaryMiner.height
      _ <- primaryMiner.waitForHeight(height + 10)
    } yield (),
    3.minutes
  )

  "after that we connect nodes back to the network" in nodes.foreach(docker.connectToNetwork)

  "nodes should sync" in Await.result(
    for {
      maxHeight <- traverse(nodes)(_.height).map(_.max)
      _ <- waitForSameBlocksAt(nodes, 5.seconds, maxHeight + 5)
    } yield (),
    6.minutes
  )

}
