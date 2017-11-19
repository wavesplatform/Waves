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

  "node should grow up to 10 blocks together and sync" in Await.result(
    Await.ready(waitForSameBlocksAt(nodes, 5.seconds, 5), 3.minutes),
    5.minutes
  )

  // Doing all work in one step, because nodes will not be available for requests and ReportingTestName fails here
  "then we disconnect nodes from the network, wait some time and connect them again" in {
    def maxHeight: Int = Await.result(traverse(nodes)(_.height).map(_.max), 12.seconds)

    val lastMaxHeight = maxHeight
    nodes.foreach(docker.disconnectFromNetwork)
    Thread.sleep(1.minute.toMillis) // ≈ 10 blocks, because a new block appears every 6 seconds
    nodes.foreach(docker.connectToNetwork)

    maxHeight shouldBe >=(lastMaxHeight + 6)
  }

  "nodes should sync" in Await.result(
    for {
      maxHeight <- traverse(nodes)(_.height).map(_.max)
      _ = log.debug(s"Max height is $maxHeight")
      _ <- waitForSameBlocksAt(nodes, 5.seconds, maxHeight + 5)
    } yield (),
    6.minutes
  )

}
