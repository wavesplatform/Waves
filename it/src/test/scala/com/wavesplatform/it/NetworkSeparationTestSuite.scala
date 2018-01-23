package com.wavesplatform.it

import com.typesafe.config.Config
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class NetworkSeparationTestSuite extends FreeSpec with Matchers with WaitForHeight2
  with CancelAfterFailure with ReportingTestName with NodesFromDocker {

  override protected def nodeConfigs: Seq[Config] = NodeConfigs.newBuilder
    .overrideBase(_.quorum(3))
    .withDefault(3)
    .withSpecial(_.quorum(0))
    .buildNonConflicting()

  "node should grow up to 10 blocks together and sync" in Await.result(
    Await.ready(nodes.waitForSameBlocksAt(5.seconds, 10), 3.minutes),
    5.minutes
  )

  // Doing all work in one step, because nodes will not be available for requests and ReportingTestName fails here
  "then we disconnect nodes from the network, wait some time and connect them again" in {
    def maxHeight: Int = Await.result(traverse(nodes)(_.height).map(_.max), 12.seconds)

    val lastMaxHeight = maxHeight
    dockerNodes().foreach(docker.disconnectFromNetwork)
    Thread.sleep(80.seconds.toMillis) // >= 10 blocks, because a new block appears every 6 seconds
    docker.connectToNetwork(dockerNodes())

    maxHeight shouldBe >=(lastMaxHeight + 6)
  }

  "nodes should sync" in Await.result(
    for {
      maxHeight <- traverse(nodes)(_.height).map(_.max)
      _ = log.debug(s"Max height is $maxHeight")
      _ <- nodes.waitForSameBlocksAt(5.seconds, maxHeight + 5)
    } yield (),
    6.minutes
  )

}
