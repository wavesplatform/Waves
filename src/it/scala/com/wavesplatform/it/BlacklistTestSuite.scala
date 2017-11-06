package com.wavesplatform.it

import com.wavesplatform.it.api.NodeApi.BlacklistedPeer
import org.scalatest._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class BlacklistTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with ReportingTestName {

  private val NodesCount: Int = 4
  private val docker = Docker(getClass)
  override val nodes: Seq[Node] = docker.startNodes(NodeConfigs.forTest(3, 1 -> "waves.miner.quorum = 0"))

  private val primaryNode: Node = nodes.last
  private val otherNodes = nodes.init

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Await.result(Future.traverse(nodes)(_.waitForPeers(NodesCount - 1)), 2.minute)
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }

  "network should grow up to 10 blocks" in Await.result(primaryNode.waitForHeight(10), 3.minutes)

  "primary node should blacklist other nodes" in Await.result(
    for {
      _ <- traverse(otherNodes) { n => primaryNode.blacklist(n.nodeInfo.networkIpAddress, n.nodeInfo.hostNetworkPort) }
      _ <- primaryNode.waitFor[Seq[BlacklistedPeer]](_.blacklistedPeers, _.size == NodesCount - 1, 1.second)
    } yield (),
    1.minute
  )

  "sleep while nodes are blocked" in Await.result(
    primaryNode.waitFor[Seq[BlacklistedPeer]](_.blacklistedPeers, _.isEmpty, 5.second),
    primaryNode.settings.networkSettings.blackListResidenceTime + 5.seconds
  )

  "and sync again" in {
    val targetBlocks = Await.result(for {
      baseHeight <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(baseHeight + 10))
      blocks <- traverse(nodes)(_.blockAt(baseHeight + 5))
    } yield blocks.map(_.signature), 5.minutes)
    all(targetBlocks) shouldEqual targetBlocks.head
  }
}
