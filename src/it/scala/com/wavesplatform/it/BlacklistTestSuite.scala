package com.wavesplatform.it

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.NodeApi.BlacklistedPeer
import org.scalatest._

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class BlacklistTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure {

  import BlacklistTestSuite._

  private val docker = new Docker()
  private val nodes = Configs.map(docker.startNode)
  private val richestNode = nodes.head
  private val otherNodes = nodes.tail

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Await.result(Future.traverse(nodes)(_.waitForPeers(NodesCount - 1)), 2.minute)
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }

  "network should grow up to 30 blocks" in Await.result(richestNode.waitForHeight(30), 3.minutes)

  "richest node should blacklist other nodes" in {
    val test = for {
      _ <- traverse(otherNodes) { n => richestNode.blacklist(n.nodeInfo.networkIpAddress, n.nodeInfo.hostNetworkPort) }
      blacklisted <- richestNode.blacklistedPeers
    } yield {
      blacklisted.length should be(NodesCount - 1)
    }

    Await.result(test, 1.minute)
  }

  "sleep while nodes are blocked" in Await.result(
    richestNode.waitFor[Seq[BlacklistedPeer]](_.blacklistedPeers, _.isEmpty, 5.second),
    richestNode.settings.networkSettings.blackListResidenceTime + 5.seconds
  )

  "and sync again" in {
    val targetBlocks = Await.result(for {
      baseHeight <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(baseHeight + 30))
      blocks <- traverse(nodes)(_.blockAt(baseHeight + 25))
    } yield blocks.map(_.signature), 5.minutes)
    all(targetBlocks) shouldEqual targetBlocks.head
  }
}

object BlacklistTestSuite {

  private val generatingNodeConfig = ConfigFactory.parseString(
    """
      |waves.miner.offline = yes
    """.stripMargin)

  private val dockerConfigs = Docker.NodeConfigs.getConfigList("nodes").asScala

  val NodesCount: Int = 4

  val Configs: Seq[Config] = Seq(generatingNodeConfig.withFallback(dockerConfigs.last)) ++ Random.shuffle(dockerConfigs.init).take(NodesCount - 1)

}
