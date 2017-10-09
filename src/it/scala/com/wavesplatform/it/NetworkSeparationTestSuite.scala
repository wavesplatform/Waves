package com.wavesplatform.it

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers}

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.util.Random

class NetworkSeparationTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with IntegrationNodesInitializationAndStopping {

  import NetworkSeparationTestSuite._

  override val docker = Docker(getClass)
  override val nodes = NodeConfigs.map(docker.startNode)

  private def validateBlocks(nodes: Seq[Node]): Unit = {
    val targetBlocks1 = result(for {
      height <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(height + 20))
      _ <- traverse(nodes)(_.waitForHeight(height + 15))
      blocks <- traverse(nodes)(_.blockAt(height + 15))
    } yield blocks.map(_.signature), 5.minutes)
    all(targetBlocks1) shouldEqual targetBlocks1.head
  }

  "node should grow up to 30 blocks together" in {
    val richestNode = nodes.maxBy(n => Await.result(n.balance(n.address), 1.minute).balance)
    Await.result(richestNode.waitForHeight(30), 5.minutes)
    Await.result(richestNode.height, 1.minute) >= 30 shouldBe true
  }

  "then we disconnect nodes from the network" in {
    nodes.foreach(docker.disconnectFromNetwork)
  }

  "and wait for another 10 blocks on one node" in {
    val richestNode = nodes.maxBy(n => Await.result(n.balance(n.address), 1.minute).balance)
    Await.result(richestNode.waitForHeight(40), 5.minutes)
    Await.result(richestNode.height, 1.minute) >= 40 shouldBe true
  }

  "after that we connect nodes back to the network" in {
    nodes.foreach(docker.connectToNetwork)
  }

  "nodes should sync" in {
    validateBlocks(nodes)
  }
}

object NetworkSeparationTestSuite {

  private val generatingNodeConfig = ConfigFactory.parseString(
    """
      |waves.miner.offline = yes
    """.stripMargin)

  private val configs = Docker.NodeConfigs.getConfigList("nodes").asScala

  val NodesCount: Int = 4

  val NodeConfigs: Seq[Config] = Random.shuffle(configs).take(NodesCount).map(generatingNodeConfig.withFallback(_))

}
