package com.wavesplatform.it

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers}

import scala.collection.JavaConverters._
import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random


class NetworkSeparationTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll {

  import NetworkSeparationTestSuite._

  private val docker = new Docker()
  private val nodes = NodeConfigs.map(docker.startNode)

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    Await.result(Future.traverse(nodes)(_.status), 1.minute)

    Await.result(
      for {
        count <- Future.traverse(nodes)(_.waitForPeers(NodesCount - 1))
      } yield count, 1.minute
    )
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }

  private def validateBlocks(nodes: Seq[Node]): Unit = {
    val targetBlocks1 = result(for {
      height <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(height + 30))
      _ <- traverse(nodes)(_.waitForHeight(height + 25))
      blocks <- traverse(nodes)(_.blockAt(height + 25))
    } yield blocks.map(_.signature), 5.minutes)
    all(targetBlocks1) shouldEqual targetBlocks1.head
  }

  "node should grow up to 100 blocks together" in {
    val richestNode = nodes.maxBy(n => Await.result(n.balance(n.address), 1.minute).balance)
    Await.result(richestNode.waitForHeight(100), 5.minutes)
    Await.result(richestNode.height, 1.minute) >= 100 shouldBe true
  }

  "then we disconnect nodes from the network" in {
    nodes.foreach(docker.disconnectFromNetwork)
  }

  "and wait for another 20 blocks on one node" in {
    val richestNode = nodes.maxBy(n => Await.result(n.balance(n.address), 1.minute).balance)
    Await.result(richestNode.waitForHeight(120), 5.minutes)
    Await.result(richestNode.height, 1.minute) >= 120 shouldBe true
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