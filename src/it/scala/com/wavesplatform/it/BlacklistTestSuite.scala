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


class BlacklistTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll {

  import BlacklistTestSuite._

  private val docker = new Docker()
  private val nodes = Configs.map(docker.startNode)
  private val richestNode = nodes.head
  private val otherNodes = nodes.tail

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

  "network should grow up to 100 blocks" in {
    Await.result(richestNode.waitForHeight(100), 5.minutes)

    Await.result(richestNode.height, 1.minute) >= 100 shouldBe true
  }

  "richest node should blacklist other nodes" in {
    otherNodes.foreach(n => Await.result(richestNode.blacklist(n), 1.minute))

    Await.result(
      richestNode.waitFor[Seq[String]](richestNode.blacklistedPeers, _.length >= NodesCount - 1, 5.seconds),
      5.minutes)

    val blacklisted = Await.result(richestNode.blacklistedPeers, 1.minute)
    blacklisted.length should be(NodesCount - 1)
  }

  "sleep" in {
    Thread.sleep(richestNode.settings.networkSettings.blackListResidenceTime.toMillis + 10.seconds.toMillis)
  }

  "and sync again" in {
    validateBlocks(nodes)
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