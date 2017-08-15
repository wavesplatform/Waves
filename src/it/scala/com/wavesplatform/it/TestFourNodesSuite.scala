package com.wavesplatform.it

import com.typesafe.config.ConfigFactory
import com.wavesplatform.it.transactions._
import com.wavesplatform.it.transactions.debug._
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers, Suite}
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.collection.immutable.IndexedSeq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class TestFourNodesSuite extends FreeSpec with BeforeAndAfterAll with ScorexLogging with Matchers {

  private val nonGeneratingPeerConfig = ConfigFactory.parseString(
    """
      |waves.miner.enable=no
    """.stripMargin
  )

  private val docker = new Docker()
  private val nodesCount = 4
  private val dockerConfigs = Random.shuffle(Docker.NodeConfigs.getConfigList("nodes").asScala).take(nodesCount)
  private val nodeConfigs = Seq(nonGeneratingPeerConfig.withFallback(dockerConfigs.head)) ++ dockerConfigs.tail


  private val allNodes = nodeConfigs.map(docker.startNode)
  private val notMiner = allNodes.head

  override protected def beforeAll(): Unit = {
    log.debug("Waiting for nodes to start")
    Await.result(Future.traverse(allNodes)(_.status), 1.minute)

    log.debug("Waiting for nodes to connect")
    val peersCounts = Await.result(
      for {
        count <- Future.traverse(allNodes)(_.waitForPeers(nodesCount - 1))
      } yield count, 1.minute
    )

    peersCounts.foreach(c => log.info(s"Connected peers: $c"))

    all(peersCounts.map(_.length)) shouldEqual nodesCount - 1

    log.debug("Starting tests")
  }

  override def nestedSuites: IndexedSeq[Suite] = IndexedSeq(
    new ValidChainGenerationSpec(allNodes),
    new BurnTransactionSpecification(allNodes, notMiner),
    new IssueTransactionSpecification(allNodes, notMiner),
    new LeasingTransactionsSpecification(allNodes, notMiner),
    new PaymentTransactionSpecification(allNodes, notMiner),
    new ReissueTransactionSpecification(allNodes, notMiner),
    new TransferTransactionSpecification(allNodes, notMiner),
    new AliasTransactionSpecification(allNodes, notMiner),
    new DebugPortfoliosSpecification(allNodes, notMiner)
  )

  override protected def afterAll(): Unit = docker.close()
}
