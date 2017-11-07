package com.wavesplatform.it

import com.typesafe.config.ConfigFactory
import com.wavesplatform.it.transactions._
import com.wavesplatform.it.transactions.debug._
import org.scalatest._
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.collection.immutable.IndexedSeq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class TestFourNodesSuite extends FreeSpec with BeforeAndAfterAll with ScorexLogging with Matchers with ReportingTestName{

  private val nonGeneratingPeerConfig = ConfigFactory.parseString(
    """
      |waves.miner.enable=no
    """.stripMargin
  )

  private val docker = Docker(getClass)
  private val nodesCount = 4
  private val dockerConfigs = Random.shuffle(Docker.NodeConfigs.getConfigList("nodes").asScala).take(nodesCount)
  private val nodeConfigs = Seq(nonGeneratingPeerConfig.withFallback(dockerConfigs.head)) ++ dockerConfigs.tail


  override val nodes: Seq[Node] = nodeConfigs.map(docker.startNode)
  private val notMiner = nodes.head

  override protected def beforeAll(): Unit = {
    log.debug("Waiting for nodes to start")
    Await.result(Future.traverse(nodes)(_.status), 1.minute)

    log.debug("Waiting for nodes to connect")
    val peersCounts = Await.result(
      for {
        count <- Future.traverse(nodes)(_.waitForPeers(nodesCount - 1))
      } yield count, 1.minute
    )

    peersCounts.foreach(c => log.info(s"Connected peers: $c"))

    all(peersCounts.map(_.length)) shouldEqual nodesCount - 1

    log.debug("Starting tests")
  }

  override def nestedSuites: IndexedSeq[Suite] = IndexedSeq(
    new WideStateGenerationSpec(nodes),
    new ValidChainGenerationSpec(nodes),
    new BurnTransactionSpecification(nodes, notMiner),
    new IssueTransactionSpecification(nodes, notMiner),
    new LeasingTransactionsSpecification(nodes, notMiner),
    new PaymentTransactionSpecification(nodes, notMiner),
    new ReissueTransactionSpecification(nodes, notMiner),
    new TransferTransactionSpecification(nodes, notMiner),
    new AliasTransactionSpecification(nodes, notMiner),
    new DebugPortfoliosSpecification(nodes, notMiner),
    new BlockHeadersSpecification(nodes, notMiner)
  )
  override protected def afterAll(): Unit = docker.close()
}
