package com.wavesplatform.it

import com.wavesplatform.it.transactions._
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers, Suite}
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.collection.immutable.IndexedSeq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class TestFiveNodesSuite extends FreeSpec with BeforeAndAfterAll with ScorexLogging with Matchers {
  private val docker = new Docker()
  private val nodesCount = 4
  private val nodeConfigs = Random.shuffle(Docker.NodeConfigs.getConfigList("nodes").asScala).take(nodesCount)
  private val allNodes = nodeConfigs.map(docker.startNode)

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
    new BurnTransactionSpecification(allNodes),
    new IssueTransactionSpecification(allNodes),
    new LeasingTransactionsSpecification(allNodes),
    new MakeAssetNameUniqueTransactionSpecification(allNodes),
    new PaymentTransactionSpecification(allNodes),
    new ReissueTransactionSpecification(allNodes),
    new TransferTransactionSpecification(allNodes),
    new AliasTransactionSpecification(allNodes)
  )

  override protected def afterAll(): Unit = docker.close()
}
