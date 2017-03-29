package com.wavesplatform.it

import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers, Suite}
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.collection.immutable.IndexedSeq
import scala.concurrent.{Await, Future}
import scala.util.Random
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class TestFiveNodesSuite extends FreeSpec with BeforeAndAfterAll with ScorexLogging with Matchers {
  private val docker = new Docker()
  private val nodeConfigs = Random.shuffle(Docker.NodeConfigs.getConfigList("nodes").asScala).take(5)
  private val allNodes = nodeConfigs.map(docker.startNode)

  override protected def beforeAll() = {
    log.debug("Waiting for nodes to start")
    Await.result(Future.traverse(allNodes)(_.status), 1.minute)

    log.debug("Waiting for nodes to connect")
    val peersCounts = Await.result(
      for {
        count <- Future.traverse(allNodes)(_.waitForPeers(4))
      } yield count, 1.minute
    )

    peersCounts.foreach(c => log.info(s"Connected peers: $c"))

    all(peersCounts) shouldEqual 4

    log.debug("Starting tests")
  }

  override val nestedSuites: IndexedSeq[Suite] = IndexedSeq(
    new ValidChainGenerationSpec(allNodes),
    new AliasTransactionSpec(allNodes),
    new BalancesSpecification(allNodes)
  )

  override protected def afterAll() = docker.close()
}
