package com.wavesplatform.it

import org.scalatest.{BeforeAndAfterAll, FreeSpec, Suite}
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.collection.immutable.IndexedSeq
import scala.concurrent.{Await, Future}
import scala.util.Random
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class TestFiveNodesSuite extends FreeSpec with BeforeAndAfterAll with ScorexLogging {
  private val docker = new Docker()
  private val nodeConfigs = Random.shuffle(Docker.NodeConfigs.getConfigList("nodes").asScala).take(5)
  private val allNodes = nodeConfigs.map(docker.startNode)

  override protected def beforeAll() = {
    log.debug("Waiting for nodes to start")
    Await.result(Future.traverse(allNodes)(_.status), 30.seconds)
    log.debug("Starting tests")
  }

  override def nestedSuites: IndexedSeq[Suite] = IndexedSeq(
    new ValidChainGenerationSpec(allNodes),
    new AliasTransactionSpec(allNodes)
  )

  override protected def afterAll() = docker.close()
}
