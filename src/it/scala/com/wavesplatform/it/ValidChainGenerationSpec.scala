package com.wavesplatform.it

import org.scalatest._
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class ValidChainGenerationSpec(docker: Docker) extends FreeSpec with Matchers with ScorexLogging {
  private val nodeConfigs = Random.shuffle(Docker.NodeConfigs.getConfigList("nodes").asScala).take(3)

  "Generate 30 blocks and synchronise" in {
    val allNodes = Await.result(Future.sequence(nodeConfigs.map(docker.startNode)), 30.seconds)

    log.info(allNodes
      .map(_.connectedPeers)
      .map(Await.result(_, Duration.Inf)).mkString("\n"))
  }
}
