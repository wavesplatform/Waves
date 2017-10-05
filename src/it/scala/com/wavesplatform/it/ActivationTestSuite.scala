package com.wavesplatform.it

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers}

import scala.concurrent.Await

class ActivationTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with IntegrationNodesInitializationAndStopping {
  override val docker = new Docker()
  override val nodes = Docker.NodeConfigs.getConfigList("nodes").asScala.take(2).map(docker.startNode)

  "matcher should respond with Public key" in {
    Await.result(nodes.head.waitForHeight(5), 5.minute)
    Await.result(nodes.head.activationStatus, 5.minute).height shouldBe 5
  }
}
