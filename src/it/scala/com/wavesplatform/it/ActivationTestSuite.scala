package com.wavesplatform.it

import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._

class ActivationTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with IntegrationNodesInitializationAndStopping {
  override val docker = new Docker()
  override val nodes = docker.startNodesSync(NodeConfigs.Default.take(2))

  "api consuming example" in {
    Await.result(nodes.head.waitForHeight(5), 5.minute)
    Await.result(nodes.head.activationStatus, 5.minute).height shouldBe 5
  }
}
