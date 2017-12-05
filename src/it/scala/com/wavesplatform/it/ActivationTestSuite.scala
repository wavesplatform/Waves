package com.wavesplatform.it

import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._

class ActivationTestSuite extends FreeSpec with Matchers with IntegrationNodesInitializationAndStopping {
  override lazy val nodes: Seq[Node] = docker.startNodes(NodeConfigs.Default.take(2))

  "api consuming example" in {
    Await.result(nodes.head.waitForHeight(5), 5.minute)
    Await.result(nodes.head.activationStatus, 5.minute).height shouldBe 5
  }
}
