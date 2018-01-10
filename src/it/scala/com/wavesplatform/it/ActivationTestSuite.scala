package com.wavesplatform.it

import com.typesafe.config.Config
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._

class ActivationTestSuite extends FreeSpec with Matchers with IntegrationNodesInitializationAndStopping {

  override protected def nodeConfigs: Seq[Config] = NodeConfigs.newBuilder.withDefault(2).buildNonConflicting()

  "api consuming example" in {
    Await.result(nodes.head.waitForHeight(5), 5.minute)
    Await.result(nodes.head.activationStatus, 5.minute).height shouldBe 5
  }
}
