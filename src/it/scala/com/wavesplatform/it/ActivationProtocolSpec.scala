package com.wavesplatform.it

import com.wavesplatform.it.NetworkSeparationTestSuite.NodeConfigs
import org.scalatest.{FreeSpec, Matchers}

class ActivationProtocolSpec extends FreeSpec with Matchers with IntegrationNodesInitializationAndStopping  {
  override def docker = new Docker()
  override def nodes = NodeConfigs.map(docker.startNode)

  "ddddy" in {
    val a = 1


    assert(false)
  }
}
