package com.wavesplatform.it.sync.network

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.{DockerBased, Node, NodeConfigs, Nodes}
import com.wavesplatform.utils.ScorexLogging
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent._
import scala.concurrent.duration._

class NetworkUniqueConnectionsTestSuite extends FreeSpec with Matchers with DockerBased with ScorexLogging with Nodes {
  import NetworkUniqueConnectionsTestSuite._

  "nodes should up and connect with each other" in {
    val firstNode = docker.startNode(FirstNodeConfig, autoConnect = false)
    nodes = Seq(firstNode)

    val status = firstNode.status()
    log.trace(s"#### $status")
    assert(status.blockchainHeight >= status.stateHeight)

    val secondNode = {
      // Helps to do an incoming connection: second -> first (1)
      val peersConfig = ConfigFactory.parseString(
        s"""waves.network.known-peers = [
             |  "${firstNode.containerNetworkAddress.getHostName}:${firstNode.containerNetworkAddress.getPort}"
             |]""".stripMargin
      )

      docker.startNode(peersConfig.withFallback(SecondNodeConfig), autoConnect = false)
    }
    nodes = Seq(firstNode, secondNode) // Thread dump workaround
    firstNode.waitForPeers(1)

    // Outgoing connection: first -> second (2)
    firstNode.connect(secondNode.containerNetworkAddress)

    withClue("Should fail with TimeoutException, because the connectionAttempt should fail") {
      intercept[TimeoutException] { firstNode.waitForPeers(2, 30.seconds) }
    }
  }

  protected var nodes: Seq[Node]         = Nil
  protected def nodeConfigs: Seq[Config] = NetworkUniqueConnectionsTestSuite.configs
}

object NetworkUniqueConnectionsTestSuite {
  private val configs          = NodeConfigs.newBuilder.withDefault(0).withSpecial(2, _.nonMiner).build()
  val FirstNodeConfig: Config  = configs.head
  val SecondNodeConfig: Config = configs.last
}
