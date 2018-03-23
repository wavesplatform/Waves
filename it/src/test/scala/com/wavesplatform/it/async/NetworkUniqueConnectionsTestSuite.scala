package com.wavesplatform.it.async

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.{DockerBased, NodeConfigs}
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.DurationInt

class NetworkUniqueConnectionsTestSuite extends FreeSpec with Matchers with DockerBased {

  import com.wavesplatform.it.async.NetworkUniqueConnectionsTestSuite._

  "nodes should up and connect with each other" in {
    val firstNode = docker.startNode(FirstNodeConfig)
    val prepare = for {
      _ <- firstNode.status
      secondNode = {
        // Helps to do an incoming connection: second -> first (1)
        val peersConfig = ConfigFactory.parseString(
          s"""waves.network.known-peers = [
             |  "${firstNode.containerNetworkAddress.getHostName}:${firstNode.containerNetworkAddress.getPort}"
             |]""".stripMargin
        )

        docker.startNode(peersConfig.withFallback(SecondNodeConfig), autoConnect = false)
      }
      _ <- firstNode.waitForPeers(1)

      // Outgoing connection: first -> second (2)
      _ <- firstNode.connect(secondNode.containerNetworkAddress)
    } yield ()

    Await.ready(prepare, 2.minute)
    withClue("Should fail with TimeoutException, because the connectionAttempt should fail") {
      intercept[TimeoutException] {
        Await.ready(firstNode.waitForPeers(2), 10.seconds)
      }
    }
  }

}

private object NetworkUniqueConnectionsTestSuite {

  private val configs          = NodeConfigs.newBuilder.withDefault(0).withSpecial(2, _.nonMiner).build()
  val FirstNodeConfig: Config  = configs.head
  val SecondNodeConfig: Config = configs.last

}
