package com.wavesplatform.it

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.NetworkUniqueConnectionsTestSuite._
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.DurationInt

class NetworkUniqueConnectionsTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll {

  private lazy val docker = Docker(getClass)

  "nodes should up and connect with each other" in {
    val firstNode = docker.startNode(FirstNodeConfig)
    val prepare = for {
      _ <- firstNode.status
      secondNode = {
        // Helps to do an incoming connection: second -> first (1)
        val peersConfig = ConfigFactory.parseString(
          s"""waves.network.known-peers = [
             |  "${firstNode.nodeInfo.networkIpAddress}:${firstNode.nodeInfo.containerNetworkPort}"
             |]""".stripMargin
        )

        docker.startNode(peersConfig.withFallback(SecondNodeConfig), autoConnect = false)
      }
      _ <- firstNode.waitForPeers(1)

      // Outgoing connection: first -> second (2)
      connectionAttempt <- firstNode.connect(
        secondNode.nodeInfo.networkIpAddress,
        secondNode.nodeInfo.containerNetworkPort
      )
    } yield ()

    Await.ready(prepare, 2.minute)
    withClue("Should fail with TimeoutException, because the connectionAttempt should fail") {
      intercept[TimeoutException] {
        Await.ready(firstNode.waitForPeers(2), 10.seconds)
      }
    }
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }

}

private object NetworkUniqueConnectionsTestSuite {

  private val configs = NodeConfigs.newBuilder.withDefault(0).withSpecial(2, _.nonMiner).build()
  val FirstNodeConfig: Config = configs.head
  val SecondNodeConfig: Config = configs.last

}
