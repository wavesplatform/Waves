package com.wavesplatform.it

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api._
import org.scalatest._

import scala.concurrent.Await
import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class WideStateGenerationSuite extends FreeSpec with IntegrationNodesInitializationAndStopping
  with Matchers with TransferSending with MultipleNodesApi {

  override protected def createDocker: Docker = new Docker(
    suiteConfig = ConfigFactory.parseString(
      """akka.http.server.parsing.max-content-length = 3737439
        |waves {
        |  network.traffic-logger {
        |    ignore-tx-messages = [1, 2, 23, 25]
        |    ignore-rx-messages = [1, 2, 25]
        |  }
        |  miner.minimal-block-generation-offset = 10s
        |}""".stripMargin
    ),
    tag = getClass.getSimpleName
  )

  override protected def nodeConfigs: Seq[Config] = NodeConfigs.newBuilder
    .overrideBase(_.quorum(3))
    .withDefault(3)
    .withSpecial(_.nonMiner)
    .buildNonConflicting()

  private val requestsCount = 10000

  "Generate a lot of transactions and synchronise" in result(for {
    b <- traverse(nodes)(balanceForNode).map(_.toMap)
    lastTx <- {
      log.debug(
        s"""Balances:
           |${b.map { case (account, balance) => s"$account -> $balance" }.mkString("\n")}""".stripMargin)

      processRequests(generateTransfersToRandomAddresses(requestsCount / 2, b) ++ generateTransfersBetweenAccounts(requestsCount / 2, b))
    }

    _ <- {
      log.debug(s"Wait a transaction ${lastTx.get.id} is in blockchain")
      Await.ready(traverse(nodes)(_.waitForTransaction(lastTx.get.id, retryInterval = 10.seconds)), 9.minutes)
    }

    height <- traverse(nodes)(_.height).map(_.max)

    _ <- {
      log.debug(s"waitForSameBlocksAt($height)")
      waitForSameBlocksAt(nodes, 5.seconds, height)
    }
  } yield (), 15.minutes)

}
