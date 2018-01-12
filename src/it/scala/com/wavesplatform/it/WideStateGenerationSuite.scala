package com.wavesplatform.it

import java.util.concurrent.TimeoutException

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api._
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import org.scalatest._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class WideStateGenerationSuite extends FreeSpec with IntegrationNodesInitializationAndStopping
  with Matchers with TransferSending with NodesFromDocker {

  override protected def createDocker: Docker = new Docker(
    suiteConfig = ConfigFactory.parseString(
      """akka.http.server {
        |  parsing.max-content-length = 3737439
        |  request-timeout = 60s
        |}
        |
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

  "Generate a lot of transactions and synchronise" in {
    val test = for {
      b <- dumpBalances()
      lastTx <- processRequests(generateTransfersToRandomAddresses(requestsCount / 2, b) ++ generateTransfersBetweenAccounts(requestsCount / 2, b))

      _ <- {
        log.debug(s"Wait a transaction ${lastTx.get.id} is in blockchain")
        Await.ready(traverse(nodes)(_.waitForTransaction(lastTx.get.id, retryInterval = 10.seconds)), 5.minutes)
      }

      height <- traverse(nodes)(_.height).map(_.max)

      _ <- {
        log.debug(s"waitForSameBlocksAt($height)")
        Await.ready(AsyncHttpApi.waitForSameBlocksAt(nodes, 5.seconds, height), 5.minutes)
      }
    } yield ()

    val limit = GlobalTimer.instance.schedule(Future.failed(new TimeoutException("Time is out for test")), 15.minutes)
    val testWithDumps = Future.firstCompletedOf(Seq(test, limit)).recoverWith {
      case e =>
        for {
          _ <- dumpBalances()
          dumps <- traverse(nodes)(dumpBlockChain)
        } yield {
          log.debug(dumps.mkString("Dumps:\n", "\n\n", "\n"))
          throw e
        }
    }

    Await.result(testWithDumps, 16.minutes)
  }

  private def dumpBalances(): Future[Map[String, Long]] = traverse(nodes)(balanceForNode).map(_.toMap).map { r =>
    log.debug(
      s"""Balances:
         |${r.map { case (account, balance) => s"$account -> $balance" }.mkString("\n")}""".stripMargin)
    r
  }

  private def dumpBlockChain(node: Node): Future[String] = {
    val maxRequestSize = 100

    for {
      utxSize <- node.utxSize
      height <- node.height
      blockGroups <- traverse((2 to height).grouped(maxRequestSize).map { xs => (xs.head, xs.last) })(Function.tupled(node.blockSeq))
    } yield {
      val blocks = blockGroups.flatten.toList
      val blocksInfo = blocks
        .zipWithIndex
        .map { case (x, i) => s"$i: id=${x.signature.trim}, txsSize=${x.transactions.size}, txs=${x.transactions.map(_.id.trim).mkString(", ")}" }

      s"""Dum of ${node.settings.networkSettings.nodeName}:
         |UTX size: $utxSize
         |Total txs: ${blocks.map(_.transactions.size).sum}
         |Blocks:
         |${blocksInfo.mkString("\n")}""".stripMargin
    }
  }

}
