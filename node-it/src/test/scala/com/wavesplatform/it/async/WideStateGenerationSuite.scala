package com.wavesplatform.it.async

import java.util.concurrent.TimeoutException

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it._
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.util._

import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

@LoadTest
class WideStateGenerationSuite extends BaseFreeSpec with WaitForHeight2 with TransferSending {

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
        |  synchronization.utx-synchronizer {
        |    max-buffer-size = 500
        |    max-buffer-time = 100ms
        |    max-queue-size = 50000
        |  }
        |  utx.allow-skip-checks = false
        |  utx.max-scripted-size = 1000000
        |}""".stripMargin
    ),
    tag = getClass.getSimpleName,
    enableProfiling = true
  )

  override protected val nodeConfigs: Seq[Config] = NodeConfigs.newBuilder
    .overrideBase(_.quorum(3))
    .withDefault(2)
    .withSpecial(2, _.nonMiner)
    .buildNonConflicting()

  private val nodeAddresses = nodeConfigs.map(_.getString("address")).toSet
  private val requestsCount = 10000

  "Generate a lot of transactions and synchronise" in {
    val test = for {
      b <- dumpBalances()
      uploadedTxs <- processRequests(
        generateTransfersToRandomAddresses(requestsCount / 2, nodeAddresses) ++
          generateTransfersBetweenAccounts(requestsCount / 2, b)
      )

      _ <- Await.ready(traverse(nodes)(_.waitFor[Int]("UTX is empty")(_.utxSize, _ == 0, 5.seconds)), 7.minutes)

      height <- traverse(nodes)(_.height).map(_.max)
      _      <- Await.ready(nodes.waitForSameBlockHeadersAt(height + 1), 5.minutes)

      _ <- Await.ready(traverse(nodes)(assertHasTxs(_, uploadedTxs.map(_.id).toSet)), 5.minutes)
    } yield ()

    val limit = GlobalTimer.instance.schedule(Future.failed(new TimeoutException("Time is out for test")), 18.minutes)
    val testWithDumps = Future.firstCompletedOf(Seq(test, limit)).recoverWith { case e =>
      for {
        _     <- dumpBalances()
        dumps <- traverse(nodes)(dumpBlockChain)
      } yield {
        log.debug(dumps.mkString("Dumps:\n", "\n\n", "\n"))
        throw e
      }
    }

    Await.result(testWithDumps, 18.minutes)
  }

  private def assertHasTxs(node: Node, txIds: Set[String]): Future[Unit] = {
    for {
      height <- node.height
      blocks <- node.blockSeq(1, height)
    } yield {
      val txsInBlockchain = blocks.flatMap(_.transactions.map(_.id))
      val snapshot        = txIds -- txsInBlockchain
      withClue(s"all transactions in node") {
        snapshot shouldBe empty
      }
    }
  }

  private def dumpBalances(): Future[Map[Config, Long]] = {
    traverse(nodeConfigs) { config =>
      nodes.head.balance(config.getString("address")).map(x => (config, x.balance))
    }.map(_.toMap)
      .map { r =>
        log.debug(s"""Balances:
                     |${r.map { case (config, balance) => s"${config.getString("address")} -> $balance" }.mkString("\n")}""".stripMargin)
        r
      }
  }

  private def dumpBlockChain(node: Node): Future[String] = {
    val maxRequestSize = 100

    for {
      utxSize <- node.utxSize
      height  <- node.height
      blockGroups <- traverse((2 to height).grouped(maxRequestSize).map { xs =>
        (xs.head, xs.last, false)
      })(Function.tupled(node.blockSeq))
    } yield {
      val blocks = blockGroups.flatten.toList
      val blocksInfo = blocks.zipWithIndex
        .map { case (x, i) => s"$i: id=${x.id.trim}, txsSize=${x.transactions.size}, txs=${x.transactions.map(_.id.trim).mkString(", ")}" }

      s"""Dump of ${node.name}:
         |UTX size: $utxSize
         |Total txs: ${blocks.map(_.transactions.size).sum}
         |Blocks:
         |${blocksInfo.mkString("\n")}""".stripMargin
    }
  }

}
