package com.wavesplatform.it

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.RollbackSpecSuite._
import com.wavesplatform.it.util._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class RollbackSpecSuite extends FreeSpec with ScalaFutures with IntegrationPatience
  with Matchers with TransferSending with IntegrationNodesInitializationAndStopping {
  // there are nodes with big and small balances to reduce the number of forks
  override lazy val nodes: Seq[Node] = docker.startNodes(configs)

  private val transactionsCount = 190

  "Apply the same transfer transactions twice with return to UTX" in {
    val waitBlocks = 10
    result(for {
      startHeight <- Future.traverse(nodes)(_.height).map(_.min)

      b <- traverse(nodes)(balanceForNode).map(_.toMap)

      requests = generateTransfersBetweenAccounts(transactionsCount, b)
      _ <- processRequests(requests)

      hashAfterFirstTry <- traverse(nodes)(_.waitForDebugInfoAt(startHeight + waitBlocks).map(_.stateHash)).map(infos => {
        all(infos) shouldEqual infos.head
        infos.head
      })
      stateAfterFirstTry <- nodes.head.debugStateAt(startHeight + waitBlocks)

      _ <- nodes.tail.head.rollback(1)
      _ <- nodes.head.rollback(startHeight)

      hashAfterSecondTry <- traverse(nodes)(_.waitForDebugInfoAt(startHeight + waitBlocks).map(_.stateHash)).map(infos => {
        all(infos) shouldEqual infos.head
        infos.head
      })
      stateAfterSecondTry <- nodes.head.debugStateAt(startHeight + waitBlocks)
    } yield {
      stateAfterFirstTry should contain theSameElementsAs stateAfterSecondTry
      hashAfterFirstTry shouldBe hashAfterSecondTry
    }, 5.minutes)
  }

  "Just rollback transactions" in {
    val waitBlocks = 8
    result(for {
      startHeight <- Future.traverse(nodes)(_.height).map(_.min)

      b <- traverse(nodes)(balanceForNode).map(_.toMap)
      requests = generateTransfersBetweenAccounts(transactionsCount, b)

      hashBeforeApply <- traverse(nodes)(_.waitForDebugInfoAt(startHeight + waitBlocks).map(_.stateHash)).map(infos => {
        all(infos) shouldEqual infos.head
        infos.head
      })
      _ <- processRequests(requests)
      _ <- nodes.head.waitFor[Int](_.utxSize, _ == 0, 1.second)
      _ <- traverse(nodes)(_.rollback(startHeight, returnToUTX = false))
      _ <- nodes.head.utx.map( _ shouldBe 'empty )

      hashAfterApply <- nodes.head.waitForDebugInfoAt(startHeight + waitBlocks).map(_.stateHash)
    } yield {
      hashBeforeApply shouldBe hashAfterApply
    }, 5.minutes)
  }

  "Alias transaction rollback should work fine" in {
    val alias = "test_alias4"

    val f = for {
      startHeight <- Future.traverse(nodes)(_.height).map(_.max)
      aliasTxId <- nodes.head.createAlias(nodes.head.address, alias, 1.waves).map(_.id)
      _ <- Future.traverse(nodes)(_.waitForTransaction(aliasTxId))
      _ <- Future.traverse(nodes)(_.waitForHeight(startHeight + 1))
      _ <- Future.traverse(nodes)(_.rollback(startHeight - 1, returnToUTX = false))
      _ <- Future.traverse(nodes)(_.waitForHeight(startHeight + 1))
      secondAliasTxId <- nodes.head.createAlias(nodes.head.address, alias, 1.waves).map(_.id)
      _ <- Future.traverse(nodes)(_.waitForTransaction(secondAliasTxId))
    } yield succeed

    Await.result(f, 1.minute)
  }
}

object RollbackSpecSuite {
  import NodeConfigs.Default

  private val nonGeneratingNodesConfig = ConfigFactory.parseString("waves.miner.enable = no")

  val configs: Seq[Config] = Seq(Default.last, nonGeneratingNodesConfig.withFallback(Random.shuffle(Default.init).head))
}
