package com.wavesplatform.it

import com.wavesplatform.it.TransferSending.Req
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}

import scala.collection.mutable
import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.collection.JavaConverters._

class RollbackSpecSuite extends FreeSpec with ScalaFutures with IntegrationPatience
  with Matchers with TransferSending with IntegrationNodesInitializationAndStopping {
  override val docker = new Docker()
  private val allNodes = Docker.NodeConfigs.getConfigList("nodes").asScala
  // there are nodes with big and small balances to reduce the number of forks
  override val nodes = Seq(allNodes.head, allNodes.last).map(docker.startNode)

  def rollbackTest(rollbackAndReturnTransactionsToUTX: (Int, Seq[Req]) => Future[_]): Unit = {
    val waitBlocks = 5
    result(for {
      b <- traverse(nodes)(balanceForNode).map(_.toMap)
      requests = generateRequests(301, b)
      startHeight <- Future.traverse(nodes)(_.height).map(_.min)
      _ <- processRequests(requests)
      hashAfterFirstTry <- traverse(nodes)(_.waitForDebugInfoAt(startHeight + waitBlocks).map(_.stateHash)).map(infos => {
        all(infos) shouldEqual infos.head
        infos.head
      })
      stateAfterFirstTry <- nodes.head.debugStateAt(startHeight + waitBlocks)
      _ <- rollbackAndReturnTransactionsToUTX(startHeight, requests)
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

  "Apply the same transfer transactions twice with return to UTX" in {
    rollbackTest((startHeight, _) => traverse(nodes)(_.rollback(startHeight)))
  }

  "Apply the same transfer transactions twice without return to UTX" in {
    rollbackTest((startHeight, requests) =>
      for {
      _ <- traverse(nodes)(_.rollback(startHeight, returnToUTX = false))
      _ <- processRequests(requests)
      } yield ()
    )
  }

  "Just rollback transactions" in {
    val waitBlocks = 5
    result(for {
      b <- traverse(nodes)(balanceForNode).map(_.toMap)
      requests = generateRequests(301, b)
      startHeight <- Future.traverse(nodes)(_.height).map(_.min)
      hashBeforeApply <- traverse(nodes)(_.waitForDebugInfoAt(startHeight + waitBlocks).map(_.stateHash)).map(infos => {
        all(infos) shouldEqual infos.head
        infos.head
      })
      _ <- processRequests(requests)
      _ <- traverse(nodes)(n => n.waitFor[Int](n.utxSize, _ == 0, 1.second))
      _ <- traverse(nodes)(_.rollback(startHeight, returnToUTX = false))
      _ <- traverse(nodes)(_.utx).map(utxs => {
        all(utxs) shouldBe 'empty
      })
      hashAfterApply <- nodes.head.waitForDebugInfoAt(startHeight + waitBlocks).map(_.stateHash)
    } yield {
      hashBeforeApply shouldBe hashAfterApply
    }, 5.minutes)
  }
}
