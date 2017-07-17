package com.wavesplatform.it

import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}

import scala.collection.mutable
import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.collection.JavaConverters._

class RollbackSpec extends FreeSpec with ScalaFutures with IntegrationPatience
  with Matchers with TransferSending with IntegrationNodesInitializationAndStopping {
  override val docker = new Docker()
  private val allNodes = Docker.NodeConfigs.getConfigList("nodes").asScala
  // there are nodes with big and small balances to reduce the number of forks
  override val nodes = Seq(allNodes.head, allNodes.last).map(docker.startNode)

  "Apply the same transfer transactions twice with return to UTX" in {
    val waitBlocks = 5
    result(for {
      b <- traverse(nodes)(balanceForNode).map(mutable.AnyRefMap[String, Long](_: _*))
      requests = generateRequests(301, b)
      startHeight <- Future.traverse(nodes)(_.height).map(_.min)
      _ <- processRequests(requests)
      hashAfterFirstTry <- traverse(nodes)(_.waitForDebugInfoAt(startHeight + waitBlocks).map(_.stateHash)).map(infos => {
        all(infos) shouldEqual infos.head
        infos.head
      })
      stateAfterFirstTry <- nodes.head.debugStateAt(startHeight + waitBlocks)
      _ <- traverse(nodes)(_.rollback(startHeight))
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

  "Apply the same transfer transactions twice without return to UTX" in {
    val waitBlocks = 5
    result(for {
      b <- traverse(nodes)(balanceForNode).map(mutable.AnyRefMap[String, Long](_: _*))
      requests = generateRequests(301, b)
      startHeight <- Future.traverse(nodes)(_.height).map(_.min)
      _ <- processRequests(requests)
      hashAfterFirstTry <- traverse(nodes)(_.waitForDebugInfoAt(startHeight + waitBlocks).map(_.stateHash)).map(infos => {
        all(infos) shouldEqual infos.head
        infos.head
      })
      stateAfterFirstTry <- nodes.head.debugStateAt(startHeight + waitBlocks)
      _ <- traverse(nodes)(_.rollback(startHeight, returnToUTX = false))
      _ <- processRequests(requests)
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
}
