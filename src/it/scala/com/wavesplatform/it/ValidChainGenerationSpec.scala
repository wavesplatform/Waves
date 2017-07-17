package com.wavesplatform.it

import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}

import scala.collection.mutable
import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class ValidChainGenerationSpec(override val allNodes: Seq[Node]) extends FreeSpec with ScalaFutures with IntegrationPatience
  with Matchers with TransferSending {
  "Generate 30 blocks and synchronise" in {
    val targetBlocks = result(for {
      b <- traverse(allNodes)(balanceForNode).map(mutable.AnyRefMap[String, Long](_: _*))
      _ <- processRequests(generateRequests(1000, b))
      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 40)) // wait a little longer to prevent rollbacks...
      _ <- traverse(allNodes)(_.waitForHeight(height + 35)) // ...before requesting actual blocks
      blocks <- traverse(allNodes)(_.blockAt(height + 35))
    } yield blocks.map(_.signature), 5.minutes)

    all(targetBlocks) shouldEqual targetBlocks.head
  }

  "Generate more blocks and resynchronise after rollback" in {
    val targetBlocks1 = result(for {
      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 30)) // wait a little longer to prevent rollbacks...
      _ <- traverse(allNodes)(_.waitForHeight(height + 25)) // ...before requesting actual blocks
      blocks <- traverse(allNodes)(_.blockAt(height + 25))
    } yield blocks.map(_.signature), 5.minutes)

    all(targetBlocks1) shouldEqual targetBlocks1.head

    allNodes.head.rollback(1)

    val targetBlocks2 = result(for {
      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 40)) // wait a little longer to prevent rollbacks...
      _ <- traverse(allNodes)(_.waitForHeight(height + 25)) // ...before requesting actual blocks
      blocks <- traverse(allNodes)(_.blockAt(height + 25))
    } yield blocks.map(_.signature), 5.minutes)

    all(targetBlocks2) shouldEqual targetBlocks2.head

    allNodes.tail.foreach(_.rollback(1))

    val targetBlocks3 = result(for {
      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 40)) // wait a little longer to prevent rollbacks...
      _ <- traverse(allNodes)(_.waitForHeight(height + 25)) // ...before requesting actual blocks
      blocks <- traverse(allNodes)(_.blockAt(height + 25))
    } yield blocks.map(_.signature), 5.minutes)

    all(targetBlocks3) shouldEqual targetBlocks3.head
  }
}
