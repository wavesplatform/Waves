package com.wavesplatform.it

import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}

import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class ValidChainGenerationSpec(override val nodes: Seq[Node]) extends FreeSpec with ScalaFutures with IntegrationPatience
  with Matchers with TransferSending {

  private val requestsCount = 1000

  "Generate 30 blocks and synchronise" in {
    val targetBlocks = result(for {
      b <- traverse(nodes)(balanceForNode).map(_.toMap)

      height <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(height + 1))

      _ <- processRequests(generateTransfersBetweenAccounts(requestsCount, b))

      height <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(height + 30))

      signatures <- traverse(nodes)(_.blockAt(height + 25).map(_.signature))
      eq = signatures.tail.forall(_ == signatures.head)
      _ <- if (eq) Future.successful(()) else traverse(nodes)(_.waitForHeight(height + 40))

      blocks <- traverse(nodes)(_.blockAt(height + 25))
    } yield blocks.map(_.signature), 6.minutes)

    all(targetBlocks) shouldEqual targetBlocks.head
  }

  "Generate more blocks and resynchronise after rollback" in {
    val targetBlocks1 = result(for {
      height <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(height + 30))
      blocks <- traverse(nodes)(_.blockAt(height + 25))
    } yield blocks.map(_.signature), 5.minutes)

    all(targetBlocks1) shouldEqual targetBlocks1.head

    nodes.head.rollback(1)

    val targetBlocks2 = result(for {
      height <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(height + 30))
      blocks <- traverse(nodes)(_.blockAt(height + 25))
    } yield blocks.map(_.signature), 5.minutes)

    all(targetBlocks2) shouldEqual targetBlocks2.head

    nodes.tail.foreach(_.rollback(1))

    val targetBlocks3 = result(for {
      height <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(height + 30))
      blocks <- traverse(nodes)(_.blockAt(height + 25))
    } yield blocks.map(_.signature), 5.minutes)

    all(targetBlocks3) shouldEqual targetBlocks3.head
  }
}
