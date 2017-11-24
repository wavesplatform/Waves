package com.wavesplatform.it

import com.wavesplatform.it.api.NodeApi.{Block, BlockHeaders}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.{sequence, traverse}
import scala.concurrent.duration._
import com.wavesplatform.it.util._

import scala.util.Random

class BlockHeadersSpecification(override val allNodes: Seq[Node], override val notMiner: Node)
  extends IntegrationSuiteWithThreeAddresses {

  private def txRequestsGen(n: Int, fee: Long): Future[Unit] = {
    val parallelRequests = 1

    def requests(n: Int): Future[Unit] = Future
      .sequence {
        (1 to n).map { _ => notMiner.transfer(notMiner.address, firstAddress, (1 + Random.nextInt(10)).waves, fee) }
      }
      .map(_ => ())

    val steps = (1 to n)
      .sliding(parallelRequests, parallelRequests)
      .map(_.size)

    steps.foldLeft(Future.successful(())) { (r, numRequests) => r.flatMap(_ => requests(numRequests)) }
  }

  def assertBlockInfo(blocks: Seq[Block], blockHeaders: Seq[BlockHeaders]): Unit = {
    all(blockHeaders.map(_.generator)) shouldBe blocks.head.generator
    all(blockHeaders.map(_.timestamp)) shouldBe blocks.head.timestamp
    all(blockHeaders.map(_.signature)) shouldBe blocks.head.signature
    all(blockHeaders.map(_.fee)) shouldBe blocks.head.fee
    all(blockHeaders.map(_.transactionsCount)) shouldBe blocks.head.transactions.size
  }

  test("blockAt content should be equal to blockHeaderAt, except transactions info") {
    val f = for {
      baseHeight <- traverse(allNodes)(_.height).map(_.max)
      _ <- txRequestsGen(30, 2.waves)
      _ <- traverse(allNodes)(_.waitForHeight(baseHeight + 3))
      blocks <- traverse(allNodes)(_.blockAt(baseHeight + 1))
      blocksHeaders <- traverse(allNodes)(_.blockHeadersAt(baseHeight + 1))
    } yield {
      assertBlockInfo(blocks, blocksHeaders)
    }
    Await.result(f, 1.minute)
  }

  test("lastBlock content should be equal to lastBlockHeader, except transactions info") {
    val f = for {
      baseHeight <- traverse(allNodes)(_.height).map(_.max)
      _ <- txRequestsGen(30, 2.waves)
      _ <- traverse(allNodes)(_.waitForHeight(baseHeight + 1))
      blocks <- traverse(allNodes)(_.lastBlock)
      blocksHeaders <- traverse(allNodes)(_.lastBlockHeaders)
    } yield {
      assertBlockInfo(blocks, blocksHeaders)
    }

    Await.result(f, 2.minute)
  }

  test("blockSeq content should be equal to blockHeaderSeq, except transactions info") {
    val f = for {
      baseHeight <- traverse(allNodes)(_.height).map(_.max)
      _ <- txRequestsGen(30, 2.waves)
      _ <- traverse(allNodes)(_.waitForHeight(baseHeight + 10))
      blocks <- allNodes.head.blockSeq(baseHeight + 1, baseHeight + 3)
      blocksHeaders <- allNodes.head.blockHeadersSeq(baseHeight + 1, baseHeight + 3)
    } yield succeed

    Await.result(f, 2.minute)
  }

}
