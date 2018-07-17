package com.wavesplatform.it.async

import com.typesafe.config.Config
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.api._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.it.{Node, NodeConfigs, ReportingTestName}
import com.wavesplatform.utils.ScorexLogging
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class BlockHeadersTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with NodesFromDocker
    with ReportingTestName
    with ScorexLogging {

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(2))
      .withDefault(2)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

  private def notMiner: Node = nodes.last

  private def firstAddress = nodes(1).address

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Await.result(traverse(nodes)(_.waitForHeight(2)), 1.minute)
  }

  private def txRequestsGen(n: Int, fee: Long): Future[Unit] = {
    val parallelRequests = 1

    def requests(n: Int): Future[Unit] =
      Future
        .sequence {
          (1 to n).map { _ =>
            notMiner.transfer(notMiner.address, firstAddress, (1 + Random.nextInt(10)).waves, fee)
          }
        }
        .map(_ => ())

    val steps = (1 to n)
      .sliding(parallelRequests, parallelRequests)
      .map(_.size)

    steps.foldLeft(Future.successful(())) { (r, numRequests) =>
      r.flatMap(_ => requests(numRequests))
    }
  }

  def assertBlockInfo(blocks: Seq[Block], blockHeaders: Seq[BlockHeaders]): Unit = {
    blockHeaders.head.generator shouldBe blocks.head.generator
    blockHeaders.head.timestamp shouldBe blocks.head.timestamp
    blockHeaders.head.signature shouldBe blocks.head.signature
    blockHeaders.head.transactionCount shouldBe blocks.head.transactions.size
  }

  "blockAt content should be equal to blockHeaderAt, except transactions info" in {
    val f = for {
      baseHeight    <- traverse(nodes)(_.height).map(_.max)
      _             <- txRequestsGen(30, 2.waves)
      _             <- traverse(nodes)(_.waitForHeight(baseHeight + 3))
      blocks        <- traverse(nodes)(_.blockAt(baseHeight + 1))
      blocksHeaders <- traverse(nodes)(_.blockHeadersAt(baseHeight + 1))
    } yield {
      assertBlockInfo(blocks, blocksHeaders)
    }
    Await.result(f, 2.minute)
  }

  "lastBlock content should be equal to lastBlockHeader, except transactions info" in {
    val f = for {
      baseHeight             <- traverse(nodes)(_.height).map(_.max)
      _                      <- txRequestsGen(30, 2.waves)
      _                      <- traverse(nodes)(_.waitForHeight(baseHeight + 1))
      blocksAndBlocksHeaders <- traverse(nodes)(b => (b.lastBlock zip b.lastBlockHeaders))
    } yield {
      assertBlockInfo(blocksAndBlocksHeaders.map(_._1), blocksAndBlocksHeaders.map(_._2))
    }

    Await.result(f, 2.minute)
  }

  "blockSeq content should be equal to blockHeaderSeq, except transactions info" in {
    val f = for {
      baseHeight   <- traverse(nodes)(_.height).map(_.max)
      _            <- txRequestsGen(30, 2.waves)
      _            <- nodes.waitForSameBlockHeadesAt(baseHeight + 3)
      blocks       <- nodes.head.blockSeq(baseHeight + 1, baseHeight + 3)
      blockHeaders <- nodes.head.blockHeadersSeq(baseHeight + 1, baseHeight + 3)
    } yield {
      blocks.zip(blockHeaders).foreach {
        case (block, header) =>
          header.generator shouldBe block.generator
          header.timestamp shouldBe block.timestamp
          header.signature shouldBe block.signature
          header.transactionCount shouldBe block.transactions.size
      }
    }

    Await.result(f, 2.minute)
  }

}
