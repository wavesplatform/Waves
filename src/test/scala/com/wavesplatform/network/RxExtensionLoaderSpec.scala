package com.wavesplatform.network

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.network.RxExtensionLoader.ExtensionBlocks
import com.wavesplatform.network.RxScoreObserver.{BestChannel, SyncWith}
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.{BlockGen, RxScheduler, TransactionGen}
import io.netty.channel.Channel
import io.netty.channel.local.LocalChannel
import monix.eval.Task
import monix.reactive.subjects.PublishSubject
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers}
import scorex.block.Block.BlockId
import scorex.block.{Block, BlockHeader, MicroBlock}
import scorex.transaction.{History, NgHistory, ValidationError}
import scorex.transaction.History.BlockchainScore

import scala.concurrent.duration._

class RxExtensionLoaderSpec extends FreeSpec with Matchers with TransactionGen with RxScheduler with MockFactory with BlockGen {

  class TestHistory extends NgHistory {
    def lastBlockIds_(s: Seq[BlockId]): Unit = lastBlockIds = s

    private var lastBlockIds = Seq.empty[BlockId] // fresh head
    override def lastBlockIds(howMany: Int): Seq[ByteStr] = lastBlockIds.take(howMany)

    override def microBlock(id: ByteStr): Option[MicroBlock] = ???

    override def bestLastBlockInfo(maxTimestamp: Long): Option[History.BlockMinerInfo] = ???

    override def blockBytes(height: Int): Option[Array[Byte]] = ???

    override def heightOf(blockId: ByteStr): Option[Int] = ???

    override def lastBlockTimestamp(): Option[Long] = ???

    override def blockHeaderAndSizeAt(height: Int): Option[(BlockHeader, Int)] = ???

    override def lastBlockId(): Option[ByteStr] = ???

    override def blockAt(height: Int): Option[Block] = ???

    override def height(): Int = ???

    override def scoreOf(id: ByteStr): Option[BlockchainScore] = ???

    override def synchronizationToken: ReentrantReadWriteLock = ???

    override def close(): Unit = ???
  }

  val simpleApplier: (Channel, ExtensionBlocks) => Task[Either[ValidationError, Option[BlockchainScore]]] = (_, _) => Task(Right(Some(0)))

  def buildExtensionLoader() = {
    val bestChannels = PublishSubject[SyncWith]
    val blocks = PublishSubject[(Channel, Block)]
    val sigs = PublishSubject[(Channel, Signatures)]
    val channelClosed = PublishSubject[Channel]
    val history = new TestHistory
    val op = PeerDatabase.NoOp
    val invBlockStorage = InvalidBlockStorage.Empty
    val singleBlocks = RxExtensionLoader(100, 1.minute, history, op, invBlockStorage, bestChannels, blocks, sigs, channelClosed)(simpleApplier)

    (history, invBlockStorage, bestChannels, blocks, sigs, channelClosed, singleBlocks)
  }


  "should propogate unexpected block" in {
    val (history, invBlockStorage, bestChannels, blocks, sigs, channelClosed, singleBlocks) = buildExtensionLoader()
    val ch = new LocalChannel()
    val newSingleBlocks = newItems(singleBlocks)
    val block = randomSignerBlockGen.sample.get

    test(for {
      _ <- send(blocks)((ch, block))
      _ = newSingleBlocks().last shouldBe(ch, block)
    } yield ())
  }

  "should request"
}
