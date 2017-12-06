package com.wavesplatform.network

import com.wavesplatform.network.RxExtensionLoader.{ApplierState, ExtensionBlocks, LoaderState, State}
import com.wavesplatform.network.RxScoreObserver.{BestChannel, ChannelClosedAndSyncWith}
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.{BlockGen, RxScheduler, TransactionGen}
import io.netty.channel.Channel
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.local.LocalChannel
import monix.eval.Task
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers}
import scorex.block.Block
import scorex.transaction.History.BlockchainScore
import scorex.transaction.ValidationError

import scala.concurrent.duration._

class RxExtensionLoaderSpec extends FreeSpec with Matchers with TransactionGen with RxScheduler with MockFactory with BlockGen {

  def bs(i: Int): ByteStr = ByteStr(Array(i.toByte))

  def genBlocks(amt: Int): List[Block] = Gen.listOfN(amt, randomSignerBlockGen).sample.get

  val Idle = State(LoaderState.Idle, ApplierState.Idle)

  val MaxRollback = 10
  val simpleApplier: (Channel, ExtensionBlocks) => Task[Either[ValidationError, Option[BlockchainScore]]] = (_, _) => Task(Right(Some(0)))

  def buildExtensionLoader(timeOut: FiniteDuration = 1.day):
  (TestHistory, InMemoryInvalidBlockStorage, PublishSubject[(Channel, Block)], PublishSubject[(Channel, Signatures)], PublishSubject[ChannelClosedAndSyncWith], Observable[(Channel, Block)]) = {
    val blocks = PublishSubject[(Channel, Block)]
    val sigs = PublishSubject[(Channel, Signatures)]
    val ccsw = PublishSubject[ChannelClosedAndSyncWith]
    val history = new TestHistory
    val op = PeerDatabase.NoOp
    val invBlockStorage = new InMemoryInvalidBlockStorage
    val singleBlocks = RxExtensionLoader(MaxRollback, timeOut, history, op, invBlockStorage, blocks, sigs, ccsw)(simpleApplier)

    (history, invBlockStorage, blocks, sigs, ccsw, singleBlocks)
  }


  "should propogate unexpected block" in {
    val (history, invBlockStorage, blocks, sigs, ccsw, singleBlocks) = buildExtensionLoader()
    val ch = new LocalChannel()
    val newSingleBlocks = newItems(singleBlocks)
    val block = randomSignerBlockGen.sample.get

    test(for {
      _ <- send(blocks)((ch, block))
    } yield {
      newSingleBlocks().last shouldBe ((ch, block))
    })
  }

  "should request GetSignatures from best channel if loader is idle" in {
    val (history, invBlockStorage, blocks, sigs, ccsw, singleBlocks) = buildExtensionLoader()
    val ch = new EmbeddedChannel()
    val totalBlocksInHistory = 100
    Range(0, totalBlocksInHistory).map(bs).foreach(history.appendId)
    test(for {
      _ <- send(ccsw)(ChannelClosedAndSyncWith(None, Some(BestChannel(ch, 1: BigInt))))
    } yield {
      ch.readOutbound[GetSignatures].signatures shouldBe Range(totalBlocksInHistory - MaxRollback, totalBlocksInHistory).map(bs).reverse
    })
  }

  "should blacklist GetSignatures timeout" in {
    val (history, invBlockStorage, blocks, sigs, ccsw, singleBlocks) = buildExtensionLoader(1.millis)
    val ch = new EmbeddedChannel()
    val totalBlocksInHistory = 100
    Range(0, totalBlocksInHistory).map(bs).foreach(history.appendId)
    test(for {
      _ <- send(ccsw)(ChannelClosedAndSyncWith(None, Some(BestChannel(ch, 1: BigInt))))
    } yield {
      ch.isOpen shouldBe false
    })
  }

  "should request span blocks from peer" in {
    val (history, invBlockStorage, blocks, sigs, ccsw, singleBlocks) = buildExtensionLoader()
    val ch = new EmbeddedChannel()
    val totalBlocksInHistory = 100
    Range(0, totalBlocksInHistory).map(bs).foreach(history.appendId)
    test(for {
      _ <- send(ccsw)(ChannelClosedAndSyncWith(None, Some(BestChannel(ch, 1: BigInt))))
      _ = ch.readOutbound[GetSignatures].signatures.size shouldBe MaxRollback
      _ <- send(sigs)((ch, Signatures(Range(97, 102).map(bs))))
    } yield {
      ch.readOutbound[GetBlock].signature shouldBe bs(100)
      ch.readOutbound[GetBlock].signature shouldBe bs(101)
    })
  }

}
