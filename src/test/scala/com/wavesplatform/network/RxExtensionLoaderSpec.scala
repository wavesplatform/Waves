package com.wavesplatform.network

import com.wavesplatform.network.RxExtensionLoader.{ApplierState, ExtensionBlocks, LoaderState, State}
import com.wavesplatform.network.RxScoreObserver.{BestChannel, SyncWith}
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.{BlockGen, RxScheduler, TransactionGen}
import io.netty.channel.Channel
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.local.LocalChannel
import monix.eval.Task
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

  def genBlocks(amt: Int) = Gen.listOfN(amt, randomSignerBlockGen).sample.get

  val Idle = State(LoaderState.Idle, ApplierState.Idle)

  val MaxRollback = 10
  val simpleApplier: (Channel, ExtensionBlocks) => Task[Either[ValidationError, Option[BlockchainScore]]] = (_, _) => Task(Right(Some(0)))

  def buildExtensionLoader(timeOut: FiniteDuration = 1.day) = {
    val bestChannels = PublishSubject[SyncWith]
    val blocks = PublishSubject[(Channel, Block)]
    val sigs = PublishSubject[(Channel, Signatures)]
    val channelClosed = PublishSubject[Channel]
    val history = new TestHistory
    val op = PeerDatabase.NoOp
    val invBlockStorage = InvalidBlockStorage.Empty
    val singleBlocks = RxExtensionLoader(MaxRollback, timeOut, history, op, invBlockStorage, bestChannels, blocks, sigs, channelClosed)(simpleApplier)

    (history, invBlockStorage, bestChannels, blocks, sigs, channelClosed, singleBlocks)
  }

  "should propogate unexpected block" in {
    val (_, _, _, blocks, _, _, singleBlocks) = buildExtensionLoader()
    val ch = new LocalChannel()
    val newSingleBlocks = newItems(singleBlocks)
    val block = randomSignerBlockGen.sample.get

    test(for {
      _ <- send(blocks)((ch, block))
    } yield {
      newSingleBlocks().last shouldBe((ch, block))
    })
  }

  "should request GetSignatures from best channel if loader is idle" in {
    val (history, _, bestChannels, _, _, _, _) = buildExtensionLoader()
    val ch = new EmbeddedChannel()
    val totalBlocksInHistory = 100
    Range(0, totalBlocksInHistory).map(bs).foreach(history.appendId)
    test(for {
      _ <- send(bestChannels)(Some(BestChannel(ch, 1: BigInt)))
    } yield {
      ch.readOutbound[GetSignatures].signatures shouldBe Range(totalBlocksInHistory - MaxRollback, totalBlocksInHistory).map(bs).reverse
    })
  }

  "should blacklist GetSignatures timeout" in {
    val (history, _, bestChannels, _, _, _, _) = buildExtensionLoader(1.millis)
    val ch = new EmbeddedChannel()
    val totalBlocksInHistory = 100
    Range(0, totalBlocksInHistory).map(bs).foreach(history.appendId)
    test(for {
      _ <- send(bestChannels)(Some(BestChannel(ch, 1: BigInt)))
    } yield {
      ch.isOpen shouldBe false
    })
  }
}
