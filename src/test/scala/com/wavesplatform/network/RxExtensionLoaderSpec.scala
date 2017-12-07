package com.wavesplatform.network

import com.wavesplatform.network.RxExtensionLoader.ExtensionBlocks
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
import scorex.block.{Block, SignerData}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.History.BlockchainScore
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError

import scala.concurrent.duration._

class RxExtensionLoaderSpec extends FreeSpec with Matchers with TransactionGen with RxScheduler with MockFactory with BlockGen {

  def byteStr(id: Int): ByteStr = ByteStr(Array(id.toByte))

  def block(id: Int): Block = TestBlock.create(Seq.empty).copy(signerData = SignerData(TestBlock.defaultSigner, byteStr(id)))

  def genBlocks(amt: Int): List[Block] = Gen.listOfN(amt, randomSignerBlockGen).sample.get

  val MaxRollback = 10
  type Applier = (Channel, ExtensionBlocks) => Task[Either[ValidationError, Option[BlockchainScore]]]
  val simpleApplier: Applier = (_, _) => Task(Right(Some(0)))

  def buildExtensionLoader(timeOut: FiniteDuration = 1.day, applier: Applier = simpleApplier):
  (TestHistory, InMemoryInvalidBlockStorage, PublishSubject[(Channel, Block)], PublishSubject[(Channel, Signatures)], PublishSubject[ChannelClosedAndSyncWith], Observable[(Channel, Block)]) = {
    val blocks = PublishSubject[(Channel, Block)]
    val sigs = PublishSubject[(Channel, Signatures)]
    val ccsw = PublishSubject[ChannelClosedAndSyncWith]
    val history = new TestHistory
    val op = PeerDatabase.NoOp
    val invBlockStorage = new InMemoryInvalidBlockStorage
    val singleBlocks = RxExtensionLoader(MaxRollback, timeOut, history, op, invBlockStorage, blocks, sigs, ccsw)(applier)

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

  "should blacklist GetSignatures timeout" in {
    val (history, invBlockStorage, blocks, sigs, ccsw, singleBlocks) = buildExtensionLoader(1.millis)
    val ch = new EmbeddedChannel()
    val totalBlocksInHistory = 100
    Range(0, totalBlocksInHistory).map(byteStr).foreach(history.appendId)
    test(for {
      _ <- send(ccsw)(ChannelClosedAndSyncWith(None, Some(BestChannel(ch, 1: BigInt))))
    } yield {
      ch.isOpen shouldBe false
    })
  }

  "should request GetSignatures and then span blocks from peer" in {
    val (history, invBlockStorage, blocks, sigs, ccsw, singleBlocks) = buildExtensionLoader()
    val ch = new EmbeddedChannel()
    val totalBlocksInHistory = 100
    Range(0, totalBlocksInHistory).map(byteStr).foreach(history.appendId)
    test(for {
      _ <- send(ccsw)(ChannelClosedAndSyncWith(None, Some(BestChannel(ch, 1: BigInt))))
      _ = ch.readOutbound[GetSignatures].signatures shouldBe Range(totalBlocksInHistory - MaxRollback, totalBlocksInHistory).map(byteStr).reverse
      _ <- send(sigs)((ch, Signatures(Range(97, 102).map(byteStr))))
    } yield {
      ch.readOutbound[GetBlock].signature shouldBe byteStr(100)
      ch.readOutbound[GetBlock].signature shouldBe byteStr(101)
    })
  }

  "should blacklist if received Signatures contains banned id" in {
    val (history, invBlockStorage, blocks, sigs, ccsw, singleBlocks) = buildExtensionLoader(1.millis)
    invBlockStorage.add(byteStr(105), GenericError("Some error"))
    val ch = new EmbeddedChannel()
    val totalBlocksInHistory = 100
    Range(0, totalBlocksInHistory).map(byteStr).foreach(history.appendId)
    test(for {
      _ <- send(ccsw)(ChannelClosedAndSyncWith(None, Some(BestChannel(ch, 1: BigInt))))
      _ = ch.readOutbound[GetSignatures].signatures.size shouldBe MaxRollback
      _ <- send(sigs)((ch, Signatures(Range(99, 110).map(byteStr))))
    } yield {
      ch.isOpen shouldBe false
    })
  }


  "should blacklist if some blocks didn't arrive in due time" in {
    val (history, invBlockStorage, blocks, sigs, ccsw, singleBlocks) = buildExtensionLoader(timeOut = 1.second)
    val ch = new EmbeddedChannel()
    val totalBlocksInHistory = 100
    Range(0, totalBlocksInHistory).map(byteStr).foreach(history.appendId)
    test(for {
      _ <- send(ccsw)(ChannelClosedAndSyncWith(None, Some(BestChannel(ch, 1: BigInt))))
      _ = ch.readOutbound[GetSignatures].signatures.size shouldBe MaxRollback
      _ <- send(sigs)((ch, Signatures(Range(97, 102).map(byteStr))))
      _ = ch.readOutbound[GetBlock].signature shouldBe byteStr(100)
      _ = ch.readOutbound[GetBlock].signature shouldBe byteStr(101)
      _ <- send(blocks)((ch, block(100)))
    } yield {
      Thread.sleep(1000)
      ch.isOpen shouldBe false
    })
  }


  "should process received extension" in {
    @volatile var applied = false
    val successfulApplier: Applier = (_, _) => Task {
      applied = true
      Right(None)
    }
    val (history, invBlockStorage, blocks, sigs, ccsw, singleBlocks) = buildExtensionLoader(applier = successfulApplier)
    val ch = new EmbeddedChannel()
    val totalBlocksInHistory = 100
    Range(0, totalBlocksInHistory).map(byteStr).foreach(history.appendId)
    test(for {
      _ <- send(ccsw)(ChannelClosedAndSyncWith(None, Some(BestChannel(ch, 1: BigInt))))
      _ = ch.readOutbound[GetSignatures].signatures.size shouldBe MaxRollback
      _ <- send(sigs)((ch, Signatures(Range(97, 102).map(byteStr))))
      _ = ch.readOutbound[GetBlock].signature shouldBe byteStr(100)
      _ = ch.readOutbound[GetBlock].signature shouldBe byteStr(101)
      _ <- send(blocks)((ch, block(100)))
      _ <- send(blocks)((ch, block(101)))
    } yield {
      applied shouldBe true
    })
  }
}
