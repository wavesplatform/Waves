package com.wavesplatform.network

import com.wavesplatform.network.RxExtensionLoader.ExtensionBlocks
import com.wavesplatform.network.RxScoreObserver.{BestChannel, ChannelClosedAndSyncWith}
import com.wavesplatform.{BlockGen, RxScheduler, TransactionGen}
import io.netty.channel.Channel
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.local.LocalChannel
import monix.eval.Task
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import org.scalatest.{FreeSpec, Matchers}
import scorex.block.Block
import scorex.transaction.History.BlockchainScore
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError

import scala.concurrent.duration._

class RxExtensionLoaderSpec extends FreeSpec with Matchers with TransactionGen with RxScheduler with BlockGen {

  val MaxRollback = 10
  type Applier = (Channel, ExtensionBlocks) => Task[Either[ValidationError, Option[BlockchainScore]]]
  val simpleApplier: Applier = (_, _) => Task(Right(Some(0)))

  override def testSchedulerName: String = "test-rx-extension-loader"

  private def withExtensionLoader(timeOut: FiniteDuration = 1.day, applier: Applier = simpleApplier)
                                 (f: (TestHistory, InMemoryInvalidBlockStorage, PublishSubject[(Channel, Block)],
                                   PublishSubject[(Channel, Signatures)], PublishSubject[ChannelClosedAndSyncWith],
                                   Observable[(Channel, Block)]) => Any) = {
    val blocks = PublishSubject[(Channel, Block)]
    val sigs = PublishSubject[(Channel, Signatures)]
    val ccsw = PublishSubject[ChannelClosedAndSyncWith]
    val history = new TestHistory
    val op = PeerDatabase.NoOp
    val invBlockStorage = new InMemoryInvalidBlockStorage
    val (singleBlocks, _, _) = RxExtensionLoader(MaxRollback, timeOut, history, op, invBlockStorage, blocks, sigs, ccsw, testScheduler)(applier)

    try {
      f(history, invBlockStorage, blocks, sigs, ccsw, singleBlocks)
    }
    finally {
      blocks.onComplete()
      sigs.onComplete()
      ccsw.onComplete()
    }
  }

  "should propagate unexpected block" in withExtensionLoader() { (_, _, blocks, _, _, singleBlocks) =>
    val ch = new LocalChannel()
    val newSingleBlocks = newItems(singleBlocks)
    val block = randomSignerBlockGen.sample.get

    test(for {
      _ <- send(blocks)((ch, block))
    } yield {
      newSingleBlocks().last shouldBe ((ch, block))
    })
  }

  "should blacklist GetSignatures timeout" in withExtensionLoader(1.millis) { (history, _, _, _, ccsw, _) =>
    val ch = new EmbeddedChannel()
    val totalBlocksInHistory = 100
    Range(0, totalBlocksInHistory).map(byteStr).foreach(history.appendId)
    test(for {
      _ <- send(ccsw)(ChannelClosedAndSyncWith(None, Some(BestChannel(ch, 1: BigInt))))
    } yield {
      ch.isOpen shouldBe false
    })
  }

  "should request GetSignatures and then span blocks from peer" in withExtensionLoader() { (history, _, _, sigs, ccsw, _) =>
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

  "should blacklist if received Signatures contains banned id" in withExtensionLoader(1.millis) { (history, invBlockStorage, _, sigs, ccsw, _) =>
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

  "should blacklist if some blocks didn't arrive in due time" in withExtensionLoader(timeOut = 1.second) { (history, _, blocks, sigs, ccsw, _) =>
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
    withExtensionLoader(applier = successfulApplier) { (history, _, blocks, sigs, ccsw, _) =>
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
}
