package com.wavesplatform.network

import com.wavesplatform.network.RxExtensionLoader.ExtensionBlocks
import com.wavesplatform.network.RxScoreObserver.{BestChannel, ChannelClosedAndSyncWith}
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.{BlockGen, RxScheduler, TransactionGen}
import io.netty.channel.Channel
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.local.LocalChannel
import monix.eval.{Coeval, Task}
import monix.reactive.Observable
import monix.reactive.subjects.{PublishSubject => PS}
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

  private def withExtensionLoader(lastBlockIds: Seq[ByteStr] = Seq.empty, timeOut: FiniteDuration = 1.day, applier: Applier = simpleApplier)
                                 (f: (InMemoryInvalidBlockStorage, PS[(Channel, Block)],
                                   PS[(Channel, Signatures)], PS[ChannelClosedAndSyncWith],
                                   Observable[(Channel, Block)]) => Any) = {
    val blocks = PS[(Channel, Block)]
    val sigs = PS[(Channel, Signatures)]
    val ccsw = PS[ChannelClosedAndSyncWith]
    val timeout = PS[Channel]
    val op = PeerDatabase.NoOp
    val invBlockStorage = new InMemoryInvalidBlockStorage
    val (singleBlocks, _, _) = RxExtensionLoader(timeOut, Coeval(lastBlockIds.reverse.take(MaxRollback)), op,
      invBlockStorage, blocks, sigs, ccsw, testScheduler, timeout)(applier)

    try {
      f(invBlockStorage, blocks, sigs, ccsw, singleBlocks)
    }
    finally {
      blocks.onComplete()
      sigs.onComplete()
      ccsw.onComplete()
      timeout.onComplete()
    }
  }

  "should propagate unexpected block" in withExtensionLoader() { (_, blocks, _, _, singleBlocks) =>
    val ch = new LocalChannel()
    val newSingleBlocks = newItems(singleBlocks)
    val block = randomSignerBlockGen.sample.get

    test(for {
      _ <- send(blocks)((ch, block))
    } yield {
      newSingleBlocks().last shouldBe ((ch, block))
    })
  }

  "should blacklist GetSignatures timeout" in withExtensionLoader(Seq.tabulate(100)(byteStr), 1.millis) { (_, _, _, ccsw, _) =>
    val ch = new EmbeddedChannel()
    test(for {
      _ <- send(ccsw)(ChannelClosedAndSyncWith(None, Some(BestChannel(ch, 1: BigInt))))
    } yield {
      ch.isOpen shouldBe false
    })
  }

  "should request GetSignatures and then span blocks from peer" in withExtensionLoader(Seq.tabulate(100)(byteStr)) { (_, _, sigs, ccsw, _) =>
    val ch = new EmbeddedChannel()
    val totalBlocksInHistory = 100
    test(for {
      _ <- send(ccsw)(ChannelClosedAndSyncWith(None, Some(BestChannel(ch, 1: BigInt))))
      _ = ch.readOutbound[GetSignatures].signatures shouldBe Range(totalBlocksInHistory - MaxRollback, totalBlocksInHistory).map(byteStr).reverse
      _ <- send(sigs)((ch, Signatures(Range(97, 102).map(byteStr))))
    } yield {
      ch.readOutbound[GetBlock].signature shouldBe byteStr(100)
      ch.readOutbound[GetBlock].signature shouldBe byteStr(101)
    })
  }

  "should blacklist if received Signatures contains banned id" in withExtensionLoader(Seq.tabulate(100)(byteStr), 1.millis) { (invBlockStorage, _, sigs, ccsw, _) =>
    invBlockStorage.add(byteStr(105), GenericError("Some error"))
    val ch = new EmbeddedChannel()
    test(for {
      _ <- send(ccsw)(ChannelClosedAndSyncWith(None, Some(BestChannel(ch, 1: BigInt))))
      _ = ch.readOutbound[GetSignatures].signatures.size shouldBe MaxRollback
      _ <- send(sigs)((ch, Signatures(Range(99, 110).map(byteStr))))
    } yield {
      ch.isOpen shouldBe false
    })
  }

  "should blacklist if some blocks didn't arrive in due time" in withExtensionLoader(Seq.tabulate(100)(byteStr), 1.second) { (_, blocks, sigs, ccsw, _) =>
    val ch = new EmbeddedChannel()
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
    withExtensionLoader(Seq.tabulate(100)(byteStr), applier = successfulApplier) { (_, blocks, sigs, ccsw, _) =>
      val ch = new EmbeddedChannel()
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
