package com.wavesplatform.network

import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.RxScoreObserver.ChannelClosedAndSyncWith
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.{BlockGen, RxScheduler}
import io.netty.channel.Channel
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.local.LocalChannel
import io.netty.util
import monix.eval.{Coeval, Task}
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject as PS

import scala.concurrent.duration.*
import scala.concurrent.{Future, Promise}
import scala.util.Try

class RxExtensionLoaderSpec extends FreeSpec with RxScheduler with BlockGen {
  import RxExtensionLoaderSpec.*

  val MaxRollback = 10
  type Applier = (Channel, ExtensionBlocks) => Task[Either[ValidationError, Option[BigInt]]]
  val simpleApplier: Applier = (_, _) => Task(Right(Some(0)))

  override def testSchedulerName: String = "test-rx-extension-loader"

  private def withExtensionLoader(
      lastBlockIds: Seq[ByteStr] = Seq.empty,
      timeOut: FiniteDuration = 1.day,
      cacheTimeout: FiniteDuration = 3.minute,
      applier: Applier = simpleApplier
  )(
      f: (
          InMemoryInvalidBlockStorage,
          PS[(Channel, Block)],
          PS[(Channel, Signatures)],
          PS[ChannelClosedAndSyncWith],
          Observable[(Channel, Block, Option[BlockSnapshotResponse])]
      ) => Any
  ) = {
    val blocks          = PS[(Channel, Block)]()
    val sigs            = PS[(Channel, Signatures)]()
    val ccsw            = PS[ChannelClosedAndSyncWith]()
    val snapshots       = PS[(Channel, BlockSnapshotResponse)]()
    val timeout         = PS[Channel]()
    val op              = PeerDatabase.NoOp
    val invBlockStorage = new InMemoryInvalidBlockStorage
    val (singleBlocks, _, _) =
      RxExtensionLoader(
        timeOut,
        cacheTimeout,
        isLightMode = false,
        Coeval(lastBlockIds.reverse.take(MaxRollback)),
        op,
        invBlockStorage,
        blocks,
        sigs,
        snapshots,
        ccsw,
        testScheduler,
        timeout
      )(
        applier
      )

    try {
      f(invBlockStorage, blocks, sigs, ccsw, singleBlocks)
    } finally {
      blocks.onComplete()
      sigs.onComplete()
      ccsw.onComplete()
      timeout.onComplete()
    }
  }

  "should propagate unexpected block" in withExtensionLoader() { (_, blocks, _, _, singleBlocks) =>
    val ch              = new LocalChannel()
    val newSingleBlocks = newItems(singleBlocks)
    val block           = randomSignerBlockGen.sample.get

    test(for {
      _ <- send(blocks)((ch, block))
    } yield {
      newSingleBlocks().last shouldBe ((ch, block, None))
    })
  }

  "should blacklist GetSignatures timeout" in withExtensionLoader(Seq.tabulate(100)(byteStr), 1.millis) { (_, _, _, ccsw, _) =>
    val ch = new EmbeddedChannel()
    test(for {
      _ <- send(ccsw, timeout = 1000)(ChannelClosedAndSyncWith(None, Some(BestChannel(ch, 1: BigInt))))
    } yield {
      ch.isOpen shouldBe false
    })
  }

  "should request GetSignatures and then span blocks from peer" in withExtensionLoader(Seq.tabulate(100)(byteStr)) { (_, _, sigs, ccsw, _) =>
    val ch                   = new EmbeddedChannel()
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

  "should blacklist if received Signatures contains banned id" in withExtensionLoader(Seq.tabulate(100)(byteStr), 1.millis) {
    (invBlockStorage, _, sigs, ccsw, _) =>
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

  "should blacklist if some blocks didn't arrive in due time" in withExtensionLoader(Seq.tabulate(100)(byteStr), 1.second) {
    (_, blocks, sigs, ccsw, _) =>
      val ch = new EmbeddedChannel()

      test(for {
        _ <- send(ccsw)(ChannelClosedAndSyncWith(None, Some(BestChannel(ch, 1: BigInt))))
        _ = ch.readOutbound[GetSignatures].signatures.size shouldBe MaxRollback
        _ <- send(sigs)((ch, Signatures(Range(97, 102).map(byteStr))))
        _ = ch.readOutbound[GetBlock].signature shouldBe byteStr(100)
        _ = ch.readOutbound[GetBlock].signature shouldBe byteStr(101)
        _ <- send(blocks)((ch, block(100)))
        _ <- ch.closeF()
      } yield ())
  }

  "should process received extension" in {
    @volatile var applied = false
    val successfulApplier: Applier = (_, _) =>
      Task {
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

  "should blacklist peer after receiving empty signature list" in {
    withExtensionLoader(Seq.tabulate(100)(byteStr)) { (_, _, sigs, ccsw, _) =>
      val ch = new EmbeddedChannel()

      test(for {
        _ <- send(ccsw)(ChannelClosedAndSyncWith(None, Some(BestChannel(ch, 1: BigInt))))
        _ = ch.readOutbound[GetSignatures].signatures.size shouldBe MaxRollback
        _ <- send(sigs)((ch, Signatures(Seq.empty)))
        _ <- ch.closeF()
      } yield ())
    }
  }
}

object RxExtensionLoaderSpec {
  implicit class ChannelExt(val channel: Channel) extends AnyVal {
    def closeF(): Future[Unit] = {
      val closePromise = Promise[Unit]()
      channel.closeFuture().addListener((future: util.concurrent.Future[? >: Void]) => closePromise.complete(Try(future.get())))
      closePromise.future
    }
  }
}
