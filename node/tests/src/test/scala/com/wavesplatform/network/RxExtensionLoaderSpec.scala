package com.wavesplatform.network

import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.RxExtensionLoader.ExtensionBlocks
import com.wavesplatform.network.RxScoreObserver.{BestChannel, ChannelClosedAndSyncWith}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.{BlockGen, RxScheduler, TransactionGen}
import io.netty.channel.Channel
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.local.LocalChannel
import monix.eval.{Coeval, Task}
import monix.reactive.Observable
import monix.reactive.subjects.{PublishSubject => PS}
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.duration._

class RxExtensionLoaderSpec extends FreeSpec with Matchers with TransactionGen with RxScheduler with BlockGen {

  val MaxRollback = 10
  type Applier = (Channel, ExtensionBlocks) => Task[Either[ValidationError, Option[BigInt]]]
  val simpleApplier: Applier = (_, _) => Task(Right(Some(0)))

  override def testSchedulerName: String = "test-rx-extension-loader"

  private def withExtensionLoader(lastBlockIds: Seq[ByteStr] = Seq.empty, timeOut: FiniteDuration = 1.day, applier: Applier = simpleApplier)(
      f: (InMemoryInvalidBlockStorage,
          PS[(Channel, Block)],
          PS[(Channel, Signatures)],
          PS[ChannelClosedAndSyncWith],
          Observable[(Channel, Block)]) => Any) = {
    val blocks          = PS[(Channel, Block)]
    val sigs            = PS[(Channel, Signatures)]
    val ccsw            = PS[ChannelClosedAndSyncWith]
    val timeout         = PS[Channel]
    val op              = PeerDatabase.NoOp
    val invBlockStorage = new InMemoryInvalidBlockStorage
    val (singleBlocks, _, _) =
      RxExtensionLoader(timeOut, Coeval(lastBlockIds.reverse.take(MaxRollback)), op, invBlockStorage, blocks, sigs, ccsw, testScheduler, timeout)(
        applier)

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

      val preconditionTask = Task.defer {
        Task.fromFuture {
          for {
            _ <- send(ccsw)(ChannelClosedAndSyncWith(None, Some(BestChannel(ch, 1: BigInt))))
            _ = ch.readOutbound[GetSignatures].signatures.size shouldBe MaxRollback
            _ <- send(sigs)((ch, Signatures(Range(97, 102).map(byteStr))))
            _ = ch.readOutbound[GetBlock].signature shouldBe byteStr(100)
            _ = ch.readOutbound[GetBlock].signature shouldBe byteStr(101)
            _ <- send(blocks)((ch, block(100)))
          } yield ()
        }
      }

      val testTask =
        for {
          _ <- Task
            .race(Task.sleep(8.seconds), preconditionTask)
            .flatMap {
              case Left(_) => Task.raiseError(new Exception("Preparation for the test takes too much time"))
              case _       => Task.pure(())
            }
          _ <- Task.sleep(1.second)
        } yield ch.isOpen shouldBe false

      test(testTask.runToFuture(monix.execution.Scheduler.global))
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
}
