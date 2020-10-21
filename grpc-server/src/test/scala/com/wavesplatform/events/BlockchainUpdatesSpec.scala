package com.wavesplatform.events

import java.nio.file.Files

import scala.concurrent.Promise
import scala.concurrent.duration._

import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.events.api.grpc.BlockchainUpdatesApiGrpcImpl
import com.wavesplatform.events.api.grpc.protobuf.{SubscribeEvent, SubscribeRequest}
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.repo.UpdatesRepoImpl
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.{Constants, TestFunctionalitySettings}
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.TxHelpers
import io.grpc.stub.StreamObserver
import io.grpc.StatusException
import monix.execution.CancelableFuture
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.concurrent.ScalaFutures

class BlockchainUpdatesSpec extends FreeSpec with Matchers with WithDomain with ScalaFutures {
  override implicit val patienceConfig: PatienceConfig = PatienceConfig(5 seconds, 100 millis)

  "gRPC API" - withRepo { (repo, _) =>
    "return valid errors" in {
      val (obs, result) = createFakeObserver[SubscribeEvent]()
      repo.subscribe(SubscribeRequest(999, 999), obs)
      result.failed.futureValue should matchPattern {
        case se: StatusException if se.getMessage.contains("Requested start height exceeds current blockchain height") =>
      }
    }
  }

  "BlockchainUpdates" - {
    "should include correct waves amount" in withRepo(
      (repo, triggers) =>
        withDomain(domainSettingsWithFS(TestFunctionalitySettings.withFeatures(BlockchainFeatures.BlockReward)), triggers) { d =>
          d.appendBlock(TxHelpers.genesis(TxHelpers.defaultSigner.toAddress, Constants.TotalWaves * Constants.UnitsInWave))

          val subscription = repo.createSubscription()

          d.appendBlock()
          d.appendBlock()

          subscription.cancel()
          val result = subscription.futureValue
          val balances = result.flatMap { se =>
            se.getUpdate.getAppend.body match {
              case Body.Block(value) => Some(value.updatedWavesAmount)
              case _                 => None
            }
          }

          balances shouldBe Seq(10000000000000000L, 10000000600000000L, 10000001200000000L)
        }
    )
  }

  def withRepo[T](f: (UpdatesRepoImpl, BlockchainUpdateTriggers) => T): T = {
    val repo = new UpdatesRepoImpl(Files.createTempDirectory("bc-updates").toString)
    val triggers = new BlockchainUpdateTriggers {
      override def onProcessBlock(
          block: Block,
          diff: BlockDiffer.DetailedDiff,
          minerReward: Option[Long],
          blockchainBeforeWithMinerReward: Blockchain
      ): Unit = {
        val newBlock = BlockAppended.from(block, diff, minerReward, blockchainBeforeWithMinerReward)
        repo.appendBlock(newBlock).get
      }

      def onProcessMicroBlock(
          microBlock: MicroBlock,
          diff: BlockDiffer.DetailedDiff,
          blockchainBeforeWithMinerReward: Blockchain,
          totalBlockId: ByteStr,
          totalTransactionsRoot: ByteStr
      ): Unit                                                         = ()
      def onRollback(toBlockId: ByteStr, toHeight: Int): Unit         = ()
      def onMicroBlockRollback(toBlockId: ByteStr, height: Int): Unit = ()
    }
    try f(repo, triggers)
    finally repo.shutdown()
  }

  def createFakeObserver[T](): (StreamObserver[T], CancelableFuture[Seq[T]]) = {
    val promise = Promise[Seq[T]]()
    val obs: StreamObserver[T] = new StreamObserver[T] {
      @volatile private[this] var values = Seq.empty[T]

      def onNext(value: T): Unit      = values :+= value
      def onError(t: Throwable): Unit = promise.tryFailure(t)
      def onCompleted(): Unit         = promise.trySuccess(values)
    }

    val cancelableFuture = CancelableFuture(promise.future, () => obs.onCompleted())
    (obs, cancelableFuture)
  }

  implicit class UpdatesRepoExt(ur: UpdatesRepoImpl) {
    def createSubscription(): CancelableFuture[Seq[SubscribeEvent]] = {
      val (obs, future) = createFakeObserver[SubscribeEvent]()
      ur.subscribe(SubscribeRequest(1, Int.MaxValue), obs)
      future
    }
  }

  implicit def asGrpcService(updatesRepoImpl: UpdatesRepoImpl): BlockchainUpdatesApiGrpcImpl =
    new BlockchainUpdatesApiGrpcImpl(updatesRepoImpl)
}
