package com.wavesplatform.events

import java.nio.file.Files

import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.StateUpdate.{AssetStateUpdate, BalanceUpdate, DataEntryUpdate, LeaseUpdate, LeasingBalanceUpdate}
import com.wavesplatform.events.api.grpc.BlockchainUpdatesApiGrpcImpl
import com.wavesplatform.events.api.grpc.protobuf.{SubscribeEvent, SubscribeRequest}
import com.wavesplatform.events.repo.UpdatesRepoImpl
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.{Constants, TestFunctionalitySettings}
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.state.{AssetDescription, Blockchain, EmptyDataEntry, Height, LeaseBalance, StringDataEntry}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers
import io.grpc.StatusException
import io.grpc.stub.{CallStreamObserver, StreamObserver}
import monix.execution.CancelableFuture
import monix.execution.Scheduler.Implicits.global
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.Promise
import scala.concurrent.duration._

class BlockchainUpdatesSpec extends FreeSpec with Matchers with WithDomain with ScalaFutures with PathMockFactory {
  override implicit val patienceConfig: PatienceConfig = PatienceConfig(5 seconds, 100 millis)

  "gRPC API" - {
    "return valid errors" in withRepo() { (repo, _) =>
      val (obs, result) = createFakeObserver[SubscribeEvent]()
      repo.subscribe(SubscribeRequest(999, 999), obs)
      result.failed.futureValue should matchPattern {
        case se: StatusException if se.getMessage.contains("Requested start height exceeds current blockchain height") =>
      }
    }
  }

  "BlockchainUpdates" - {
    "should include correct waves amount" in withNEmptyBlocksSubscription() { result =>
      val balances = result.map(_.getAppend.getBlock.updatedWavesAmount)
      balances shouldBe Seq(10000000000000000L, 10000000600000000L, 10000001200000000L)
    }

    "should include correct heights" in withNEmptyBlocksSubscription() { result =>
      val heights = result.map(_.height)
      heights shouldBe Seq(1, 2, 3)
    }

    "should handle toHeight=0" in withNEmptyBlocksSubscription(request = SubscribeRequest.of(1, 0)) { result =>
      result should have size 3
    }

    "should handle rollback properly" in {
      val transfer = TxHelpers.transfer()
      val lease    = TxHelpers.lease()
      val issue    = TxHelpers.issue()
      val reissue  = TxHelpers.reissue(issue.asset)
      val data     = TxHelpers.data()

      val description = AssetDescription(
        issue.assetId,
        null,
        issue.name,
        issue.description,
        issue.decimals,
        issue.reissuable,
        issue.quantity + reissue.quantity,
        Height @@ 1,
        None,
        0L,
        nft = false
      )

      withGenerateSubscription() { d =>
        d.appendKeyBlock()
        d.appendMicroBlock(transfer, lease, issue, reissue, data)
        d.appendKeyBlock()
        d.rollbackTo(1)
      } { events =>
        import com.wavesplatform.events.protobuf.serde._
        val rollback: RollbackResult = events.last.vanilla.get.asInstanceOf[RollbackCompleted].rollbackResult
        rollback.removedTransactionIds shouldBe Seq(data, reissue, issue, lease, transfer).map(_.id())
        rollback.removedBlocks should have length 1

        rollback.stateUpdate.balances shouldBe Seq(
          BalanceUpdate(TxHelpers.defaultAddress, Waves, 10000001036400000L, after = 10000000600000000L),
          BalanceUpdate(TxHelpers.defaultAddress, issue.asset, 2000, after = 0),
          BalanceUpdate(TxHelpers.secondAddress, Waves, 100000000, after = 0)
        )

        rollback.stateUpdate.leasingForAddress shouldBe Seq(
          LeasingBalanceUpdate(TxHelpers.secondAddress, LeaseBalance(1000000000, 0), LeaseBalance(0, 0)),
          LeasingBalanceUpdate(TxHelpers.defaultAddress, LeaseBalance(0, 1000000000), LeaseBalance(0, 0))
        )

        rollback.stateUpdate.leases shouldBe Seq(
          LeaseUpdate(lease.id(), LeaseStatus.Inactive, lease.amount, lease.sender, lease.recipient.asInstanceOf[Address], lease.id())
        )

        rollback.stateUpdate.dataEntries shouldBe Seq(
          DataEntryUpdate(TxHelpers.defaultAddress, StringDataEntry("test", "test"), EmptyDataEntry("test"))
        )

        rollback.stateUpdate.assets shouldBe Seq(
          AssetStateUpdate(Some(description), None)
        )
      }

      withGenerateSubscription() { d =>
        d.appendKeyBlock()
        d.appendMicroBlock(TxHelpers.transfer())
        d.appendMicroBlock(transfer, lease, issue, reissue, data)
        d.rollbackMicros()
      } { events =>
        import com.wavesplatform.events.protobuf.serde._
        val rollback: RollbackResult = events.last.vanilla.get.asInstanceOf[MicroBlockRollbackCompleted].rollbackResult
        rollback.removedTransactionIds shouldBe Seq(data, reissue, issue, lease, transfer).map(_.id())
        rollback.removedBlocks shouldBe empty

        rollback.stateUpdate.balances shouldBe Seq(
          BalanceUpdate(TxHelpers.defaultAddress, Waves, 10000000935800000L, after = 10000001099400000L),
          BalanceUpdate(TxHelpers.defaultAddress, issue.asset, 2000, after = 0),
          BalanceUpdate(TxHelpers.secondAddress, Waves, 200000000, after = 100000000)
        )

        rollback.stateUpdate.leasingForAddress shouldBe Seq(
          LeasingBalanceUpdate(TxHelpers.secondAddress, LeaseBalance(1000000000, 0), LeaseBalance(0, 0)),
          LeasingBalanceUpdate(TxHelpers.defaultAddress, LeaseBalance(0, 1000000000), LeaseBalance(0, 0))
        )

        rollback.stateUpdate.leases shouldBe Seq(
          LeaseUpdate(lease.id(), LeaseStatus.Inactive, lease.amount, lease.sender, lease.recipient.asInstanceOf[Address], lease.id())
        )

        rollback.stateUpdate.dataEntries shouldBe Seq(
          DataEntryUpdate(TxHelpers.defaultAddress, StringDataEntry("test", "test"), EmptyDataEntry("test"))
        )

        rollback.stateUpdate.assets shouldBe Seq(
          AssetStateUpdate(Some(description), None)
        )
      }
    }
  }

  def withGenerateSubscription(request: SubscribeRequest = SubscribeRequest.of(1, Int.MaxValue))(generateBlocks: Domain => Unit)(
      f: Seq[protobuf.BlockchainUpdated] => Unit
  ): Unit = {

    val fs = TestFunctionalitySettings.withFeatures(
      BlockchainFeatures.BlockReward,
      BlockchainFeatures.NG,
      BlockchainFeatures.SmartAccounts,
      BlockchainFeatures.DataTransaction,
      BlockchainFeatures.FeeSponsorship
    )

    withDomain(domainSettingsWithFS(fs)) { d =>
      withRepo(d.blocksApi) { (repo, updateRepoTrigger) =>
        d.triggers = Seq(updateRepoTrigger)
        d.appendBlock(TxHelpers.genesis(TxHelpers.defaultSigner.toAddress, Constants.TotalWaves * Constants.UnitsInWave))

        val subscription = repo.createSubscription(request)
        generateBlocks(d)
        Thread.sleep(1000)
        subscription.cancel()

        val result = subscription.futureValue
        f(result.map(_.getUpdate))
      }

    }
  }

  def withNEmptyBlocksSubscription(count: Int = 2, request: SubscribeRequest = SubscribeRequest.of(1, Int.MaxValue))(
      f: Seq[protobuf.BlockchainUpdated] => Unit
  ): Unit = withGenerateSubscription(request)(d => for (_ <- 1 to count) d.appendBlock())(f)

  def withRepo[T](blocksApi: CommonBlocksApi = stub[CommonBlocksApi])(f: (UpdatesRepoImpl, BlockchainUpdateTriggers) => T): T = {
    val repo = new UpdatesRepoImpl(Files.createTempDirectory("bc-updates").toString, blocksApi)
    val triggers = new BlockchainUpdateTriggers {
      override def onProcessBlock(
          block: Block,
          diff: BlockDiffer.DetailedDiff,
          minerReward: Option[Long],
          blockchainBeforeWithMinerReward: Blockchain
      ): Unit = {
        val newBlock = BlockAppended.from(block, diff, blockchainBeforeWithMinerReward)
        repo.appendBlock(newBlock).get
      }

      def onProcessMicroBlock(
          microBlock: MicroBlock,
          diff: BlockDiffer.DetailedDiff,
          blockchainBeforeWithMinerReward: Blockchain,
          totalBlockId: ByteStr,
          totalTransactionsRoot: ByteStr
      ): Unit = {
        val newMicroBlock = MicroBlockAppended.from(microBlock, diff, blockchainBeforeWithMinerReward, totalBlockId, totalTransactionsRoot)
        repo.appendMicroBlock(newMicroBlock)
      }

      def onRollback(toBlockId: ByteStr, toHeight: Int): Unit         = repo.rollback(toBlockId, toHeight).get
      def onMicroBlockRollback(toBlockId: ByteStr, height: Int): Unit = repo.rollbackMicroBlock(toBlockId).get
    }
    try f(repo, triggers)
    finally repo.shutdown()
  }

  def createFakeObserver[T](): (StreamObserver[T], CancelableFuture[Seq[T]]) = {
    val promise = Promise[Seq[T]]()
    val obs: StreamObserver[T] = new CallStreamObserver[T] {
      @volatile private[this] var values = Seq.empty[T]

      override def isReady: Boolean                                  = true
      override def setOnReadyHandler(onReadyHandler: Runnable): Unit = ()
      override def disableAutoInboundFlowControl(): Unit             = ()
      override def request(count: Int): Unit                         = ()
      override def setMessageCompression(enable: Boolean): Unit      = ()

      def onNext(value: T): Unit      = values :+= value
      def onError(t: Throwable): Unit = promise.tryFailure(t)
      def onCompleted(): Unit         = promise.trySuccess(values)
    }

    val cancelableFuture = CancelableFuture(promise.future, () => obs.onCompleted())
    (obs, cancelableFuture)
  }

  implicit class UpdatesRepoExt(ur: UpdatesRepoImpl) {
    def createSubscription(request: SubscribeRequest): CancelableFuture[Seq[SubscribeEvent]] = {
      val (obs, future) = createFakeObserver[SubscribeEvent]()
      ur.subscribe(request, obs)
      future
    }
  }

  implicit def asGrpcService(updatesRepoImpl: UpdatesRepoImpl): BlockchainUpdatesApiGrpcImpl =
    new BlockchainUpdatesApiGrpcImpl(updatesRepoImpl)
}
