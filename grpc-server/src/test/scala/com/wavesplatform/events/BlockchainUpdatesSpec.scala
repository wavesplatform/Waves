package com.wavesplatform.events

import java.nio.file.Files

import com.google.common.primitives.Longs
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.db.WithDomain
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.StateUpdate.{AssetInfo, AssetStateUpdate, BalanceUpdate, DataEntryUpdate, LeaseUpdate, LeasingBalanceUpdate}
import com.wavesplatform.events.api.grpc.BlockchainUpdatesApiGrpcImpl
import com.wavesplatform.events.api.grpc.protobuf.{GetBlockUpdatesRangeRequest, SubscribeEvent, SubscribeRequest}
import com.wavesplatform.events.protobuf.serde._
import com.wavesplatform.events.repo.UpdatesRepoImpl
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.settings.{Constants, FunctionalitySettings, TestFunctionalitySettings, WavesSettings}
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.state.{AssetDescription, Blockchain, EmptyDataEntry, Height, LeaseBalance, StringDataEntry}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{Asset, GenesisTransaction, PaymentTransaction, TxHelpers}
import io.grpc.StatusException
import io.grpc.stub.{CallStreamObserver, StreamObserver}
import monix.execution.CancelableFuture
import monix.execution.Scheduler.Implicits.global
import org.scalactic.source.Position
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, Promise}
import scala.reflect.ClassTag
import scala.util.Random

class BlockchainUpdatesSpec extends FreeSpec with Matchers with WithDomain with ScalaFutures with PathMockFactory {
  var currentSettings: WavesSettings = domainSettingsWithFS(
    TestFunctionalitySettings.withFeatures(
      BlockchainFeatures.BlockReward,
      BlockchainFeatures.NG,
      BlockchainFeatures.SmartAccounts,
      BlockchainFeatures.DataTransaction,
      BlockchainFeatures.FeeSponsorship,
      BlockchainFeatures.BlockV5,
      BlockchainFeatures.SynchronousCalls
    )
  )

  def currentFS: FunctionalitySettings = currentSettings.blockchainSettings.functionalitySettings

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(10 seconds, 500 millis)

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
    "should not freeze on micro rollback" in withDomainAndRepo {
      case (d, repo) =>
        val keyBlockId = d.appendKeyBlock().id()
        d.appendMicroBlock(TxHelpers.transfer())
        d.appendMicroBlock(TxHelpers.transfer())

        val subscription = repo.createSubscription(SubscribeRequest.of(1, 0))
        Thread.sleep(1000)
        d.appendKeyBlock(Some(keyBlockId))

        Thread.sleep(1000)
        subscription.cancel()
        val events = subscription.futureValue.map(_.toUpdate)
        events should matchPattern {
          case Seq(
              E.Block(1, _),
              E.Micro(1, _),
              E.Micro(1, _),
              E.MicroRollback(1, `keyBlockId`),
              E.Block(2, _)
              ) =>
        }
    }

    "should not freeze on block rollback" in withDomainAndRepo {
      case (d, repo) =>
        val block1Id = d.appendKeyBlock().id()
        d.appendKeyBlock()

        val subscription = repo.createSubscription(SubscribeRequest.of(1, 0))
        Thread.sleep(1000)
        d.rollbackTo(block1Id)
        d.appendKeyBlock()

        Thread.sleep(1000)
        subscription.cancel()
        val events = subscription.futureValue.map(_.toUpdate)
        events should matchPattern {
          case Seq(
              E.Block(1, _),
              E.Block(2, _),
              E.Rollback(1, `block1Id`),
              E.Block(2, _)
              ) =>
        }
    }

    "should not duplicate blocks" in withDomainAndRepo {
      case (d, repo) =>
        for (_ <- 1 to 99) d.appendBlock()
        d.appendKeyBlock()
        d.appendMicroBlock(TxHelpers.transfer())
        d.appendKeyBlock()

        val events = {
          val sub = repo.createSubscription(SubscribeRequest.of(1, 0))
          Thread.sleep(1000)
          sub.cancel()
          sub.futureValue.map(_.toUpdate)
        }

        val lastEvents = events.dropWhile(_.height < 100)
        lastEvents should matchPattern {
          case Seq(
              E.Block(100, _),
              E.Block(101, _)
              ) =>
        }
    }

    "should not freeze on block rollback without key-block" in withDomainAndRepo {
      case (d, repo) =>
        val block1Id = d.appendBlock().id()
        val block2Id = d.appendBlock().id()
        d.appendBlock()
        d.rollbackTo(block2Id)

        val subscription = repo.createSubscription(SubscribeRequest.of(1, 0))
        Thread.sleep(1000)
        d.rollbackTo(block1Id)
        d.appendBlock()

        Thread.sleep(1000)
        subscription.cancel()
        val events = subscription.futureValue.map(_.toUpdate)
        events should matchPattern {
          case Seq(
              E.Block(1, _),
              E.Block(2, _),
              E.Block(3, _),
              E.Rollback(2, `block2Id`),
              E.Rollback(1, `block1Id`),
              E.Block(2, _)
              ) =>
        }
    }

    "should survive invalid rollback" in withDomain(
      SettingsFromDefaultConfig.copy(dbSettings = SettingsFromDefaultConfig.dbSettings.copy(maxRollbackDepth = 0))
    ) { d =>
      withRepo(d.blocksApi) { (repo, updateRepoTrigger) =>
        d.triggers = Seq(updateRepoTrigger)
        for (_ <- 1 to 10) d.appendBlock()
        intercept[RuntimeException](d.rollbackTo(1)) // Should fail
        repo.getBlockUpdatesRange(GetBlockUpdatesRangeRequest.of(1, 10)).futureValue.updates.map(_.height) shouldBe (1 to 10)
      }
    }

    "should survive invalid micro rollback" in withDomainAndRepo {
      case (d, repo) =>
        d.appendKeyBlock()
        val sub   = repo.createSubscription(SubscribeRequest(1))
        val mb1Id = d.appendMicroBlock(TxHelpers.transfer())
        val mb2Id = d.appendMicroBlock(TxHelpers.transfer())
        d.appendMicroBlock(TxHelpers.transfer())

        d.blockchain.removeAfter(mb1Id) // Should not do anything
        d.appendKeyBlock(ref = Some(mb2Id))

        sub.cancel()
        val result = sub.futureValue.map(_.toUpdate)
        result should matchPattern {
          case Seq(
              E.Block(1, _),
              E.Micro(1, _),
              E.Micro(1, _),
              E.Micro(1, _),
              E.MicroRollback(1, `mb2Id`),
              E.Block(2, _)
              ) =>
        }
    }

    "should include correct waves amount" in withNEmptyBlocksSubscription() { result =>
      val balances = result.collect { case b: BlockAppended => b.updatedWavesAmount }
      balances shouldBe Seq(10000000000000000L, 10000000600000000L, 10000001200000000L)
    }

    "should include correct heights" in withNEmptyBlocksSubscription() { result =>
      val heights = result.map(_.height)
      heights shouldBe Seq(1, 2, 3)
    }

    "should handle toHeight=0" in withNEmptyBlocksSubscription(request = SubscribeRequest.of(1, 0)) { result =>
      result should have size 3
    }

    "should handle stream from height 1" in {
      withNEmptyBlocksSubscription(99, SubscribeRequest(1, 60)) { updates =>
        updates.map(_.height) shouldBe (1 to 60)
      }

      withNEmptyBlocksSubscription(99, SubscribeRequest(1, 70)) { updates =>
        updates.map(_.height) shouldBe (1 to 70)
      }

      withNEmptyBlocksSubscription(99, SubscribeRequest(1, 110)) { updates =>
        updates.map(_.height) shouldBe (1 to 100)
      }
    }

    "should handle genesis and payment" in withFuncSettings(currentFS.copy(blockVersion3AfterHeight = 3))(withGenerateSubscription() { d =>
      val tx =
        PaymentTransaction.create(TxHelpers.defaultSigner, TxHelpers.secondAddress, 100, 100000, TxHelpers.timestamp).explicitGet()
      d.appendBlock(tx)
    } { results =>
      val reward        = 600000000
      val genesisAmount = Constants.TotalWaves * Constants.UnitsInWave + reward
      val genesis = results.head match {
        case bu: BlockAppended => bu.transactionStateUpdates.head.balances.head
        case _                 => ???
      }
      genesis.address shouldBe TxHelpers.defaultAddress
      genesis.after shouldBe genesisAmount
      genesis.before shouldBe reward
      genesis.asset shouldBe Waves

      val payment = results.last match {
        case bu: BlockAppended => bu.transactionStateUpdates.last.balances.last
        case _                 => ???
      }
      payment.address shouldBe TxHelpers.secondAddress
      payment.after shouldBe 100
      payment.before shouldBe 0
      payment.asset shouldBe Waves
    })

    "should fail stream with invalid range" in {
      intercept[StatusException](withNEmptyBlocksSubscription(99, SubscribeRequest(0, 60))(_ => ()))
      intercept[StatusException](withNEmptyBlocksSubscription(99, SubscribeRequest(-1, 60))(_ => ()))
      intercept[StatusException](withNEmptyBlocksSubscription(99, SubscribeRequest(300, 60))(_ => ()))
    }

    "should return issued assets" in {
      val issue = TxHelpers.issue()
      val description = AssetDescription(
        issue.assetId,
        issue.sender,
        issue.name,
        issue.description,
        issue.decimals,
        issue.reissuable,
        issue.quantity,
        Height @@ 2,
        None,
        0L,
        nft = false
      )

      withGenerateSubscription() { d =>
        d.appendBlock(issue)
      } { events =>
        val event  = events.last.asInstanceOf[BlockAppended]
        val issued = event.transactionStateUpdates.head.assets
        issued shouldBe Seq(AssetStateUpdate(issue.assetId, None, Some(description)))
        event.referencedAssets shouldBe Seq(AssetInfo(issue.assetId, description.decimals, description.name.toStringUtf8))
      }
    }

    "should handle rollback properly" in {
      val transfer = TxHelpers.transfer()
      val lease    = TxHelpers.lease()
      val issue    = TxHelpers.issue()
      val reissue  = TxHelpers.reissue(issue.asset)
      val data     = TxHelpers.data()

      val description = AssetDescription(
        issue.assetId,
        issue.sender,
        issue.name,
        issue.description,
        issue.decimals,
        issue.reissuable,
        issue.quantity + reissue.quantity,
        Height @@ 2,
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
        val rollback: RollbackResult = events.collectFirst { case r: RollbackCompleted => r.rollbackResult }.get
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
          AssetStateUpdate(issue.assetId, Some(description), None)
        )
      }

      withGenerateSubscription() { d =>
        d.appendKeyBlock()
        val firstMicroId = d.appendMicroBlock(TxHelpers.transfer())
        d.appendMicroBlock(transfer, lease, issue, reissue, data)
        d.appendKeyBlock(Some(firstMicroId))
      } { events =>
        val rollback: RollbackResult = events.collectFirst { case r: MicroBlockRollbackCompleted => r.rollbackResult }.get
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
          AssetStateUpdate(issue.assetId, Some(description), None)
        )
      }
    }

    "should skip rollback in real time updates" in withDomainAndRepo { (d, repo) =>
      d.appendKeyBlock()
      d.appendKeyBlock()
      d.rollbackTo(1)
      d.appendKeyBlock()
      d.appendKeyBlock()

      val subscription = repo.createSubscription(SubscribeRequest(1))
      Thread.sleep(1000)
      subscription.cancel()
      subscription.futureValue.map(_.getUpdate.height) shouldBe Seq(1, 2, 3)
    }

    "should get valid range" in withDomainAndRepo { (d, repo) =>
      for (_ <- 1 to 10) d.appendBlock()
      val blocks = repo.getBlockUpdatesRange(GetBlockUpdatesRangeRequest(3, 5)).futureValue.updates
      blocks.map(_.height) shouldBe Seq(3, 4, 5)
    }

    "should return correct ids for assets and leases from invoke" in withDomainAndRepo { (d, repo) =>
      val issuer        = KeyPair(Longs.toByteArray(Random.nextLong()))
      val invoker       = KeyPair(Longs.toByteArray(Random.nextLong()))
      val issuerAddress = issuer.toAddress
      val (dAppScript, _) = ScriptCompiler
        .compile(
          s"""
           |{-# STDLIB_VERSION 5 #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |{-# CONTENT_TYPE DAPP #-}
           |
           |@Callable(i)
           |func issue() = {
           |  let issue = Issue("name", "description", 1000, 4, true, unit, 0)
           |  let lease = Lease(i.caller, 500000000)
           |  [
           |    issue,
           |    BinaryEntry("assetId", calculateAssetId(issue)),
           |    lease,
           |    BinaryEntry("leaseId", calculateLeaseId(lease))
           |  ]
           |}
           |""".stripMargin,
          ScriptEstimatorV3
        )
        .explicitGet()
      val invoke = InvokeScriptTransaction
        .selfSigned(
          2.toByte,
          invoker,
          issuer.toAddress,
          Some(FUNCTION_CALL(FunctionHeader.User("issue"), Nil)),
          Seq.empty,
          2.waves,
          Asset.Waves,
          ntpTime.getTimestamp()
        )
        .explicitGet()
      d.appendBlock(
        GenesisTransaction.create(issuerAddress, 1000.waves, ntpTime.getTimestamp()).explicitGet(),
        GenesisTransaction.create(invoker.toAddress, 1000.waves, ntpTime.getTimestamp()).explicitGet(),
        SetScriptTransaction.selfSigned(2.toByte, issuer, Some(dAppScript), 0.005.waves, ntpTime.getTimestamp()).explicitGet(),
        invoke
      )

      val assetId = d.blockchain.binaryData(issuer.toAddress, "assetId").get
      val leaseId = d.blockchain.binaryData(issuerAddress, "leaseId").get

      def check()(implicit pos: Position): Unit = {
        val genesisUpdate = repo.updateForHeight(1).get
        genesisUpdate.referencedAssets.head.id shouldEqual assetId
        val leaseUpdate = genesisUpdate.transactionStateUpdates(3).leases.head
        leaseUpdate.originTransactionId shouldEqual invoke.id()
        leaseUpdate.leaseId shouldEqual leaseId
      }

      check()

      d.appendBlock()

      check()
    }
  }

  def withDomainAndRepo(f: (Domain, UpdatesRepoImpl) => Unit): Unit = {
    withDomain(currentSettings) { d =>
      withRepo(d.blocksApi) { (repo, updateRepoTrigger) =>
        d.triggers = Seq(updateRepoTrigger)
        f(d, repo)
      }
    }
  }

  def withGenerateSubscription(request: SubscribeRequest = SubscribeRequest.of(1, Int.MaxValue))(generateBlocks: Domain => Unit)(
      f: Seq[BlockchainUpdated] => Unit
  ): Unit = {
    withDomainAndRepo { (d, repo) =>
      d.appendBlock(TxHelpers.genesis(TxHelpers.defaultSigner.toAddress, Constants.TotalWaves * Constants.UnitsInWave))

      val subscription = repo.createSubscription(request)
      generateBlocks(d)
      Thread.sleep(1000)
      subscription.cancel()

      val result = Await.result(subscription, 20 seconds)
      f(result.map(_.toUpdate))
    }
  }

  def withFuncSettings(settings: FunctionalitySettings)(f: => Unit): Unit = {
    val oldSettings = currentSettings
    currentSettings = oldSettings.copy(blockchainSettings = oldSettings.blockchainSettings.copy(functionalitySettings = settings))
    f
    currentSettings = oldSettings
  }

  def withSettings(settings: WavesSettings)(f: => Unit): Unit = {
    val oldSettings = currentSettings
    currentSettings = settings
    f
    currentSettings = oldSettings
  }

  def withNEmptyBlocksSubscription(count: Int = 2, request: SubscribeRequest = SubscribeRequest.of(1, Int.MaxValue))(
      f: Seq[BlockchainUpdated] => Unit
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

      def onRollback(blockchainBefore: Blockchain, toBlockId: ByteStr, toHeight: Int): Unit = repo.rollback(blockchainBefore, toBlockId, toHeight).get
      def onMicroBlockRollback(blockchainBefore: Blockchain, toBlockId: ByteStr): Unit      = repo.rollbackMicroBlock(blockchainBefore, toBlockId).get
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

  // Matchers
  private[this] object E {
    class EventMatcher[T: ClassTag] {
      def unapply(bu: BlockchainUpdated): Option[(Int, ByteStr)] = bu match {
        case ba if implicitly[ClassTag[T]].runtimeClass.isInstance(bu) => Some((ba.height, ba.id))
        case _                                                         => None
      }
    }

    object Block         extends EventMatcher[BlockAppended]
    object Micro         extends EventMatcher[MicroBlockAppended]
    object Rollback      extends EventMatcher[RollbackCompleted]
    object MicroRollback extends EventMatcher[MicroBlockRollbackCompleted]
  }

  implicit class ProtoSubscribeEventOps(e: com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent) {
    def toUpdate: BlockchainUpdated = e.getUpdate.vanilla.get
  }
}
