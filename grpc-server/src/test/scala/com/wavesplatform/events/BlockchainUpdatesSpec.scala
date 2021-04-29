package com.wavesplatform.events

import java.nio.file.Files

import scala.concurrent.{Await, Promise}
import scala.concurrent.duration._
import scala.reflect.ClassTag

import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.events.api.grpc.BlockchainUpdatesApiGrpcImpl
import com.wavesplatform.events.api.grpc.protobuf.{GetBlockUpdatesRangeRequest, SubscribeEvent, SubscribeRequest}
import com.wavesplatform.events.protobuf.serde._
import com.wavesplatform.events.repo.UpdatesRepoImpl
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.{Constants, FunctionalitySettings, TestFunctionalitySettings, WavesSettings}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.transaction.TxHelpers
import io.grpc.{StatusException, StatusRuntimeException}
import io.grpc.stub.{CallStreamObserver, StreamObserver}
import monix.execution.CancelableFuture
import monix.execution.Scheduler.Implicits.global
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.concurrent.ScalaFutures

class BlockchainUpdatesSpec extends FreeSpec with Matchers with WithDomain with ScalaFutures with PathMockFactory {
  var currentSettings: WavesSettings = domainSettingsWithFS(
    TestFunctionalitySettings.withFeatures(
      BlockchainFeatures.BlockReward,
      BlockchainFeatures.NG,
      BlockchainFeatures.SmartAccounts,
      BlockchainFeatures.DataTransaction,
      BlockchainFeatures.FeeSponsorship
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

    "should include correct waves amount" in withNEmptyBlocksSubscription() { result =>
      val balances = result.collect { case b: BlockAppended => b.updatedWavesAmount }
      balances shouldBe Seq(10000000000000000L, 10000000600000000L, 10000001200000000L)
    }

    "should include correct heights" in withNEmptyBlocksSubscription() { result =>
      val heights = result.map(_.toHeight)
      heights shouldBe Seq(1, 2, 3)
    }

    "should handle toHeight=0" in withNEmptyBlocksSubscription(request = SubscribeRequest.of(1, 0)) { result =>
      result should have size 3
    }

    "should handle stream from height 1" in {
      withNEmptyBlocksSubscription(99, SubscribeRequest(1, 60)) { updates =>
        updates.map(_.toHeight) shouldBe (1 to 60)
      }

      withNEmptyBlocksSubscription(99, SubscribeRequest(1, 70)) { updates =>
        updates.map(_.toHeight) shouldBe (1 to 70)
      }

      withNEmptyBlocksSubscription(99, SubscribeRequest(1, 110)) { updates =>
        updates.map(_.toHeight) shouldBe (1 to 100)
      }
    }

    "should handle genesis and payment" in pending

    "should fail stream with invalid range" in {
      intercept[StatusException](withNEmptyBlocksSubscription(99, SubscribeRequest(0, 60))(_ => ()))
      intercept[StatusException](withNEmptyBlocksSubscription(99, SubscribeRequest(-1, 60))(_ => ()))
      intercept[StatusException](withNEmptyBlocksSubscription(99, SubscribeRequest(300, 60))(_ => ()))
    }

    "should return issued assets" in pending

    "should handle rollback properly" in pending

    "should skip rollback in real time updates" in {
      withDomainAndRepo { (d, repo) =>
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
    }

    "should get valid range" in withDomainAndRepo { (d, repo) =>
      for (_ <- 1 to 10) d.appendBlock()
      val blocks = repo.getBlockUpdatesRange(GetBlockUpdatesRangeRequest(3, 5)).futureValue.updates
      blocks.map(_.height) shouldBe Seq(3, 4, 5)
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
        val newBlock = BlockAppended.from(block, diff, minerReward, blockchainBeforeWithMinerReward)
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
        repo.appendMicroBlock(newMicroBlock).get
      }

      def onRollback(toBlockId: ByteStr, toHeight: Int): Unit = repo.rollback(toBlockId, toHeight).get

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

  // Matchers
  private[this] object E {
    class EventMatcher[T: ClassTag] {
      def unapply(bu: BlockchainUpdated): Option[(Int, ByteStr)] = bu match {
        case ba if implicitly[ClassTag[T]].runtimeClass.isInstance(bu) => Some((ba.toHeight, ba.toId))
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
