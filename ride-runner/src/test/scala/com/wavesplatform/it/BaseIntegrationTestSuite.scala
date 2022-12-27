package com.wavesplatform.it

import akka.actor.ActorSystem
import akka.testkit.TestKit
import cats.syntax.option.*
import com.google.common.primitives.Ints
import com.google.protobuf.{ByteString, UnsafeByteOperations}
import com.wavesplatform.account.{Address, AddressScheme, Alias, PublicKey}
import com.wavesplatform.api.grpc.BalanceResponse
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.blockchain.{BlockchainProcessor, BlockchainState, SharedBlockchainData}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.events.api.grpc.protobuf.*
import com.wavesplatform.events.protobuf.{BlockchainUpdated, StateUpdate}
import com.wavesplatform.grpc.DefaultBlockchainApi.*
import com.wavesplatform.grpc.{BlockchainApi, DefaultBlockchainApi, HasGrpc}
import com.wavesplatform.history.DefaultBlockchainSettings
import com.wavesplatform.lang.API
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf.block.Block
import com.wavesplatform.protobuf.transaction.DataTransactionData
import com.wavesplatform.ride.app.{Cleanup, CustomShutdownPhase}
import com.wavesplatform.state.{AssetDescription, DataEntry, Height, IntegerDataEntry}
import com.wavesplatform.storage.HasLevelDb.TestDb
import com.wavesplatform.storage.RequestsStorage.RequestKey
import com.wavesplatform.storage.persistent.LevelDbPersistentCaches
import com.wavesplatform.storage.{HasLevelDb, RequestsStorage}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{BaseTestSuite, HasMonixHelpers}
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.exceptions.UpstreamTimeoutException
import monix.execution.schedulers.TestScheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import play.api.libs.json.{JsObject, Json}

import java.nio.charset.StandardCharsets
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

abstract class BaseIntegrationTestSuite extends TestKit(ActorSystem()) with BaseTestSuite with HasGrpc with HasLevelDb with HasMonixHelpers {
  override protected def afterAll(): Unit = TestKit.shutdownActorSystem(system)
}

class TestTestSuite extends BaseIntegrationTestSuite {
  private val settings = DefaultBlockchainSettings

  // TODO
  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = settings.addressSchemeCharacter.toByte
  }

  private val miner     = Wallet.generateNewAccount("miner".getBytes(StandardCharsets.UTF_8), 0)
  private val alice     = Wallet.generateNewAccount("alice".getBytes(StandardCharsets.UTF_8), 0)
  private val aliceAddr = alice.toAddress(settings.addressSchemeCharacter.toByte)

  private def toPb(pk: PublicKey): ByteString = UnsafeByteOperations.unsafeWrap(pk.arr)

  "a transaction restored" in {
    implicit val testScheduler = TestScheduler()
    val cs                     = new Cleanup(system)

    val blockchainApi = new TestBlockchainApi() {
      override def getBlockHeader(height: Int): Option[SignedBlockHeader] = toVanilla(mkPbBlock(height)).some

      override def getActivatedFeatures(height: Int): Map[Short, Int] =
        settings.functionalitySettings.preActivatedFeatures

      override def getAccountScript(address: Address): Option[Script] =
        if (address == aliceAddr) mkScript().some
        else super.getAccountScript(address)

      override def getAccountDataEntry(address: Address, key: String): Option[DataEntry[_]] =
        if (address == aliceAddr && key == "x") IntegerDataEntry("x", 0).some
        else super.getAccountDataEntry(address, key)
    }

    val testDb = TestDb.mk()
    cs.cleanup(CustomShutdownPhase.Db) {
      testDb.close()
    }
    val dbCaches = new LevelDbPersistentCaches(testDb.db)
    val blockchainStorage = new SharedBlockchainData[RequestKey](
      settings,
      dbCaches,
      blockchainApi
    )

    val lastHeightAtStart = Height(blockchainApi.getCurrentBlockchainHeight())
    log.info(s"Current height: known=${blockchainStorage.height}, blockchain=$lastHeightAtStart")

    val request: (Address, JsObject) = aliceAddr -> Json.obj(
      "expr" -> "foo()"
    )
    val processor = new BlockchainProcessor(
      BlockchainProcessor.Settings(
        enableTraces = false,
        evaluateScriptComplexityLimit = 52000,
        maxTxErrorLogSize = 0
      ),
      blockchainStorage,
      new RequestsStorage {
        override def all(): List[(Address, JsObject)]     = List(request)
        override def append(x: (Address, JsObject)): Unit = {} // Ignore, because no way to evaluate a new expr
      },
      testScheduler
    )

    log.info("Warming up caches...") // Helps to figure out, which data is used by a script
    processor.runScripts(forceAll = true).runToFuture
    testScheduler.tick()

    log.info(s"Watching blockchain updates...")
    val blockchainUpdatesStream = blockchainApi.mkBlockchainUpdatesStream(testScheduler)
    cs.cleanup(CustomShutdownPhase.BlockchainUpdatesStream) {
      blockchainUpdatesStream.close()
    }

    val workingHeight = Height(1)
    val events = blockchainUpdatesStream.downstream
      .doOnError(e =>
        Task {
          log.error("Error!", e)
        }
      )
      .take(4) // Block 1, Block 2, Timeout, Block 2_2
      .scanEval(Task.now[BlockchainState](BlockchainState.Starting(Height(0), workingHeight))) {
        BlockchainState(processor, blockchainUpdatesStream, _, _)
      }
      .doOnError { e =>
        Task {
          log.error("Got an unhandled error, closing streams. Contact with developers", e)
          blockchainUpdatesStream.close()
        }
      }
      .lastL
      .runToFuture

    blockchainUpdatesStream.start(1)

    log.info("Initialization completed")

    blockchainApi.blockchainUpdatesUpstream.onNext(WrappedEvent.Next(mkBlockAppendEvent(1, 1)))
    blockchainApi.blockchainUpdatesUpstream.onNext(WrappedEvent.Next(mkBlockAppendEvent(
      height = 2,
      forkNumber = 1,
      dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", 0, 1))
    )))
    blockchainApi.blockchainUpdatesUpstream.onNext(WrappedEvent.Failed(UpstreamTimeoutException(1.minute)))
    blockchainApi.blockchainUpdatesUpstream.onNext(WrappedEvent.Next(mkBlockAppendEvent(
      height = 2,
      forkNumber = 2,
      dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", 0, 1))
    )))

    testScheduler.tick()

    withClue(dumpedTasks) {
      testScheduler.state.tasks shouldBe empty
    }
    Await.result(events, 5.seconds)

    val newResultTask = processor.getCachedResultOrRun(request._1, request._2).runToFuture
    val r             = Await.result(newResultTask, 5.seconds)
    withClue(Json.prettyPrint(r)) {
      (r \ "result" \ "value" \ "_2" \ "value").as[Boolean] shouldBe true
    }

    log.info("Done")
    cs.forceStop()
  }

  private def mkScript(): Script = {
    val scriptSrc =
      s"""
{-#STDLIB_VERSION 6 #-}
{-#SCRIPT_TYPE ACCOUNT #-}
{-#CONTENT_TYPE DAPP #-}

@Callable(inv)
func foo() = {
  let alice = Address(base58'$aliceAddr')
  let x = getIntegerValue(alice, "x")
  ([], x > 0)
}
"""
    val estimator      = ScriptEstimatorV3(fixOverflow = true, overhead = false)
    val compiledScript = API.compile(input = scriptSrc, estimator).explicitGet()
    Script.fromBase64String(Base64.encode(compiledScript.bytes)).explicitGet()
  }

  private def mkRollbackEvent(height: Int): SubscribeEvent = SubscribeEvent().withUpdate(
    BlockchainUpdated()
      .withHeight(height)
      .withUpdate(
        BlockchainUpdated.Update.Rollback(
          BlockchainUpdated.Rollback()
        )
      )
  )

  private def mkMicroBlockAppendEvent(height: Int): SubscribeEvent = SubscribeEvent().withUpdate(
    BlockchainUpdated()
      .withHeight(height)
      .withUpdate(
        BlockchainUpdated.Update.Append(
          BlockchainUpdated
            .Append()
            .withMicroBlock(
              BlockchainUpdated.Append.MicroBlockAppend()
            )
        )
      )
  )

  // TODO with stateUpdate.dataEntries
  private def mkBlockAppendEvent(height: Int, forkNumber: Int, dataEntryUpdates: List[StateUpdate.DataEntryUpdate] = Nil): SubscribeEvent =
    SubscribeEvent().withUpdate(
      BlockchainUpdated()
        .withHeight(height)
        .withId(
          UnsafeByteOperations.unsafeWrap(
            Array
              .concat(
                Ints.toByteArray(forkNumber),
                Ints.toByteArray(height)
              )
              .padTo(32, 0.toByte)
          )
        )
        .withUpdate(
          BlockchainUpdated.Update.Append(
            BlockchainUpdated
              .Append()
              .withBlock(BlockchainUpdated.Append.BlockAppend().withBlock(mkPbBlock(height)))
              .withStateUpdate(StateUpdate.defaultInstance.withDataEntries(dataEntryUpdates))
          )
        )
    )

  private def mkDataEntryUpdate(address: Address, key: String, before: Long, after: Long): StateUpdate.DataEntryUpdate =
    StateUpdate.DataEntryUpdate(
      address = DefaultBlockchainApi.toPb(address),
      dataEntry = DataTransactionData.DataEntry(key, DataTransactionData.DataEntry.Value.IntValue(after)).some,
      dataEntryBefore = DataTransactionData.DataEntry(key, DataTransactionData.DataEntry.Value.IntValue(before)).some
    )

  private def mkPbBlock(height: Int) =
    Block.defaultInstance.withHeader(Block.Header.defaultInstance.withGenerator(toPb(miner.publicKey)).withTimestamp(height))
}

// TODO throw instead
class TestBlockchainApi(implicit val scheduler: Scheduler) extends BlockchainApi {

  val blockchainUpdatesUpstream = ConcurrentSubject.publish[WrappedEvent[SubscribeEvent]]

  override def mkBlockchainUpdatesStream(scheduler: Scheduler): BlockchainApi.BlockchainUpdatesStream =
    new BlockchainApi.BlockchainUpdatesStream {
      override val downstream: Observable[WrappedEvent[SubscribeEvent]] = blockchainUpdatesUpstream

      override def start(fromHeight: Int, toHeight: Int): Unit = {}

      override def closeUpstream(): Unit   = blockchainUpdatesUpstream.onComplete()
      override def closeDownstream(): Unit = {}

      override def close(): Unit = {
        closeUpstream()
        closeDownstream()
      }
    }

  override def getCurrentBlockchainHeight(): Int = 2

  private val allFeaturesActivated                                = (1 to 15).map { id => id.toShort -> 0 }.toMap
  override def getActivatedFeatures(height: Int): Map[Short, Int] = allFeaturesActivated

  override def getAccountDataEntries(address: Address): Seq[DataEntry[?]]                   = Nil
  override def getAccountDataEntry(address: Address, key: String): Option[DataEntry[?]]     = None
  override def getAccountScript(address: Address): Option[Script]                           = None
  override def getBlockHeader(height: Int): Option[SignedBlockHeader]                       = None
  override def getBlockHeaderRange(fromHeight: Int, toHeight: Int): List[SignedBlockHeader] = Nil
  override def getVrf(height: Int): Option[ByteStr]                                         = None
  override def getAssetDescription(asset: Asset.IssuedAsset): Option[AssetDescription]      = None
  override def resolveAlias(alias: Alias): Option[Address]                                  = None
  override def getBalance(address: Address, asset: Asset): Long                             = 0
  override def getLeaseBalance(address: Address): BalanceResponse.WavesBalances             = BalanceResponse.WavesBalances()
  override def getTransactionHeight(id: ByteStr): Option[Height]                            = None
}
