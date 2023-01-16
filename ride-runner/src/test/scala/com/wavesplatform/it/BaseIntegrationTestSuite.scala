package com.wavesplatform.it

import cats.syntax.option.*
import com.google.common.primitives.Ints
import com.google.protobuf.{ByteString, UnsafeByteOperations}
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.blockchain.{BlockchainProcessor, BlockchainState, RestApiScript, SharedBlockchainData}
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.events.protobuf.{BlockchainUpdated, StateUpdate}
import com.wavesplatform.api.DefaultBlockchainApi.*
import com.wavesplatform.api.{DefaultBlockchainApi, HasGrpc}
import com.wavesplatform.lang.API
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf.block.Block
import com.wavesplatform.protobuf.transaction.DataTransactionData
import com.wavesplatform.state.{DataEntry, Height, IntegerDataEntry}
import com.wavesplatform.storage.HasLevelDb.TestDb
import com.wavesplatform.storage.RequestsStorage.RequestKey
import com.wavesplatform.storage.persistent.LevelDbPersistentCaches
import com.wavesplatform.storage.{HasLevelDb, RequestsStorage}
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{BaseTestSuite, HasMonixHelpers}
import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import play.api.libs.json.{JsObject, Json}

import java.nio.charset.StandardCharsets
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Using

abstract class BaseIntegrationTestSuite extends BaseTestSuite with HasGrpc with HasLevelDb with HasMonixHelpers {
  protected val miner     = Wallet.generateNewAccount("miner".getBytes(StandardCharsets.UTF_8), 0)
  protected val alice     = Wallet.generateNewAccount("alice".getBytes(StandardCharsets.UTF_8), 0)
  protected val aliceAddr = alice.toAddress(chainId)

  protected val initX = 0

  /** @param xGt0
    *   An expected script result, x > 0? The initial value is 0
    */
  protected def test(events: List[WrappedEvent[SubscribeEvent]], xGt0: Boolean): Unit = Using.Manager { use =>
    implicit val testScheduler = TestScheduler()

    val blockchainApi = new TestBlockchainApi() {
      override def getCurrentBlockchainHeight(): Int = 2

      override def getBlockHeader(height: Int): Option[SignedBlockHeader] = toVanilla(mkPbBlock(height)).some

      override def getActivatedFeatures(height: Int): Map[Short, Int] = blockchainSettings.functionalitySettings.preActivatedFeatures

      override def getAccountScript(address: Address): Option[Script] =
        if (address == aliceAddr) mkScript().some
        else super.getAccountScript(address)

      override def getAccountDataEntry(address: Address, key: String): Option[DataEntry[?]] =
        if (address == aliceAddr && key == "x") IntegerDataEntry("x", initX).some
        else super.getAccountDataEntry(address, key)
    }

    val testDb   = use(TestDb.mk())
    val dbCaches = new LevelDbPersistentCaches(testDb.db)
    val blockchainStorage = new SharedBlockchainData[RequestKey](
      blockchainSettings,
      dbCaches,
      blockchainApi
    )

    val request = aliceAddr -> Json.obj("expr" -> "foo()")
    val processor = new BlockchainProcessor(
      BlockchainProcessor.Settings(
        enableTraces = false,
        evaluateScriptComplexityLimit = 52000,
        maxTxErrorLogSize = 0
      ),
      blockchainStorage,
      new RequestsStorage {
        override def all(): List[(Address, JsObject)] = List(request)

        override def append(x: (Address, JsObject)): Unit = {} // Ignore, because no way to evaluate a new expr
      },
      testScheduler
    )

    processor.runScripts(forceAll = true).runToFuture
    testScheduler.tick(1.milli) // 1 millisecond to detect that script will be ran in the end

    def getScriptResult = Await.result(processor.getCachedResultOrRun(request._1, request._2).runToFuture, 5.seconds)

    val before = getScriptResult

    val blockchainUpdatesStream = use(blockchainApi.mkBlockchainUpdatesStream(testScheduler))

    val workingHeight = Height(1)
    val eventsStream = blockchainUpdatesStream.downstream
      .doOnError(e =>
        Task {
          log.error("Error!", e)
        }
      )
      .take(events.size)
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
    events.foreach(blockchainApi.blockchainUpdatesUpstream.onNext)

    testScheduler.tick()

    withClue(dumpedTasks) {
      testScheduler.state.tasks shouldBe empty
    }
    log.info(s"The last result: ${Await.result(eventsStream, 5.seconds)}")

    val after = getScriptResult
    withClue("was refreshed:") {
      val runTimeBefore = (before \ RestApiScript.LastUpdatedKey).as[Long]
      val runTimeAfter  = (after \ RestApiScript.LastUpdatedKey).as[Long]
      runTimeAfter should be > runTimeBefore
    }

    withClue(Json.prettyPrint(after)) {
      (after \ "result" \ "value" \ "_2" \ "value").as[Boolean] shouldBe xGt0
    }
  }.get

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

  protected def mkRollbackEvent(
      height: Int,
      forkNumber: Int,
      microBlockNumber: Int = 0,
      dataEntryUpdates: List[StateUpdate.DataEntryUpdate] = Nil
  ): SubscribeEvent = SubscribeEvent().withUpdate(
    BlockchainUpdated()
      .withId(toByteString32(forkNumber, height, microBlockNumber))
      .withHeight(height)
      .withUpdate(
        BlockchainUpdated.Update.Rollback(
          BlockchainUpdated
            .Rollback()
            .withRollbackStateUpdate(StateUpdate.defaultInstance.withDataEntries(dataEntryUpdates))
        )
      )
  )

  protected def mkMicroBlockAppendEvent(
      height: Int,
      forkNumber: Int,
      microBlockNumber: Int,
      dataEntryUpdates: List[StateUpdate.DataEntryUpdate] = Nil
  ): SubscribeEvent = SubscribeEvent().withUpdate(
    BlockchainUpdated()
      .withId(toByteString32(forkNumber, height, microBlockNumber))
      .withHeight(height)
      .withUpdate(
        BlockchainUpdated.Update.Append(
          BlockchainUpdated
            .Append()
            .withMicroBlock(BlockchainUpdated.Append.MicroBlockAppend())
            .withStateUpdate(StateUpdate.defaultInstance.withDataEntries(dataEntryUpdates))
        )
      )
  )

  protected def mkBlockAppendEvent(height: Int, forkNumber: Int, dataEntryUpdates: List[StateUpdate.DataEntryUpdate] = Nil): SubscribeEvent =
    SubscribeEvent().withUpdate(
      BlockchainUpdated()
        .withHeight(height)
        .withId(toByteString32(forkNumber, height))
        .withUpdate(
          BlockchainUpdated.Update.Append(
            BlockchainUpdated
              .Append()
              .withBlock(BlockchainUpdated.Append.BlockAppend().withBlock(mkPbBlock(height)))
              .withStateUpdate(StateUpdate.defaultInstance.withDataEntries(dataEntryUpdates))
          )
        )
    )

  protected def mkDataEntryUpdate(address: Address, key: String, before: Long, after: Long): StateUpdate.DataEntryUpdate =
    StateUpdate.DataEntryUpdate(
      address = DefaultBlockchainApi.toPb(address),
      dataEntry = DataTransactionData.DataEntry(key, DataTransactionData.DataEntry.Value.IntValue(after)).some,
      dataEntryBefore = DataTransactionData.DataEntry(key, DataTransactionData.DataEntry.Value.IntValue(before)).some
    )

  private def mkPbBlock(height: Int) =
    Block.defaultInstance.withHeader(Block.Header.defaultInstance.withGenerator(toPb(miner.publicKey)).withTimestamp(height))

  private def toPb(pk: PublicKey): ByteString = UnsafeByteOperations.unsafeWrap(pk.arr)

  private def toByteString32(xs: Int*): ByteString = {
    require(xs.size < 4)
    UnsafeByteOperations.unsafeWrap(Array.concat(xs.map(Ints.toByteArray)*).padTo(32, 0.toByte))
  }
}
