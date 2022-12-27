package com.wavesplatform.it

import cats.syntax.option.*
import com.google.common.primitives.Ints
import com.google.protobuf.{ByteString, UnsafeByteOperations}
import com.wavesplatform.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.blockchain.{BlockchainProcessor, BlockchainState, SharedBlockchainData}
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.events.api.grpc.protobuf.*
import com.wavesplatform.events.protobuf.{BlockchainUpdated, StateUpdate}
import com.wavesplatform.grpc.DefaultBlockchainApi
import com.wavesplatform.grpc.DefaultBlockchainApi.*
import com.wavesplatform.history.DefaultBlockchainSettings
import com.wavesplatform.lang.API
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf.block.Block
import com.wavesplatform.protobuf.transaction.DataTransactionData
import com.wavesplatform.state.{DataEntry, Height, IntegerDataEntry}
import com.wavesplatform.storage.HasLevelDb.TestDb
import com.wavesplatform.storage.RequestsStorage
import com.wavesplatform.storage.RequestsStorage.RequestKey
import com.wavesplatform.storage.persistent.LevelDbPersistentCaches
import com.wavesplatform.wallet.Wallet
import monix.eval.Task
import monix.execution.exceptions.UpstreamTimeoutException
import monix.execution.schedulers.TestScheduler
import play.api.libs.json.{JsObject, Json}

import java.nio.charset.StandardCharsets
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Using

class EventsWithTimeoutIntegrationTestSuite extends BaseIntegrationTestSuite {
  private val settings = DefaultBlockchainSettings
  private val chainId  = settings.addressSchemeCharacter.toByte

  // TODO
  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = settings.addressSchemeCharacter.toByte
  }

  private val miner     = Wallet.generateNewAccount("miner".getBytes(StandardCharsets.UTF_8), 0)
  private val alice     = Wallet.generateNewAccount("alice".getBytes(StandardCharsets.UTF_8), 0)
  private val aliceAddr = alice.toAddress(chainId)

  private def toPb(pk: PublicKey): ByteString = UnsafeByteOperations.unsafeWrap(pk.arr)

  "a transaction received after a timeout" - {
    val initX = 0

    /** @param xGt0
      *   An expected script result, x > 0?
      */
    def test(expectedEvents: List[WrappedEvent[SubscribeEvent]], xGt0: Boolean): Unit = Using.Manager { use =>
      implicit val testScheduler = TestScheduler()

      val blockchainApi = new TestBlockchainApi() {
        override def getCurrentBlockchainHeight(): Int = 2

        override def getBlockHeader(height: Int): Option[SignedBlockHeader] = toVanilla(mkPbBlock(height)).some

        override def getActivatedFeatures(height: Int): Map[Short, Int] = settings.functionalitySettings.preActivatedFeatures

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
        settings,
        dbCaches,
        blockchainApi
      )

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
          override def all(): List[(Address, JsObject)] = List(request)

          override def append(x: (Address, JsObject)): Unit = {} // Ignore, because no way to evaluate a new expr
        },
        testScheduler
      )

      processor.runScripts(forceAll = true).runToFuture
      testScheduler.tick()

      val blockchainUpdatesStream = use(blockchainApi.mkBlockchainUpdatesStream(testScheduler))

      val workingHeight = Height(1)
      val events = blockchainUpdatesStream.downstream
        .doOnError(e =>
          Task {
            log.error("Error!", e)
          }
        )
        .take(expectedEvents.size)
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
      expectedEvents.foreach(blockchainApi.blockchainUpdatesUpstream.onNext)

      testScheduler.tick()

      withClue(dumpedTasks) {
        testScheduler.state.tasks shouldBe empty
      }
      Await.result(events, 5.seconds)

      val newResultTask = processor.getCachedResultOrRun(request._1, request._2).runToFuture
      val r             = Await.result(newResultTask, 5.seconds)
      withClue(Json.prettyPrint(r)) {
        (r \ "result" \ "value" \ "_2" \ "value").as[Boolean] shouldBe xGt0
      }
    }

    "block" in test(
      expectedEvents = List(
        WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
        WrappedEvent.Next(
          mkBlockAppendEvent(
            height = 2,
            forkNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX, 1))
          )
        ),
        WrappedEvent.Failed(UpstreamTimeoutException(1.minute)),
        WrappedEvent.Next(
          mkBlockAppendEvent(
            height = 2,
            forkNumber = 2,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX, 1))
          )
        )
      ),
      xGt0 = true
    )

    "micro block" in test(
      expectedEvents = List(
        WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
        WrappedEvent.Next(mkBlockAppendEvent(2, 1)),
        WrappedEvent.Next(
          mkMicroBlockAppendEvent(
            height = 2,
            forkNumber = 1,
            microBlockNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX, 1))
          )
        ),
        WrappedEvent.Failed(UpstreamTimeoutException(1.minute)),
        WrappedEvent.Next(mkBlockAppendEvent(2, 2)),
        WrappedEvent.Next(mkBlockAppendEvent(3, 2)),
        WrappedEvent.Next(
          mkMicroBlockAppendEvent(
            height = 3,
            forkNumber = 2,
            microBlockNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX, 1))
          )
        )
      ),
      xGt0 = true
    )
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

  private def mkMicroBlockAppendEvent(
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

  private def mkBlockAppendEvent(height: Int, forkNumber: Int, dataEntryUpdates: List[StateUpdate.DataEntryUpdate] = Nil): SubscribeEvent =
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

  private def mkDataEntryUpdate(address: Address, key: String, before: Long, after: Long): StateUpdate.DataEntryUpdate =
    StateUpdate.DataEntryUpdate(
      address = DefaultBlockchainApi.toPb(address),
      dataEntry = DataTransactionData.DataEntry(key, DataTransactionData.DataEntry.Value.IntValue(after)).some,
      dataEntryBefore = DataTransactionData.DataEntry(key, DataTransactionData.DataEntry.Value.IntValue(before)).some
    )

  private def mkPbBlock(height: Int) =
    Block.defaultInstance.withHeader(Block.Header.defaultInstance.withGenerator(toPb(miner.publicKey)).withTimestamp(height))

  private def toByteString32(xs: Int*): ByteString = {
    require(xs.size < 4)
    UnsafeByteOperations.unsafeWrap(Array.concat(xs.map(Ints.toByteArray)*).padTo(32, 0.toByte))
  }
}
