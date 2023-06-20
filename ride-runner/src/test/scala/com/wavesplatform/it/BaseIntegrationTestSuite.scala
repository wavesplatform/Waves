package com.wavesplatform.it

import cats.syntax.option.*
import com.typesafe.config.ConfigMemorySize
import com.wavesplatform.account.Address
import com.wavesplatform.api.DefaultBlockchainApi.*
import com.wavesplatform.api.grpc.{BalanceResponse, BlockWithHeight}
import com.wavesplatform.api.{HasBasicGrpcConverters, HasGrpc, TestBlockchainApi}
import com.wavesplatform.blockchain.SignedBlockHeaderWithVrf
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.lang.script.Script
import com.wavesplatform.ride.ScriptUtil
import com.wavesplatform.ride.runner.requests.{DefaultRequestService, RideScriptRunRequest, TestJobScheduler}
import com.wavesplatform.ride.runner.storage.persistent.HasDb.TestDb
import com.wavesplatform.ride.runner.storage.persistent.{DefaultPersistentCaches, HasDb}
import com.wavesplatform.ride.runner.storage.{CommonCache, SharedBlockchainStorage}
import com.wavesplatform.ride.runner.{BlockchainProcessor, BlockchainState}
import com.wavesplatform.state.{DataEntry, Height, IntegerDataEntry}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.{BaseTestSuite, HasMonixHelpers}
import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import play.api.libs.json.{JsObject, Json}

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Using

abstract class BaseIntegrationTestSuite extends BaseTestSuite with HasGrpc with HasBasicGrpcConverters with HasDb with HasMonixHelpers {
  protected val initX = 0L

  /** @param xPlusHeight
    *   An expected script result, x > 0? The initial value is 0
    */
  protected def test(events: List[WrappedEvent[SubscribeEvent]], xPlusHeight: Int): Unit = Using.Manager { use =>
    implicit val testScheduler = TestScheduler()

    val blockchainApi = new TestBlockchainApi() {
      override def getCurrentBlockchainHeight(): Height = Height(1)

      override def getBlockHeader(height: Height): Option[SignedBlockHeaderWithVrf] =
        toVanilla(BlockWithHeight(mkPbBlock(height).some, height))

      override def getBlockHeaderRange(fromHeight: Height, toHeight: Height): List[SignedBlockHeaderWithVrf] = ???

      override def getActivatedFeatures(height: Height): Map[Short, Height] =
        blockchainSettings.functionalitySettings.preActivatedFeatures.view.mapValues(Height(_)).toMap

      override def getAccountScript(address: Address): Option[Script] =
        if (address == aliceAddr) aliceScript.some
        else None

      override def getAccountDataEntry(address: Address, key: String): Option[DataEntry[?]] =
        if (address == aliceAddr && key == "x") IntegerDataEntry("x", initX).some
        else super.getAccountDataEntry(address, key)

      override def getBalance(address: Address, asset: Asset): Long = Long.MaxValue / 3

      override def getLeaseBalance(address: Address): BalanceResponse.WavesBalances =
        BalanceResponse.WavesBalances(getBalance(address, Asset.Waves))
    }

    val testDb = use(TestDb.mk())
    val sharedBlockchain = testDb.storage.batchedReadWrite { implicit ctx =>
      SharedBlockchainStorage[RideScriptRunRequest](
        SharedBlockchainStorage.Settings(blockchainSettings, CommonCache.Settings(ConfigMemorySize.ofBytes(1024))),
        testDb.storage,
        DefaultPersistentCaches(testDb.storage),
        blockchainApi
      )
    }

    val request                = RideScriptRunRequest(aliceAddr, Json.obj("expr" -> "foo()"), trace = false, intAsString = true)
    val requestServiceSettings = DefaultRequestService.Settings(enableTraces = true, enableStateChanges = false, Int.MaxValue, 0, 3, 0.seconds, 100)
    val requestService = use(
      new DefaultRequestService(
        settings = requestServiceSettings,
        sharedBlockchain = sharedBlockchain,
        requestScheduler = use(new TestJobScheduler()),
        runScriptScheduler = testScheduler
      ) {
        override def start(): Unit = {
          super.start()
          testScheduler.tick()
          trackAndRun(request).runToFuture
          testScheduler.tick()
        }
      }
    )
    val processor = new BlockchainProcessor(sharedBlockchain, requestService)

    def getScriptResult: JsObject = {
      val r = requestService.trackAndRun(request).runToFuture
      testScheduler.tick()
      Await.result(r, 5.seconds).lastResult
    }

    val blockchainUpdatesStream = use(blockchainApi.mkBlockchainUpdatesStream(testScheduler))

    val blockchainStateSettings = BlockchainState.Settings(1.second)
    val workingHeight           = Height(1)
    val eventsStream = blockchainUpdatesStream.downstream
      .doOnError(e =>
        Task {
          log.error("Error!", e)
        }
      )
      .take(events.size)
      .scanEval(Task.now[BlockchainState](BlockchainState.Starting(Height(0), workingHeight))) {
        BlockchainState.applyWithRestarts(blockchainStateSettings, processor, blockchainUpdatesStream, _, _)
      }
      .doOnError { e =>
        Task {
          log.error("Got an unhandled error, closing streams. Contact with developers", e)
          blockchainUpdatesStream.close()
        }
      }
      .lastL
      .runToFuture

    // We don't have a correct mock for getFromBlockchain, so we need to track changes from BlockchainUpdates

    blockchainUpdatesStream.start(Height(1))
    events.foreach { evt =>
      blockchainApi.blockchainUpdatesUpstream.onNext(evt)
      testScheduler.tick()
    }

    testScheduler.tick()
    testScheduler.tick(blockchainStateSettings.delayBeforeForceRestart)
    testScheduler.tick()

    withClue(dumpedTasks) {
      testScheduler.state.tasks shouldBe empty
    }
    log.info(s"The last result: ${Await.result(eventsStream, 5.seconds)}")

    val after = getScriptResult
    withClue(s"result.value._2.value at ${Json.prettyPrint(after)}") {
      (after \ "result" \ "value" \ "_2" \ "value").as[BigInt] shouldBe xPlusHeight
    }
  }.get

  private lazy val aliceScript: Script = ScriptUtil.from(s"""
{-#STDLIB_VERSION 6 #-}
{-#SCRIPT_TYPE ACCOUNT #-}
{-#CONTENT_TYPE DAPP #-}

@Callable(inv)
func foo() = {
let alice = Address(base58'$aliceAddr')
let x = getIntegerValue(alice, "x")
([], x + height)
}
""")
}
