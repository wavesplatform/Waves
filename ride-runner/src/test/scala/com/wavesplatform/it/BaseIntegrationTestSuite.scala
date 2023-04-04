package com.wavesplatform.it

import cats.syntax.option.*
import com.wavesplatform.account.Address
import com.wavesplatform.api.DefaultBlockchainApi.*
import com.wavesplatform.api.HasGrpc
import com.wavesplatform.api.grpc.{BalanceResponse, BlockWithHeight}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.blockchain.SignedBlockHeaderWithVrf
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.lang.script.Script
import com.wavesplatform.ride.runner.requests.DefaultRequestService
import com.wavesplatform.ride.runner.storage.persistent.HasDb.TestDb
import com.wavesplatform.ride.runner.storage.persistent.{DefaultPersistentCaches, HasDb}
import com.wavesplatform.ride.runner.storage.{RequestsStorage, ScriptRequest, SharedBlockchainStorage}
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

abstract class BaseIntegrationTestSuite extends BaseTestSuite with HasGrpc with HasDb with HasMonixHelpers {
  protected val initX = 0

  /** @param xPlusHeight
    *   An expected script result, x > 0? The initial value is 0
    */
  protected def test(events: List[WrappedEvent[SubscribeEvent]], xPlusHeight: Int): Unit = Using.Manager { use =>
    implicit val testScheduler = TestScheduler()

    val blockchainApi = new TestBlockchainApi() {
      override def getCurrentBlockchainHeight(): Int = 1

      override def getBlockHeader(height: Int): Option[SignedBlockHeaderWithVrf] =
        toVanilla(BlockWithHeight(mkPbBlock(height).some, height))

      override def getActivatedFeatures(height: Int): Map[Short, Int] = blockchainSettings.functionalitySettings.preActivatedFeatures

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
    val sharedBlockchain = testDb.storage.readWrite { implicit ctx =>
      SharedBlockchainStorage[ScriptRequest](
        settings.rideRunner.sharedBlockchain,
        testDb.storage,
        DefaultPersistentCaches(testDb.storage),
        blockchainApi
      )
    }

    val request = ScriptRequest(aliceAddr, Json.obj("expr" -> "foo()"))
    val requestService = new DefaultRequestService(
      settings = DefaultRequestService.Settings(enableTraces = true, Int.MaxValue, 0, 3, 0.seconds),
      db = testDb.storage,
      sharedBlockchain = sharedBlockchain,
//      requestsStorage = new RequestsStorage {
//        override def size: Int                      = 1
//        override def append(x: ScriptRequest): Unit = {} // Ignore, because no way to evaluate a new expr
//        override def all(): List[ScriptRequest]     = List(request)
//      },
      null, // TODO
      runScriptScheduler = testScheduler
    )
    val processor = new BlockchainProcessor(sharedBlockchain, requestService)

    requestService.runAll().runToFuture
    testScheduler.tick(1.milli) // 1 millisecond to detect that script will be ran in the end

    def getScriptResult: JsObject = {
      val r = requestService.trackAndRun(request).runToFuture
      testScheduler.tick()
      Await.result(r, 5.seconds)
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
        BlockchainState(blockchainStateSettings, processor, blockchainUpdatesStream, _, _)
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
    val _ = getScriptResult // So, track this script

    blockchainUpdatesStream.start(1)
    events.foreach(blockchainApi.blockchainUpdatesUpstream.onNext)

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

  private lazy val aliceScript: Script = mkScript(s"""
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
