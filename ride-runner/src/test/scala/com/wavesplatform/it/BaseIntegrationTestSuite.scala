package com.wavesplatform.it

import cats.syntax.option.*
import com.wavesplatform.account.Address
import com.wavesplatform.api.DefaultBlockchainApi.*
import com.wavesplatform.api.HasGrpc
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.blockchain.{BlockchainProcessor, BlockchainState, SharedBlockchainData}
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.lang.script.Script
import com.wavesplatform.riderunner.DefaultRequestsService
import com.wavesplatform.riderunner.storage.HasLevelDb.TestDb
import com.wavesplatform.riderunner.storage.persistent.LevelDbPersistentCaches
import com.wavesplatform.riderunner.storage.{HasLevelDb, RequestKey, RequestsStorage}
import com.wavesplatform.state.{DataEntry, Height, IntegerDataEntry}
import com.wavesplatform.{BaseTestSuite, HasMonixHelpers}
import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import play.api.libs.json.Json

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Using

abstract class BaseIntegrationTestSuite extends BaseTestSuite with HasGrpc with HasLevelDb with HasMonixHelpers {
  protected val initX = 0

  /** @param xPlusHeight
    *   An expected script result, x > 0? The initial value is 0
    */
  protected def test(events: List[WrappedEvent[SubscribeEvent]], xPlusHeight: Int): Unit = Using.Manager { use =>
    implicit val testScheduler = TestScheduler()

    val blockchainApi = new TestBlockchainApi() {
      override def getCurrentBlockchainHeight(): Int = 2

      override def getBlockHeader(height: Int): Option[SignedBlockHeader] = toVanilla(mkPbBlock(height)).some

      override def getActivatedFeatures(height: Int): Map[Short, Int] = blockchainSettings.functionalitySettings.preActivatedFeatures

      override def getAccountScript(address: Address): Option[Script] =
        if (address == aliceAddr) aliceScript.some
        else super.getAccountScript(address)

      override def getAccountDataEntry(address: Address, key: String): Option[DataEntry[?]] =
        if (address == aliceAddr && key == "x") IntegerDataEntry("x", initX).some
        else super.getAccountDataEntry(address, key)
    }

    val testDb   = use(TestDb.mk())
    val dbCaches = new LevelDbPersistentCaches(testDb.db)
    val blockchainStorage = new SharedBlockchainData[RequestKey](
      settings.rideRunner.sharedBlockchain,
      dbCaches,
      blockchainApi
    )

    val request = RequestKey(aliceAddr, Json.obj("expr" -> "foo()"))
    val requestsService = new DefaultRequestsService(
      settings = DefaultRequestsService.Settings(enableTraces = false, Int.MaxValue, 0, 3, 0.seconds),
      sharedBlockchain = blockchainStorage,
      storage = new RequestsStorage {
        override def size: Int                   = 1
        override def append(x: RequestKey): Unit = {} // Ignore, because no way to evaluate a new expr
        override def all(): List[RequestKey]     = List(request)
      },
      runScriptsScheduler = testScheduler
    )
    val processor = new BlockchainProcessor(blockchainStorage, requestsService)

    requestsService.runAll().runToFuture
    testScheduler.tick(1.milli) // 1 millisecond to detect that script will be ran in the end

    def getScriptResult = {
      val r = requestsService.trackAndRun(request).runToFuture
      testScheduler.tick()
      Await.result(r, 5.seconds)
    }

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
    withClue(Json.prettyPrint(after)) {
      (after \ "result" \ "value" \ "_2" \ "value").as[Int] shouldBe xPlusHeight
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
