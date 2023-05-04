package com.wavesplatform.ride.runner

import cats.syntax.option.*
import com.wavesplatform.BaseTestSuite
import com.wavesplatform.account.Address
import com.wavesplatform.api.DefaultBlockchainApi.toVanilla
import com.wavesplatform.api.grpc.{BalanceResponse, BlockWithHeight}
import com.wavesplatform.api.{HasBasicGrpcConverters, HasGrpc, TestBlockchainApi}
import com.wavesplatform.blockchain.SignedBlockHeaderWithVrf
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.lang.script.Script
import com.wavesplatform.ride.runner.requests.{DefaultRequestService, RequestService, TestJobScheduler}
import com.wavesplatform.ride.runner.storage.persistent.HasDb.TestDb
import com.wavesplatform.ride.runner.storage.persistent.{DefaultPersistentCaches, HasDb}
import com.wavesplatform.ride.runner.storage.{ScriptRequest, SharedBlockchainStorage}
import com.wavesplatform.state.{DataEntry, Height, IntegerDataEntry}
import com.wavesplatform.transaction.Asset
import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import play.api.libs.json.{JsValue, Json}

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Using

class RequestServiceTestSuite extends BaseTestSuite with HasGrpc with HasBasicGrpcConverters with HasDb {
  private val aRequest = ScriptRequest(aliceAddr, Json.obj("expr" -> "default()"))
  private val bRequest = ScriptRequest(bobAddr, Json.obj("expr" -> "default()"))
  private val cRequest = ScriptRequest(carlAddr, Json.obj("expr" -> "default()"))

  private val accountScripts = Map(
    aliceAddr -> mkScript(aliceScriptSrc),
    bobAddr   -> mkScript(bobScriptSrc),
    carlAddr  -> mkScript(carlScriptSrc)
  )

  "RequestsService" - {
    "trackAndRun" - {
      "returns an actual value" in test { d =>
        def checkExpectedResults(aResult: Int, bResult: Int, cResult: Int): Unit = {
          withClue(s"a $aliceAddr:")(d.check(aRequest, aResult))
          withClue(s"b $bobAddr:")(d.check(bRequest, bResult))
          withClue(s"c: $carlAddr")(d.check(cRequest, cResult))
        }

        d.blockchainApi.blockchainUpdatesUpstream.onNext(WrappedEvent.Next(mkBlockAppendEvent(1, 0)))
        d.scheduler.tick()
        checkExpectedResults(0, 1, 0)

        d.blockchainApi.blockchainUpdatesUpstream.onNext(
          WrappedEvent.Next(
            mkBlockAppendEvent(2, 0, List(mkDataEntryUpdate(aliceAddr, "x", 0L.some, 1L.some)))
          )
        )
        d.scheduler.tick()
        checkExpectedResults(1, 2, 0)

        d.blockchainApi.blockchainUpdatesUpstream.onNext(WrappedEvent.Next(mkBlockAppendEvent(3, 0)))
        d.scheduler.tick()
        checkExpectedResults(1, 3, 1)

        d.blockchainApi.blockchainUpdatesUpstream.onNext(WrappedEvent.Next(mkBlockAppendEvent(4, 0)))
        d.scheduler.tick()
        checkExpectedResults(1, 4, 1)
      }

      "returns an error if the provided expr is wrong" in test { d =>
        val request = ScriptRequest(
          address = aliceAddr,
          requestBody = Json.obj("expr" -> "buyNsbtREADONLY(10000000000000000000)")
        )

        d.blockchainApi.blockchainUpdatesUpstream.onNext(WrappedEvent.Next(mkBlockAppendEvent(1, 0)))
        d.scheduler.tick()

        d.checkFull(
          request,
          Json.obj(
            "error"   -> 119,
            "message" -> "For input string: \"10000000000000000000\""
          )
        )
      }
    }
  }

  private case class TestDependencies(
      requestServiceSettings: DefaultRequestService.Settings,
      requests: RequestService,
      processor: Processor,
      blockchainApi: TestBlockchainApi,
      scheduler: TestScheduler
  ) {
    def check(request: ScriptRequest, expectedValue: Int): Unit = {
      val task = requests.trackAndRun(request).runToFuture(scheduler)
      scheduler.tick()
      val r = Await.result(task, 5.seconds)
      withClue(s"$r:") {
        (r \ "result" \ "value" \ "_2" \ "value").as[BigInt].toInt shouldBe expectedValue
      }
    }

    def checkFull(request: ScriptRequest, json: JsValue): Unit = {
      val task = requests.trackAndRun(request).runToFuture(scheduler)
      scheduler.tick()
      val r = Await.result(task, 5.seconds)
      withClue(s"$r:") {
        r shouldBe json
      }
    }
  }

  protected def test(f: TestDependencies => Unit): Unit = Using.Manager { use =>
    implicit val testScheduler = TestScheduler()

    val blockchainApi = new TestBlockchainApi() {
      override def getCurrentBlockchainHeight(): Height = Height(2)
      override def getBlockHeader(height: Height): Option[SignedBlockHeaderWithVrf] =
        toVanilla(BlockWithHeight(mkPbBlock(height).some, height))
      override def getActivatedFeatures(height: Height): Map[Short, Height] =
        blockchainSettings.functionalitySettings.preActivatedFeatures.view.mapValues(Height(_)).toMap
      override def getAccountScript(address: Address): Option[Script] = accountScripts.get(address)
      override def getAccountDataEntry(address: Address, key: String): Option[DataEntry[?]] =
        if (address == aliceAddr && key == "x") IntegerDataEntry("x", 0).some
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

    val requestServiceSettings = DefaultRequestService.Settings(enableTraces = true, Int.MaxValue, 0, 3, 0.seconds)
    val requestsService = use(
      new DefaultRequestService(
        settings = requestServiceSettings,
        sharedBlockchain = sharedBlockchain,
        use(new TestJobScheduler()),
        runScriptScheduler = testScheduler
      )
    )
    val processor               = new BlockchainProcessor(sharedBlockchain, requestsService)
    val blockchainUpdatesStream = use(blockchainApi.mkBlockchainUpdatesStream(testScheduler))

    val workingHeight = Height(1)
    val eventsStream = blockchainUpdatesStream.downstream
      .doOnError(e =>
        Task {
          log.error("Error!", e)
        }
      )
      .scanEval(Task.now[BlockchainState](BlockchainState.Starting(Height(0), workingHeight))) {
        BlockchainState(BlockchainState.Settings(1.second), processor, blockchainUpdatesStream, _, _)
      }
      .doOnError { e =>
        Task {
          log.error("Got an unhandled error, closing streams. Contact with developers", e)
          blockchainUpdatesStream.close()
        }
      }
      .lastL
      .runToFuture

    blockchainUpdatesStream.start(Height(1))

    testScheduler.tick()
    f(
      TestDependencies(
        requestServiceSettings,
        requestsService,
        new BlockchainProcessor(sharedBlockchain, requestsService),
        blockchainApi,
        testScheduler
      )
    )
    blockchainApi.blockchainUpdatesUpstream.onComplete()
    testScheduler.tick()
    Await.result(eventsStream, 5.seconds)
  }.get

  private lazy val aliceScriptSrc = s"""
{-#STDLIB_VERSION 6 #-}
{-#SCRIPT_TYPE ACCOUNT #-}
{-#CONTENT_TYPE DAPP #-}

@Callable(inv)
func default() = {
  let alice = Address(base58'$aliceAddr')
  let x = getIntegerValue(alice, "x")
  ([], x)
}
"""

  private lazy val bobScriptSrc = s"""
{-#STDLIB_VERSION 6 #-}
{-#SCRIPT_TYPE ACCOUNT #-}
{-#CONTENT_TYPE DAPP #-}

@Callable(inv)
func default() = {
  ([], height)
}
"""

  private lazy val carlScriptSrc = s"""
{-#STDLIB_VERSION 6 #-}
{-#SCRIPT_TYPE ACCOUNT #-}
{-#CONTENT_TYPE DAPP #-}

@Callable(inv)
func default() = {
  ([], height / 3)
}
"""
}
