package com.wavesplatform.ride.runner

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import cats.syntax.option.*
import com.typesafe.config.ConfigMemorySize
import com.wavesplatform.BaseTestSuite
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.DefaultBlockchainApi.toVanilla
import com.wavesplatform.api.grpc.{BalanceResponse, BlockWithHeight}
import com.wavesplatform.api.http.ApiError.CustomValidationError
import com.wavesplatform.api.{HasBasicGrpcConverters, HasGrpc, TestBlockchainApi}
import com.wavesplatform.blockchain.SignedBlockHeaderWithVrf
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.lang.script.Script
import com.wavesplatform.ride.ScriptUtil
import com.wavesplatform.ride.runner.requests.*
import com.wavesplatform.ride.runner.storage.persistent.HasDb.mkTestDb
import com.wavesplatform.ride.runner.storage.persistent.{DefaultPersistentCaches, HasDb}
import com.wavesplatform.ride.runner.storage.{CacheKeyTags, InMemBlockchainDataCache, SharedBlockchainStorage}
import com.wavesplatform.state.{DataEntry, Height, IntegerDataEntry}
import com.wavesplatform.transaction.Asset
import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import play.api.libs.json.{JsNumber, JsString, JsValue, Json}

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Using

class RequestServiceTestSuite extends BaseTestSuite with HasGrpc with HasBasicGrpcConverters with HasDb {
  private val aRequest = RideScriptRunRequest(aliceAddr, Json.obj("expr" -> "default()"), trace = false, intAsString = true)
  private val bRequest = RideScriptRunRequest(bobAddr, Json.obj("expr" -> "default()"), trace = false, intAsString = true)
  private val cRequest = RideScriptRunRequest(carlAddr, Json.obj("expr" -> "default()"), trace = false, intAsString = true)

  private val accountScripts = Map(
    aliceAddr -> (alice.publicKey, ScriptUtil.from(aliceScriptSrc)),
    bobAddr   -> (bob.publicKey, ScriptUtil.from(bobScriptSrc)),
    carlAddr  -> (carl.publicKey, ScriptUtil.from(carlScriptSrc))
  )

  private val defaultRequestServiceSettings =
    DefaultRequestService.Settings(
      enableTraces = true,
      enableStateChanges = true,
      Int.MaxValue,
      0,
      3,
      ConfigMemorySize.ofBytes(10000),
      0.seconds,
      100
    )

  "RequestService" - {
    "trackAndRun" - {
      "returns an actual value" in test() { d =>
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

      "returns an error if the provided expr is wrong" in test() { d =>
        val request = RideScriptRunRequest(
          address = aliceAddr,
          requestBody = Json.obj("expr" -> "buyNsbtREADONLY(10000000000000000000)"),
          trace = false,
          intAsString = true
        )

        d.emitSimpleEvent()
        d.checkFull(
          request,
          Json.obj(
            "error"   -> CustomValidationError.Id,
            "message" -> "For input string: \"10000000000000000000\""
          ),
          StatusCodes.BadRequest
        )
      }

      "contains address and expr" in test(defaultRequestServiceSettings) { d =>
        d.emitSimpleEvent()

        val r = d.trackAndRunLastResult(aRequest)
        withClue("address:") { (r \ "address").toOption shouldBe defined }
        withClue("expr:") { (r \ "expr").toOption shouldBe defined }
      }

      def runTracesTest(enableTraces: Boolean, askTraces: Boolean, hasTraces: Boolean): Unit =
        s"enable=$enableTraces, ask=$askTraces, has=$hasTraces" in test(defaultRequestServiceSettings.copy(enableTraces = enableTraces)) { d =>
          d.emitSimpleEvent()

          val traces = (d.trackAndRunLastResult(aRequest.copy(trace = askTraces)) \ "vars").toOption
          if (hasTraces) traces should not be empty
          else traces shouldBe empty
        }

      "traces" - {
        runTracesTest(enableTraces = true, askTraces = true, hasTraces = true)
        runTracesTest(enableTraces = true, askTraces = false, hasTraces = false)
        runTracesTest(enableTraces = false, askTraces = true, hasTraces = false)
        runTracesTest(enableTraces = false, askTraces = false, hasTraces = false)
      }

      "state changes" - {
        "has if enabled" in test(defaultRequestServiceSettings.copy(enableStateChanges = true)) { d =>
          d.emitSimpleEvent()
          (d.trackAndRunLastResult(aRequest) \ "stateChanges").toOption should not be empty
        }

        "hasn't if disabled" in test(defaultRequestServiceSettings.copy(enableStateChanges = false)) { d =>
          d.emitSimpleEvent()
          (d.trackAndRunLastResult(aRequest) \ "stateChanges").toOption shouldBe empty
        }
      }

      "int as string" in test(defaultRequestServiceSettings) { d =>
        d.emitSimpleEvent()

        def run(intAsString: Boolean) = (d.trackAndRunLastResult(aRequest.copy(intAsString = intAsString)) \ "result" \ "value" \ "_2" \ "value").get

        withClue("intAsString == true:") {
          run(intAsString = true) shouldBe a[JsString]
        }

        withClue("intAsString == false:") {
          run(intAsString = false) shouldBe a[JsNumber]
        }
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
    def check(request: RideScriptRunRequest, expectedValue: Int): Unit = {
      val r = trackAndRun(request)
      withClue(s"$r:") {
        r.lastStatus shouldBe StatusCodes.OK
        (Json.parse(r.lastResult) \ "result" \ "value" \ "_2" \ "value").as[BigInt].toInt shouldBe expectedValue
      }
    }

    def checkFull(request: RideScriptRunRequest, json: JsValue, status: StatusCode): Unit = {
      val r = trackAndRun(request)
      withClue(s"$r:") {
        r.lastStatus shouldBe status
        Json.parse(r.lastResult) shouldBe json
      }
    }

    def trackAndRun(request: RideScriptRunRequest): RideScriptRunResult = {
      val task = requests.trackAndRun(request).runToFuture(scheduler)
      scheduler.tick()
      Await.result(task, 5.seconds)
    }

    def trackAndRunLastResult(request: RideScriptRunRequest): JsValue = {
      val task = requests.trackAndRun(request).runToFuture(scheduler)
      scheduler.tick()
      Json.parse(Await.result(task, 5.seconds).lastResult)
    }

    def emitSimpleEvent(): Unit = {
      blockchainApi.blockchainUpdatesUpstream.onNext(WrappedEvent.Next(mkBlockAppendEvent(1, 0)))
      scheduler.tick()
    }
  }

  protected def test(requestServiceSettings: DefaultRequestService.Settings = defaultRequestServiceSettings)(f: TestDependencies => Unit): Unit =
    Using.Manager { use =>
      implicit val testScheduler = TestScheduler()

      val blockchainApi = new TestBlockchainApi() {
        override def getCurrentBlockchainHeight(): Height = Height(2)
        override def getBlockHeader(height: Height): Option[SignedBlockHeaderWithVrf] =
          toVanilla(BlockWithHeight(mkPbBlock(height).some, height))
        override def getActivatedFeatures(height: Height): Map[Short, Height] =
          blockchainSettings.functionalitySettings.preActivatedFeatures.view.mapValues(Height(_)).toMap
        override def getAccountScript(address: Address): Option[(PublicKey, Script)] = accountScripts.get(address)
        override def getAccountDataEntry(address: Address, key: String): Option[DataEntry[?]] =
          if (address == aliceAddr && key == "x") IntegerDataEntry("x", 0).some
          else super.getAccountDataEntry(address, key)

        override def getBalance(address: Address, asset: Asset): Long = Long.MaxValue / 3
        override def getLeaseBalance(address: Address): BalanceResponse.WavesBalances =
          BalanceResponse.WavesBalances(getBalance(address, Asset.Waves))
      }

      val testDb  = use(mkTestDb())
      val allTags = new CacheKeyTags[RideScriptRunRequest]
      val sharedBlockchain = testDb.access.directReadWrite { implicit ctx =>
        SharedBlockchainStorage[RideScriptRunRequest](
          SharedBlockchainStorage.Settings(blockchainSettings, InMemBlockchainDataCache.Settings(ConfigMemorySize.ofBytes(1024))),
          allTags,
          testDb.access,
          DefaultPersistentCaches(testDb.access),
          blockchainApi
        )
      }

      val requestsService =
        use(new DefaultRequestService(requestServiceSettings, sharedBlockchain, allTags, use(new TestJobScheduler()), testScheduler))
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
          BlockchainState.applyWithRestarts(BlockchainState.Settings(1.second), processor, blockchainUpdatesStream, _, _)
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
