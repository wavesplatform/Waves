package com.wavesplatform.ride.runner

import cats.syntax.option.*
import com.wavesplatform.BaseTestSuite
import com.wavesplatform.account.Address
import com.wavesplatform.api.DefaultBlockchainApi.toVanilla
import com.wavesplatform.api.HasGrpc
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.it.TestBlockchainApi
import com.wavesplatform.lang.script.Script
import com.wavesplatform.ride.runner.storage.persistent.HasDb.TestDb
import com.wavesplatform.ride.runner.storage.persistent.{DefaultPersistentCaches, HasDb}
import com.wavesplatform.ride.runner.storage.{RequestsStorage, ScriptRequest, SharedBlockchainStorage}
import com.wavesplatform.state.{DataEntry, Height, IntegerDataEntry}
import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import play.api.libs.json.Json

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Using

class RequestServiceTestSuite extends BaseTestSuite with HasGrpc with HasDb {
  private val aRequest = ScriptRequest(aliceAddr, Json.obj("expr" -> "default()"))
  private val bRequest = ScriptRequest(bobAddr, Json.obj("expr" -> "default()"))
  private val cRequest = ScriptRequest(carlAddr, Json.obj("expr" -> "default()"))

  private val requestsStorage = new RequestsStorage {
    override val all: List[ScriptRequest]       = List(aRequest, bRequest, cRequest)
    override val size: Int                      = all.size
    override def append(x: ScriptRequest): Unit = {} // Ignore, because no way to evaluate a new expr
  }

  private val accountScripts = Map(
    aliceAddr -> mkScript(aliceScriptSrc),
    bobAddr   -> mkScript(bobScriptSrc),
    carlAddr  -> mkScript(carlScriptSrc)
  )

  "RequestsService" - {
    "trackAndRun" - {
      "multiple simultaneous requests resolve once" in test { d =>
        d.blockchainApi.blockchainUpdatesUpstream.onNext(WrappedEvent.Next(mkBlockAppendEvent(1, 0)))
        d.scheduler.tick()

        val r1, r2 = d.requests.trackAndRun(aRequest)
        r1 shouldBe r2
      }

      "returns an actual value" in test { d =>
        def checkExpectedResults(aResult: Int, bResult: Int, cResult: Int): Unit = {
          withClue("a:")(d.trackAndRun(aRequest) shouldBe aResult)
          withClue("b:")(d.trackAndRun(bRequest) shouldBe bResult)
          withClue("c:")(d.trackAndRun(cRequest) shouldBe cResult)
        }

        d.blockchainApi.blockchainUpdatesUpstream.onNext(WrappedEvent.Next(mkBlockAppendEvent(1, 0)))
        d.scheduler.tick()
        checkExpectedResults(0, 1, 0)

        d.blockchainApi.blockchainUpdatesUpstream.onNext(WrappedEvent.Next(mkBlockAppendEvent(2, 0)))
        d.scheduler.tick()
        checkExpectedResults(0, 2, 0)

        d.blockchainApi.blockchainUpdatesUpstream.onNext(WrappedEvent.Next(mkBlockAppendEvent(3, 0)))
        d.scheduler.tick()
        checkExpectedResults(0, 3, 1)

        d.blockchainApi.blockchainUpdatesUpstream.onNext(WrappedEvent.Next(mkBlockAppendEvent(4, 0)))
        d.scheduler.tick()
        checkExpectedResults(0, 4, 1)
      }
    }
  }

  private case class TestDependencies(
      requests: RequestService,
      processor: Processor,
      blockchainApi: TestBlockchainApi,
      scheduler: TestScheduler
  ) {
    def trackAndRun(request: ScriptRequest): Int = {
      val task = requests.trackAndRun(request).runToFuture(scheduler)
      scheduler.tick()
      val r = Await.result(task, 5.seconds)
      withClue(s"$r:") {
        (r \ "result" \ "value" \ "_2" \ "value").as[Int]
      }
    }
  }

  protected def test(f: TestDependencies => Unit): Unit = Using.Manager { use =>
    implicit val testScheduler = TestScheduler()

    val blockchainApi = new TestBlockchainApi() {
      override def getCurrentBlockchainHeight(): Int                      = 2
      override def getBlockHeader(height: Int): Option[SignedBlockHeader] = toVanilla(mkPbBlock(height)).some
      override def getActivatedFeatures(height: Int): Map[Short, Int]     = blockchainSettings.functionalitySettings.preActivatedFeatures
      override def getAccountScript(address: Address): Option[Script]     = accountScripts.get(address).orElse(super.getAccountScript(address))
      override def getAccountDataEntry(address: Address, key: String): Option[DataEntry[?]] =
        if (address == aliceAddr && key == "x") IntegerDataEntry("x", 0).some
        else super.getAccountDataEntry(address, key)
    }

    val testDb = use(TestDb.mk())
    val blockchainStorage = testDb.storage.readWrite { implicit ctx =>
      SharedBlockchainStorage[ScriptRequest](
        settings.rideRunner.sharedBlockchain,
        testDb.storage,
        DefaultPersistentCaches(testDb.storage),
        blockchainApi
      )
    }

    val requestsService = new DefaultRequestService(
      settings = DefaultRequestService.Settings(enableTraces = false, Int.MaxValue, 0, 3, 0.seconds),
      storage = testDb.storage,
      sharedBlockchain = blockchainStorage,
      requestsStorage = requestsStorage,
      runScriptsScheduler = testScheduler
    )
    val processor               = new BlockchainProcessor(blockchainStorage, requestsService)
    val blockchainUpdatesStream = use(blockchainApi.mkBlockchainUpdatesStream(testScheduler))

    val workingHeight = Height(1)
    val eventsStream = blockchainUpdatesStream.downstream
      .doOnComplete(Task(log.info("test")))
      .doOnError(e =>
        Task {
          log.error("Error!", e)
        }
      )
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

    testScheduler.tick()
    f(
      TestDependencies(
        requestsService,
        new BlockchainProcessor(blockchainStorage, requestsService),
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
