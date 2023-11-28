package com.wavesplatform.it

import com.wavesplatform.BaseTestSuite
import com.wavesplatform.api.{DefaultBlockchainApi, EmptyChannel, HasGrpc}
import com.wavesplatform.blockchain.TestProcessor
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.events.api.grpc.protobuf.*
import com.wavesplatform.ride.runner.BlockchainState
import com.wavesplatform.state.Height
import io.grpc.*
import io.grpc.stub.StreamObserver
import monix.eval.Task
import monix.execution.exceptions.UpstreamTimeoutException
import monix.execution.schedulers.TestScheduler
import org.scalatest.prop.TableDrivenPropertyChecks

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class RestartDueToGrpcErrorTestSuite extends BaseTestSuite with TableDrivenPropertyChecks with HasGrpc {
  "Restart due to gRPC error" in forAll(
    // See BlockchainState.grpcStatusesToRestart
    Table[Throwable, Boolean](
      ("failReason", "restartIsExpected"),
      (new StatusRuntimeException(Status.INTERNAL), true),
      (new StatusRuntimeException(Status.UNKNOWN), true),
      (new StatusRuntimeException(Status.UNAVAILABLE), true),
      (new StatusRuntimeException(Status.INVALID_ARGUMENT.withDescription("Requested start height exceeds current blockchain height")), true),
      (new UpstreamTimeoutException(1.second), true),
      (new StatusRuntimeException(Status.INVALID_ARGUMENT), false),
      (new StatusRuntimeException(Status.UNIMPLEMENTED), false)
    )
  ) { (failReason, restartIsExpected) =>
    implicit val testScheduler = TestScheduler()
    val restarted              = new AtomicBoolean(false)

    val blockchainUpdatesGrpcService = new BlockchainUpdatesApiGrpc.BlockchainUpdatesApi {
      override def subscribe(request: SubscribeRequest, responseObserver: StreamObserver[SubscribeEvent]): Unit = {
        if (request.fromHeight == 3) responseObserver.onError(failReason)
        else if (request.fromHeight == 2) {
          responseObserver.onCompleted()
          restarted.set(true)
        } else responseObserver.onError(new RuntimeException("Impossible"))
      }

      override def getBlockUpdatesRange(request: GetBlockUpdatesRangeRequest): Future[GetBlockUpdatesRangeResponse] = ???
      override def getBlockUpdate(request: GetBlockUpdateRequest): Future[GetBlockUpdateResponse]                   = ???
    }

    withGrpc(blockchainUpdatesGrpcService) { channel =>
      val blockchainApi = new DefaultBlockchainApi(
        DefaultBlockchainApi.Settings(
          DefaultBlockchainApi.GrpcApiSettings(None),
          DefaultBlockchainApi.BlockchainUpdatesApiSettings(100.minutes, 2)
        ),
        EmptyChannel,
        channel
      )

      val stream        = blockchainApi.mkBlockchainUpdatesStream(testScheduler)
      val testProcessor = new TestProcessor
      val settings      = BlockchainState.Settings(1.second)
      stream.downstream
        .doOnNext {
          case WrappedEvent.Closed => Task(stream.close())
          case _                   => Task.unit
        }
        .scanEval(Task.now[BlockchainState](BlockchainState.Starting(Height(2), Height(4)))) { case (acc, x) =>
          BlockchainState.applyWithRestarts(
            settings = settings,
            processor = testProcessor,
            blockchainUpdatesStream = stream,
            orig = acc,
            event = x
          )
        }
        .lastL
        .runToFuture(testScheduler)

      stream.start(Height(3))

      testScheduler.tickOne() // Subscribe
      testScheduler.tickOne() // Sending error
      withClue("not yet restarted: ") { restarted.get() shouldBe false }

      testScheduler.tick(1.second) // BlockchainState.Settings
      if (restartIsExpected) withClue("restarted: ") { restarted.get() shouldBe true }
      else withClue("still not restarted: ") { restarted.get() shouldBe false }
    }
  }
}
