package com.wavesplatform.grpc.observers

import com.wavesplatform.BaseTestSuite
import com.wavesplatform.events.api.grpc.protobuf.*
import com.wavesplatform.grpc.observers.MonixWrappedDownstreamTestSuite.EmptyChannel
import com.wavesplatform.grpc.{DefaultBlockchainApi, HasGrpc}
import com.wavesplatform.utils.ScorexLogging
import io.grpc.stub.StreamObserver
import io.grpc.{CallOptions, Channel, ClientCall, MethodDescriptor}
import monix.execution.Scheduler.Implicits.traced
import monix.execution.Scheduler.global
import monix.execution.exceptions.UpstreamTimeoutException
import sttp.client3.testing.SttpBackendStub

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class MonixWrappedDownstreamTestSuite extends BaseTestSuite with HasGrpc with ScorexLogging {
  "MonixWrappedDownstream" - {
    "timeouts" in {
      val blockchainUpdatesGrpcService = new BlockchainUpdatesApiGrpc.BlockchainUpdatesApi {
        override def subscribe(request: SubscribeRequest, responseObserver: StreamObserver[SubscribeEvent]): Unit     = {}
        override def getBlockUpdatesRange(request: GetBlockUpdatesRangeRequest): Future[GetBlockUpdatesRangeResponse] = ???
        override def getBlockUpdate(request: GetBlockUpdateRequest): Future[GetBlockUpdateResponse]                   = ???
      }

      val r = withGrpc(blockchainUpdatesGrpcService) { channel =>
        val blockchainApi = new DefaultBlockchainApi(
          DefaultBlockchainApi.Settings(
            "",
            DefaultBlockchainApi.GrpcApiSettings(None),
            DefaultBlockchainApi.BlockchainUpdatesApiSettings(100.millis, 2)
          ),
          EmptyChannel,
          channel,
          SttpBackendStub.synchronous
        )

        val stream = blockchainApi.mkBlockchainUpdatesStream(global)
        val r      = stream.stream.failed.runAsyncGetFirst
        stream.start(1)
        r
      }

      Await.result(r, 1.minute).value shouldBe a[UpstreamTimeoutException]
    }
  }
}

object MonixWrappedDownstreamTestSuite {
  object EmptyChannel extends Channel {
    override def authority(): String = ???
    override def newCall[RequestT, ResponseT](
        methodDescriptor: MethodDescriptor[RequestT, ResponseT],
        callOptions: CallOptions
    ): ClientCall[RequestT, ResponseT] = ???
  }
}
