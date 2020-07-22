package com.wavesplatform.events.grpc

import com.wavesplatform.events.BlockchainUpdated
import com.wavesplatform.events.grpc.protobuf.{
  BlockchainUpdatesApiGrpc,
  GetForHeightRequest,
  GetForHeightResponse,
  SubscribeEvent,
  SubscribeRequest
}
import com.wavesplatform.events.protobuf.serde._
import com.wavesplatform.events.repo.UpdatesRepo
import com.wavesplatform.utils.ScorexLogging
import io.grpc.stub.StreamObserver
import monix.execution.{Ack, Scheduler}
import io.grpc.{Status, StatusRuntimeException}
import monix.execution.Ack.Continue
import monix.reactive.{Observable, Observer}

import scala.concurrent.Future
import scala.util.{Failure, Success}

class BlockchainUpdatesApiGrpcImpl(repo: UpdatesRepo, currentUpdates: Observable[BlockchainUpdated])(implicit sc: Scheduler)
    extends BlockchainUpdatesApiGrpc.BlockchainUpdatesApi
    with ScorexLogging {
  override def getForHeight(request: GetForHeightRequest): Future[GetForHeightResponse] = Future {
    repo.getForHeight(request.height) match {
      case Success(Some(upd)) => GetForHeightResponse(Some(upd.protobuf))
      case Success(None)      => throw new StatusRuntimeException(Status.NOT_FOUND)
      case Failure(exception) =>
        log.error(s"Failed to get block append for height ${request.height}", exception)
        throw new StatusRuntimeException(Status.INTERNAL)
    }
  }

  // one client should not block whole database
  override def subscribe(request: SubscribeRequest, responseObserver: StreamObserver[SubscribeEvent]): Unit = {
    currentUpdates.subscribe(new Observer[BlockchainUpdated] {
      override def onNext(elem: BlockchainUpdated): Future[Ack] = {
        responseObserver.onNext(SubscribeEvent(update = Some(elem.protobuf)))
        Future.successful(Continue)
      }
      override def onError(ex: Throwable): Unit = responseObserver.onError(ex)
      override def onComplete(): Unit           = responseObserver.onCompleted()
    })
  }

}
