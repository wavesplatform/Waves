package com.wavesplatform.events.api.grpc

import com.wavesplatform.events.api.grpc.backpressure._
import com.wavesplatform.events.api.grpc.protobuf._
import com.wavesplatform.events.protobuf.serde._
import com.wavesplatform.events.repo.UpdatesRepo
import com.wavesplatform.utils.ScorexLogging
import io.grpc.stub.StreamObserver
import io.grpc.{Status, StatusRuntimeException}
import monix.execution.Scheduler

import scala.concurrent.Future
import scala.util.{Failure, Success}

class BlockchainUpdatesApiGrpcImpl(repo: UpdatesRepo.Read with UpdatesRepo.Stream)(implicit sc: Scheduler)
    extends BlockchainUpdatesApiGrpc.BlockchainUpdatesApi
    with ScorexLogging {
  override def getBlockUpdate(request: GetBlockUpdateRequest): Future[GetBlockUpdateResponse] = Future {
    repo.updateForHeight(request.height) match {
      case Success(Some(upd)) => GetBlockUpdateResponse(Some(upd.protobuf))
      case Success(None)      => throw new StatusRuntimeException(Status.NOT_FOUND)
      case Failure(e: IllegalArgumentException) =>
        throw new StatusRuntimeException(Status.INVALID_ARGUMENT.withDescription(e.getMessage))
      case Failure(exception) =>
        log.error(s"BlockchainUpdates gRPC failed to get block update for height ${request.height}", exception)
        throw new StatusRuntimeException(Status.INTERNAL)
    }
  }

  override def getBlockUpdatesRange(request: GetBlockUpdatesRangeRequest): Future[GetBlockUpdatesRangeResponse] = Future {
    repo.updatesRange(request.fromHeight, request.toHeight) match {
      case Success(updates) => GetBlockUpdatesRangeResponse(updates.map(_.protobuf))
      case Failure(e: IllegalArgumentException) =>
        throw new StatusRuntimeException(Status.INVALID_ARGUMENT.withDescription(e.getMessage))
      case Failure(e) =>
        log.error(s"BlockchainUpdates gRPC failed to get block range updates for range ${request.fromHeight} to ${request.toHeight}", e)
        throw new StatusRuntimeException(Status.INTERNAL)
    }
  }

  override def subscribe(request: SubscribeRequest, responseObserver: StreamObserver[SubscribeEvent]): Unit = {
    if (request.fromHeight <= 0) {
      responseObserver.onError(new StatusRuntimeException(Status.INVALID_ARGUMENT.withDescription("height must be a positive integer")))
    } else {
      val updatesPB = repo
        .stream(request.fromHeight)
        .map(elem => SubscribeEvent(update = Some(elem.protobuf)))

      wrapObservable(updatesPB, responseObserver)(identity)
    }
  }

}
