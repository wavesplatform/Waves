package com.wavesplatform.events.api.grpc

import scala.concurrent.Future
import scala.util.{Failure, Success}

import com.wavesplatform.api.grpc._
import com.wavesplatform.events.api.grpc.protobuf._
import com.wavesplatform.events.protobuf.serde._
import com.wavesplatform.events.repo.UpdatesRepo
import com.wavesplatform.utils.ScorexLogging
import io.grpc.{Status, StatusRuntimeException}
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler

class BlockchainUpdatesApiGrpcImpl(repo: UpdatesRepo.Read with UpdatesRepo.Stream)(implicit sc: Scheduler)
    extends BlockchainUpdatesApiGrpc.BlockchainUpdatesApi
    with ScorexLogging {
  override def getBlockUpdate(request: GetBlockUpdateRequest): Future[GetBlockUpdateResponse] = Future {
    repo.updateForHeight(request.height) match {
      case Success(upd)                       => GetBlockUpdateResponse(Some(upd.protobuf))
      case Failure(_: NoSuchElementException) => throw new StatusRuntimeException(Status.NOT_FOUND)
      case Failure(e: IllegalArgumentException) =>
        throw new StatusRuntimeException(Status.INVALID_ARGUMENT.withDescription(e.getMessage))
      case Failure(exception) =>
        log.error(s"BlockchainUpdates gRPC failed to get block update for height ${request.height}", exception)
        throw new StatusRuntimeException(Status.INTERNAL)
    }
  }

  override def getBlockUpdatesRange(request: GetBlockUpdatesRangeRequest): Future[GetBlockUpdatesRangeResponse] = {
    if (request.fromHeight <= 0) {
      Future.failed(new IllegalArgumentException("height must be a positive integer")).wrapErrors
    } else if (request.toHeight < request.fromHeight) {
      Future.failed(new IllegalArgumentException("toHeight should be >= fromHeight")).wrapErrors
    } else {
      repo
        .updatesRange(request.fromHeight, request.toHeight) // TODO: Use stream
        .take(1000)                                         // Limit
        .toListL
        .runAsyncLogErr
        .map(updates => GetBlockUpdatesRangeResponse(updates.map(_.protobuf)))
        .wrapErrors
    }
  }

  override def subscribe(request: SubscribeRequest, responseObserver: StreamObserver[SubscribeEvent]): Unit = {
    if (request.fromHeight <= 0) {
      responseObserver.failWith(new IllegalArgumentException("height must be a positive integer"))
    } else if (request.toHeight != 0 && request.toHeight < request.fromHeight) {
      responseObserver.failWith(new IllegalArgumentException("toHeight should be >= fromHeight"))
    } else {
      val updatesPB = repo
        .stream(request.fromHeight)
        .takeWhile(bu => request.toHeight == 0 || bu.height <= request.toHeight)
        .map(elem => SubscribeEvent(update = Some(elem.protobuf)))

      responseObserver.completeWith(updatesPB)
    }
  }

}
