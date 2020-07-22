package com.wavesplatform.events.grpc

import com.wavesplatform.events.grpc.protobuf.{BlockchainUpdatesApiGrpc, GetForHeightRequest, GetForHeightResponse}
import com.wavesplatform.events.protobuf.serde._
import com.wavesplatform.events.repo.UpdatesRepo
import com.wavesplatform.utils.ScorexLogging
import monix.execution.Scheduler
import io.grpc.{Status, StatusRuntimeException}

import scala.concurrent.Future
import scala.util.{Failure, Success}

class BlockchainUpdatesApiGrpcImpl(repo: UpdatesRepo)(implicit sc: Scheduler)
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
}
