package com.wavesplatform.events.grpc

import com.wavesplatform.events.grpc.protobuf.{BlockchainUpdatesApiGrpc, GetForHeightRequest, GetForHeightResponse}
import com.wavesplatform.events.protobuf.PBEvents
import com.wavesplatform.events.repo.UpdatesRepo
import monix.execution.Scheduler
import io.grpc.{Status, StatusRuntimeException}

import scala.concurrent.Future

class BlockchainUpdatesApiGrpcImpl(repo: UpdatesRepo)(implicit sc: Scheduler) extends BlockchainUpdatesApiGrpc.BlockchainUpdatesApi {
  override def getForHeight(request: GetForHeightRequest): Future[GetForHeightResponse] = {
    repo.getForHeight(request.height) match {
      case Some(upd) =>
        Future.successful {
          GetForHeightResponse(Some(PBEvents.protobuf(upd)))
        }
      case None =>
        Future {
          throw new StatusRuntimeException(Status.NOT_FOUND)
        }
    }
  }
}
