package com.wavesplatform.events.grpc

import com.wavesplatform.events.grpc.protobuf.{BlockchainUpdatesApiGrpc, GetForHeightRequest, GetForHeightResponse}
import monix.execution.Scheduler

import scala.concurrent.Future

class BlockchainUpdatesApiGrpcImpl(implicit sc: Scheduler) extends BlockchainUpdatesApiGrpc.BlockchainUpdatesApi {
  override def getForHeight(request: GetForHeightRequest): Future[GetForHeightResponse] = ???
}
