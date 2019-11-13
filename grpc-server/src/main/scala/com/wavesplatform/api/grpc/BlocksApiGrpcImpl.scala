package com.wavesplatform.api.grpc

import com.google.protobuf.empty.Empty
import com.google.protobuf.wrappers.UInt32Value
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.grpc.BlockRequest.Request
import com.wavesplatform.api.http.ApiError.BlockDoesNotExist
import com.wavesplatform.protobuf.block.PBBlock
import io.grpc.stub.StreamObserver
import io.grpc.{Status, StatusRuntimeException}
import monix.execution.Scheduler

import scala.concurrent.Future

class BlocksApiGrpcImpl(commonApi: CommonBlocksApi)(implicit sc: Scheduler) extends BlocksApiGrpc.BlocksApi {

  override def getCurrentHeight(request: Empty): Future[UInt32Value] = {
    Future.successful(UInt32Value(commonApi.currentHeight))
  }

  override def getBlockRange(request: BlockRangeRequest, responseObserver: StreamObserver[BlockWithHeight]): Unit = responseObserver.interceptErrors {
    request.filter.generator.foreach(_.toAddress) // exception during conversion will be propagated to the client

    val stream =
      if (request.includeTransactions)
        commonApi
          .blocksRange(request.fromHeight, request.toHeight)
          .map { case (block, height) => BlockWithHeight(Some(block.toPB), height) } else
        commonApi
          .metaRange(request.fromHeight, request.toHeight)
          .map { meta =>
            BlockWithHeight(Some(PBBlock(Some(meta.header.toPBHeader), meta.signature)), meta.height)
          }

    responseObserver.completeWith(request.filter.generator match {
      case Some(generator) => stream.filter(_.block.exists(_.header.exists(h => h.generator.toAddress == generator.toAddress)))
      case None            => stream
    })
  }

  override def getBlock(request: BlockRequest): Future[BlockWithHeight] = Future {
    val result = request.request match {
      case Request.BlockId(blockId) =>
        commonApi
          .block(blockId)
          .map { case (block, height) => BlockWithHeight(Some(block.toPB), height) }

      case Request.Height(height) =>
        val actualHeight = if (height > 0) height else commonApi.currentHeight + height
        commonApi
          .blockAtHeight(actualHeight)
          .map(block => BlockWithHeight(Some(block.toPB), actualHeight))

      case Request.Reference(_) =>
        throw new StatusRuntimeException(Status.UNIMPLEMENTED)

      case Request.Empty =>
        None
    }

    val finalResult = if (request.includeTransactions) result else result.map(_.update(_.block.transactions := Nil))
    finalResult.explicitGetErr(BlockDoesNotExist)
  }
}
