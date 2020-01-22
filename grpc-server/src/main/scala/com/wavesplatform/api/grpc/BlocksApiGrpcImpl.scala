package com.wavesplatform.api.grpc

import com.google.protobuf.empty.Empty
import com.google.protobuf.wrappers.UInt32Value
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.grpc.BlockRequest.Request
import com.wavesplatform.api.http.ApiError.BlockDoesNotExist
import com.wavesplatform.protobuf.block.PBBlock
import com.wavesplatform.state.Blockchain
import io.grpc.stub.StreamObserver
import io.grpc.{Status, StatusRuntimeException}
import monix.execution.Scheduler

import scala.concurrent.Future

class BlocksApiGrpcImpl(blockchain: Blockchain)(implicit sc: Scheduler) extends BlocksApiGrpc.BlocksApi {
  private[this] val commonApi = new CommonBlocksApi(blockchain)

  override def getCurrentHeight(request: Empty): Future[UInt32Value] = {
    Future.successful(UInt32Value(commonApi.currentHeight()))
  }

  override def getBlockRange(request: BlockRangeRequest, responseObserver: StreamObserver[BlockWithHeight]): Unit = responseObserver.interceptErrors {
    request.filter.generator.foreach(_.toAddress) // exception during conversion will be propagated to the client

    val stream =
      if (request.includeTransactions)
        commonApi
          .blocksRange(request.fromHeight, request.toHeight)
          .map { case (block, height) => BlockWithHeight(Some(block.toPB), height) } else
        commonApi
          .blockHeadersRange(request.fromHeight, request.toHeight)
          .map { case (header, _, height) => BlockWithHeight(Some(PBBlock(Some(header.toPBHeader), header.signerData.signature)), height) }

    responseObserver.completeWith(request.filter.generator match {
      case Some(generator) => stream.filter(_.block.exists(_.header.exists(h => h.generator.toAddress == generator.toAddress)))
      case None            => stream
    })
  }

  override def getBlock(request: BlockRequest): Future[BlockWithHeight] = Future {
    val result = request.request match {
      case Request.BlockId(blockId) =>
        commonApi
          .blockBySignature(blockId)
          .map(block => BlockWithHeight(Some(block.toPB), blockchain.heightOf(block.uniqueId).get))

      case Request.Height(height) =>
        val actualHeight = if (height > 0) height else blockchain.height + height
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
