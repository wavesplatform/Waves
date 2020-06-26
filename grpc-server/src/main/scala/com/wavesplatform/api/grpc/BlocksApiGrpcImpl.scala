package com.wavesplatform.api.grpc

import com.google.protobuf.empty.Empty
import com.google.protobuf.wrappers.UInt32Value
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.grpc.BlockRangeRequest.Filter
import com.wavesplatform.api.grpc.BlockRequest.Request
import com.wavesplatform.api.http.ApiError.BlockDoesNotExist
import com.wavesplatform.protobuf.block.PBBlock
import com.wavesplatform.transaction.Transaction
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler

import scala.concurrent.Future

class BlocksApiGrpcImpl(commonApi: CommonBlocksApi)(implicit sc: Scheduler) extends BlocksApiGrpc.BlocksApi {
  import BlocksApiGrpcImpl._

  override def getCurrentHeight(request: Empty): Future[UInt32Value] = {
    Future.successful(UInt32Value(commonApi.currentHeight))
  }

  override def getBlockRange(request: BlockRangeRequest, responseObserver: StreamObserver[BlockWithHeight]): Unit = responseObserver.interceptErrors {
    val stream =
      if (request.includeTransactions)
        commonApi
          .blocksRange(request.fromHeight, request.toHeight)
          .map(toBlockWithHeight)
      else
        commonApi
          .metaRange(request.fromHeight, request.toHeight)
          .map { meta =>
            BlockWithHeight(Some(PBBlock(Some(meta.header.toPBHeader), meta.signature)), meta.height)
          }

    responseObserver.completeWith(request.filter match {
      case Filter.GeneratorPublicKey(publicKey) => stream.filter(_.getBlock.getHeader.generator.toPublicKey == publicKey.toPublicKey)
      case Filter.GeneratorAddress(address)     => stream.filter(_.getBlock.getHeader.generator.toAddress == address.toAddress)
      case Filter.Empty                         => stream
    })
  }

  override def getBlock(request: BlockRequest): Future[BlockWithHeight] = Future {
    val result = request.request match {
      case Request.BlockId(blockId) =>
        commonApi
          .block(blockId)
          .map(toBlockWithHeight)

      case Request.Height(height) =>
        val actualHeight = if (height > 0) height else commonApi.currentHeight + height
        commonApi
          .blockAtHeight(actualHeight)
          .map(toBlockWithHeight)

      case Request.Empty =>
        None
    }

    val finalResult = if (request.includeTransactions) result else result.map(_.update(_.block.transactions := Nil))
    finalResult.explicitGetErr(BlockDoesNotExist)
  }
}

object BlocksApiGrpcImpl {
  private def toBlockWithHeight(v: (BlockMeta, Seq[(Transaction, Boolean)])) = {
    BlockWithHeight(Some(PBBlock(Some(v._1.header.toPBHeader), v._1.signature.toPBByteString, v._2.map(_._1.toPB))), v._1.height)
  }
}
