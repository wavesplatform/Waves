package com.wavesplatform.api.grpc

import com.google.protobuf.empty.Empty
import com.google.protobuf.wrappers.{UInt32Value, UInt64Value}
import com.wavesplatform.account.PublicKey
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.grpc.BlockRequest.Request
import com.wavesplatform.api.http.BlockDoesNotExist
import com.wavesplatform.protobuf.block.PBBlock
import com.wavesplatform.state.Blockchain
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler

import scala.concurrent.Future

class BlocksApiGrpcImpl(blockchain: Blockchain)(implicit sc: Scheduler) extends BlocksApiGrpc.BlocksApi {
  private[this] val commonApi = new CommonBlocksApi(blockchain)

  override def calcBlocksDelay(request: BlocksDelayRequest): Future[UInt64Value] = {
    commonApi
      .calcBlocksDelay(request.blockId, request.blockNum)
      .map(UInt64Value(_))
      .toFuture
  }

  override def getCurrentHeight(request: Empty): Future[UInt32Value] = {
    Future.successful(UInt32Value(commonApi.currentHeight()))
  }

  override def getBlocksRange(request: BlocksRangeRequest, responseObserver: StreamObserver[BlockAndHeight]): Unit = {
    val stream = if (request.includeTransactions) {
      commonApi
        .blocksRange(request.fromHeight, request.toHeight)
        .map { case (block, height) => BlockAndHeight(Some(block.toPB), height) }
    } else {
      commonApi
        .blockHeadersRange(request.fromHeight, request.toHeight)
        .map { case (header, _, height) => BlockAndHeight(Some(PBBlock(Some(header.toPBHeader), header.signerData.signature)), height) }
    }

    val filteredStream = stream.filter {
      case BlockAndHeight(Some(PBBlock(Some(header), _, _)), _) =>
        request.filter match {
          case BlocksRangeRequest.Filter.Generator(generator) =>
            header.generator == generator || PublicKey(header.generator.toByteArray).toAddress.bytes == generator.toByteStr
          case BlocksRangeRequest.Filter.Empty => true
        }

      case _ => true
    }

    responseObserver.completeWith(filteredStream)
  }

  override def getBlock(request: BlockRequest): Future[BlockAndHeight] = {
    val result = request.request match {
      case Request.BlockId(blockId) =>
        commonApi
          .blockBySignature(blockId)
          .map(block => BlockAndHeight(Some(block.toPB), blockchain.heightOf(block.uniqueId).get))

      case Request.Height(height) =>
        commonApi
          .blockAtHeight(if (height > 0) height else blockchain.height + height)
          .toRight(BlockDoesNotExist)
          .map(block => BlockAndHeight(Some(block.toPB), height))

      case Request.ParentId(parentId) =>
        commonApi
          .childBlock(parentId)
          .toRight(BlockDoesNotExist)
          .map(block => BlockAndHeight(Some(block.toPB), blockchain.heightOf(block.uniqueId).get))

      case Request.Empty =>
        Right(BlockAndHeight.defaultInstance)
    }

    if (request.includeTransactions) {
      result.toFuture
    } else {
      result
        .map(_.update(_.block.transactions := Nil))
        .toFuture
    }
  }
}
