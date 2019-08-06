package com.wavesplatform.api.grpc

import com.google.protobuf.empty.Empty
import com.google.protobuf.wrappers.UInt32Value
import com.wavesplatform.account.PublicKey
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.grpc.BlockRequest.Request
import com.wavesplatform.api.http.ApiError.BlockDoesNotExist
import com.wavesplatform.protobuf.block.PBBlock
import com.wavesplatform.state.Blockchain
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler

import scala.concurrent.Future

class BlocksApiGrpcImpl(blockchain: Blockchain)(implicit sc: Scheduler) extends BlocksApiGrpc.BlocksApi {
  private[this] val commonApi = new CommonBlocksApi(blockchain)

  override def getCurrentHeight(request: Empty): Future[UInt32Value] = {
    Future.successful(UInt32Value(commonApi.currentHeight()))
  }

  override def getBlockRange(request: BlockRangeRequest, responseObserver: StreamObserver[BlockWithHeight]): Unit = {
    val stream = if (request.includeTransactions) {
      commonApi
        .blocksRange(request.fromHeight, request.toHeight)
        .map { case (block, height) => BlockWithHeight(Some(block.toPB), height) }
    } else {
      commonApi
        .blockHeadersRange(request.fromHeight, request.toHeight)
        .map { case (header, _, height) => BlockWithHeight(Some(PBBlock(Some(header.toPBHeader), header.signerData.signature)), height) }
    }

    val filteredStream = stream.filter {
      case BlockWithHeight(Some(PBBlock(Some(header), _, _)), _) =>
        request.filter match {
          case BlockRangeRequest.Filter.Generator(generator) =>
            header.generator == generator || PublicKey(header.generator.toByteArray).toAddress.bytes == generator.toByteStr
          case BlockRangeRequest.Filter.Empty => true
        }

      case _ => true
    }

    responseObserver.completeWith(filteredStream)
  }

  override def getBlock(request: BlockRequest): Future[BlockWithHeight] = Future {
    val result = request.request match {
      case Request.BlockId(blockId) =>
        commonApi
          .blockBySignature(blockId)
          .map(block => BlockWithHeight(Some(block.toPB), blockchain.heightOf(block.uniqueId).get))

      case Request.Height(height) =>
        commonApi
          .blockAtHeight(if (height > 0) height else blockchain.height + height)
          .map(block => BlockWithHeight(Some(block.toPB), height))

      case Request.Empty =>
        None
    }

    val finalResult = if (request.includeTransactions) result else result.map(_.update(_.block.transactions := Nil))
    finalResult.explicitGetErr(BlockDoesNotExist)
  }
}
