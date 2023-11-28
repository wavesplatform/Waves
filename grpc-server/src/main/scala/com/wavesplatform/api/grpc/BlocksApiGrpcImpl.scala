package com.wavesplatform.api.grpc

import com.google.protobuf.ByteString
import com.google.protobuf.empty.Empty
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.grpc.BlockRangeRequest.Filter
import com.wavesplatform.api.grpc.BlockRequest.Request
import com.wavesplatform.api.http.ApiError.BlockDoesNotExist
import com.wavesplatform.protobuf.*
import com.wavesplatform.protobuf.block.PBBlock
import com.wavesplatform.state.TxMeta
import com.wavesplatform.transaction.Transaction
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler

import scala.concurrent.Future

class BlocksApiGrpcImpl(commonApi: CommonBlocksApi)(implicit sc: Scheduler) extends BlocksApiGrpc.BlocksApi {
  import BlocksApiGrpcImpl.*

  override def getCurrentHeight(request: Empty): Future[Int] = {
    Future.successful(commonApi.currentHeight)
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
          .map(toBlockWithHeight)

    responseObserver.completeWith(request.filter match {
      case Filter.GeneratorPublicKey(publicKey) => stream.filter(_.getBlock.getHeader.generator.toPublicKey == publicKey.toPublicKey)
      case Filter.GeneratorAddress(address)     => stream.filter(_.getBlock.getHeader.generator.toAddress() == address.toAddress())
      case Filter.Empty                         => stream
    })
  }

  override def getBlock(request: BlockRequest): Future[BlockWithHeight] = Future {
    (request.request match {
      case Request.BlockId(blockId) =>
        if (request.includeTransactions)
          commonApi
            .block(blockId.toByteStr)
            .map(toBlockWithHeight)
        else commonApi.meta(blockId.toByteStr).map(toBlockWithHeight)

      case Request.Height(height) =>
        val actualHeight = if (height > 0) height else commonApi.currentHeight + height
        if (request.includeTransactions)
          commonApi
            .blockAtHeight(actualHeight)
            .map(toBlockWithHeight)
        else commonApi.metaAtHeight(actualHeight).map(toBlockWithHeight)

      case Request.Empty =>
        None
    }).explicitGetErr(BlockDoesNotExist)
  }
}

object BlocksApiGrpcImpl {
  private def toBlockWithHeight(v: (BlockMeta, Seq[(TxMeta, Transaction)])) = {
    val (blockMeta, txs) = v

    BlockWithHeight(
      Some(PBBlock(Some(blockMeta.header.toPBHeader), blockMeta.signature.toByteString, txs.map(_._2.toPB))),
      blockMeta.height,
      blockMeta.vrf.fold(ByteString.EMPTY)(_.toByteString),
      blockMeta.rewardShares.map { case (addr, reward) => RewardShare(ByteString.copyFrom(addr.bytes), reward) }
    )
  }

  private def toBlockWithHeight(m: BlockMeta) =
    BlockWithHeight(
      Some(PBBlock(Some(m.header.toPBHeader), m.signature.toByteString)),
      m.height,
      m.vrf.fold(ByteString.EMPTY)(_.toByteString),
      m.rewardShares.map { case (addr, reward) => RewardShare(ByteString.copyFrom(addr.bytes), reward) }
    )
}
