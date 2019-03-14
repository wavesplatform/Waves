package com.wavesplatform.api.grpc

import com.google.protobuf.empty.Empty
import com.google.protobuf.wrappers.{UInt32Value, UInt64Value}
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.http.BlockDoesNotExist
import com.wavesplatform.protobuf.block.PBBlock
import com.wavesplatform.state.Blockchain
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler

import scala.concurrent.Future

class BlocksApiGrpcImpl(blockchain: Blockchain)(implicit sc: Scheduler) extends BlocksApiGrpc.BlocksApi {
  private[this] val commonApi = new CommonBlocksApi(blockchain)

  override def getBlocksByAddress(request: BlocksByAddressRequest, responseObserver: StreamObserver[BlockAndHeight]): Unit = {
    val blocks = commonApi
      .blocksByAddress(request.getAddress.toAddress, request.fromHeight, request.toHeight)
      .map { case (block, height) => BlockAndHeight(Some(block.toPB), height) }

    responseObserver.completeWith(blocks)
  }

  override def getChildBlock(request: BlockIdRequest): Future[PBBlock] = {
    commonApi
      .childBlock(request.blockId)
      .map(_.toPB)
      .toFuture(BlockDoesNotExist)
  }

  override def calcBlocksDelay(request: BlocksDelayRequest): Future[UInt64Value] = {
    commonApi.calcBlocksDelay(request.blockId, request.blockNum)
      .map(UInt64Value(_))
      .toFuture
  }

  override def getBlockHeight(request: BlockIdRequest): Future[UInt32Value] = {
    commonApi
      .blockHeight(request.blockId)
      .map(UInt32Value(_))
      .toFuture(BlockDoesNotExist)
  }

  override def getCurrentHeight(request: Empty): Future[UInt32Value] = {
    Future.successful(UInt32Value(commonApi.currentHeight()))
  }

  override def getBlockAtHeight(request: UInt32Value): Future[PBBlock] = {
    commonApi
      .blockAtHeight(request.value)
      .map(_.toPB)
      .toFuture(BlockDoesNotExist)
  }

  override def getBlockHeaderAtHeight(request: UInt32Value): Future[PBBlock.SignedHeader] = {
    commonApi
      .blockHeaderAtHeight(request.value)
      .map { case (header, _) => header.toPBHeader }
      .toFuture(BlockDoesNotExist)
  }

  override def getBlocksRange(request: BlocksRangeRequest, responseObserver: StreamObserver[PBBlock]): Unit = {
    val stream = commonApi
      .blocksRange(request.fromHeight, request.toHeight)
      .map { case (_, block) => block.toPB }

    responseObserver.completeWith(stream)
  }

  override def getBlockHeadersRange(request: BlocksRangeRequest, responseObserver: StreamObserver[PBBlock.SignedHeader]): Unit = {
    val stream = commonApi
      .blockHeadersRange(request.fromHeight, request.toHeight)
      .map { case (_, bh, _) => bh.toPBHeader }

    responseObserver.completeWith(stream)
  }

  override def getLastBlock(request: Empty): Future[PBBlock] = {
    commonApi
      .lastBlock()
      .map(_.toPB)
      .toFuture
  }

  override def getLastBlockHeader(request: Empty): Future[PBBlock.SignedHeader] = {
    commonApi
      .lastBlockHeader()
      .map(_._1.toPBHeader)
      .toFuture
  }

  override def getFirstBlock(request: Empty): Future[PBBlock] = {
    Future.successful(commonApi.firstBlock().toPB)
  }

  override def getBlockBySignature(request: BlockIdRequest): Future[PBBlock] = {
    commonApi
      .blockBySignature(request.blockId)
      .map(_.toPB)
      .toFuture
  }
}
