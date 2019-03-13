package com.wavesplatform.api.grpc

import com.google.protobuf.empty.Empty
import com.google.protobuf.wrappers.{UInt32Value, UInt64Value}
import com.wavesplatform.api.http.{ApiError, BlockDoesNotExist}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.block.PBBlock
import com.wavesplatform.state.Blockchain
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable

import scala.concurrent.Future

class BlocksApiGrpcImpl(blockchain: Blockchain)
    extends BlocksApiGrpc.BlocksApi {

  override def blocksByAddress(request: BlocksByAddressRequest, responseObserver: StreamObserver[BlockAndHeight]): Unit = {
    val address = request.getAddress.toAddress
    val blocks = Observable
      .fromIterable(request.fromHeight to request.toHeight)
      .map(height => (blockchain.blockAt(height), height))
      .filter(_._1.isDefined)
      .map(pair => (pair._1.get, pair._2))
      .filter(_._1.signerData.generator.toAddress == address)
      .map(pair => BlockAndHeight(Some(pair._1.toPB), pair._2))
    responseObserver.completeWith(blocks)
  }

  override def childBlock(request: BlockIdRequest): Future[PBBlock] = {
    val childBlock = for {
      h <- blockchain.heightOf(request.blockId)
      b <- blockchain.blockAt(h + 1)
    } yield b.toPB

    childBlock.toFuture
  }

  override def blocksDelay(request: BlocksDelayRequest): Future[UInt64Value] = {
    val result = withBlock(request.blockId).flatMap { block =>
      blockchain
        .parent(block.toVanilla, request.blockNum)
        .map(parent => UInt64Value((block.getHeader.getHeader.timestamp - parent.timestamp) / request.blockNum))
        .toRight(BlockDoesNotExist)
    }

    result.toFuture
  }

  override def blockHeight(request: BlockIdRequest): Future[UInt32Value] = {
    blockchain
      .heightOf(request.blockId)
      .map(UInt32Value(_))
      .toFuture
  }

  override def currentHeight(request: Empty): Future[UInt32Value] = {
    Future.successful(UInt32Value(blockchain.height))
  }

  override def blockAtHeight(request: UInt32Value): Future[PBBlock] = {
    blockchain.blockAt(request.value).map(_.toPB).toFuture
  }

  override def blockHeaderAtHeight(request: UInt32Value): Future[PBBlock.SignedHeader] = {
    blockchain.blockHeaderAndSize(request.value).map { case (header, _) => header.toPBHeader }.toFuture
  }

  override def blocksRange(request: BlocksRangeRequest, responseObserver: StreamObserver[PBBlock]): Unit = {
    val stream = Observable
      .fromIterable(request.fromHeight to request.toHeight)
      .map(height => blockchain.blockAt(height))
      .collect { case Some(block) => block.toPB }

    responseObserver.completeWith(stream)
  }

  override def blockHeadersRange(request: BlocksRangeRequest, responseObserver: StreamObserver[PBBlock.SignedHeader]): Unit = {
    val stream = Observable
      .fromIterable(request.fromHeight to request.toHeight)
      .map(height => blockchain.blockHeaderAndSize(height))
      .collect { case Some((header, _)) => header.toPBHeader }

    responseObserver.completeWith(stream)
  }

  override def lastBlock(request: Empty): Future[PBBlock] = {
    blockchain.lastBlock.map(_.toPB).toFuture
  }

  override def lastBlockHeader(request: Empty): Future[PBBlock.SignedHeader] = {
    blockchain.lastBlockHeaderAndSize
      .map(_._1.toPBHeader)
      .toFuture
  }

  override def firstBlock(request: Empty): Future[PBBlock] = {
    Future.successful(blockchain.genesis.toPB)
  }

  override def blockBySignature(request: BlockIdRequest): Future[PBBlock] = {
    withBlock(request.blockId).toFuture
  }

  private[this] def withBlock(signature: ByteStr): Either[ApiError, PBBlock] = {
    blockchain
      .blockById(signature)
      .toRight(BlockDoesNotExist)
      .map(_.toPB)
  }
}
