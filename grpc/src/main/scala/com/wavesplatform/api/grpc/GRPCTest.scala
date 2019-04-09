package com.wavesplatform.api.grpc

import com.wavesplatform.account.Address
import com.wavesplatform.protobuf.transaction.PBRecipients
import io.grpc.ManagedChannelBuilder

object GRPCTest extends App {
  val channel = ManagedChannelBuilder
    .forAddress("localhost", 6870)
    .usePlaintext()
    .build()

  val service = TransactionsApiGrpc.blockingStub(channel)
  val result = service.getTransactionsByAddress(
    TransactionsByAddressRequest(Some(PBRecipients.create(Address.fromString("3NBVqYXrapgJP9atQccdBPAgJPwHDKkh6A8").right.get))))
  result.take(100).foreach(println)

  val blocksService = BlocksApiGrpc.blockingStub(channel)
  val result1       = blocksService.getBlocksRange(BlocksRangeRequest(100, 200))
  result1.foreach(println)
}
