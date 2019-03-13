package com.wavesplatform.api.grpc
import com.wavesplatform.account.Address
import com.wavesplatform.protobuf.transaction.Recipient
import io.grpc.ManagedChannelBuilder

object GRPCTest extends App {
  val channel = ManagedChannelBuilder
    .forAddress("localhost", 1234)
    .usePlaintext()
    .build()

  val service = TransactionsApiGrpc.blockingStub(channel)
  val result = service.transactionsByAddress(
    TransactionsByAddressRequest(
      Some(Recipient().withAddress(Address.fromString("3NBVqYXrapgJP9atQccdBPAgJPwHDKkh6A8").right.get.bytes.toPBByteString))))
  result.take(100).foreach(println)

  val blocksService = BlocksApiGrpc.blockingStub(channel)
  val result1       = blocksService.blocksRange(BlocksRangeRequest(100, 200))
  result1.foreach(println)
}
