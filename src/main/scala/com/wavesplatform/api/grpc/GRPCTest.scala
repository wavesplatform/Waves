package com.wavesplatform.api.grpc
import com.wavesplatform.account.Address
import io.grpc.ManagedChannelBuilder

object GRPCTest extends App {
  val channel = ManagedChannelBuilder
    .forAddress("localhost", 1234)
    .usePlaintext()
    .build()

  val service = TransactionsApiGrpc.blockingStub(channel)
  val result  = service.transactionsByAddress(TransactionsByAddressRequest(Address.fromString("3NBVqYXrapgJP9atQccdBPAgJPwHDKkh6A8").right.get))
  result.foreach(println)
}
