package com.wavesplatform.api.grpc

import io.grpc.ManagedChannelBuilder

object GRPCTest extends App {
  val channel = ManagedChannelBuilder.forAddress("123", 23).build()
  val blocksApi = BlocksApiGrpc.blockingStub(channel)
  val block = blocksApi.getBlock(BlockRequest(includeTransactions = true, BlockRequest.Request.Height(123)))
  println(block)
}
