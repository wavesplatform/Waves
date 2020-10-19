package com.wavesplatform

import com.wavesplatform.events.api.grpc.protobuf.{BlockchainUpdatesApiGrpc, SubscribeRequest}
import io.grpc.{Channel, ManagedChannelBuilder}

object Test extends App {
  val channel: Channel = ManagedChannelBuilder.forAddress("localhost", 6881)
    .usePlaintext()
    .build()

  val service = BlockchainUpdatesApiGrpc.blockingStub(channel)
  var lastHeight = 0
  service.subscribe(SubscribeRequest.of(1, Int.MaxValue)).foreach { se ⇒
    val currentHeight = se.getUpdate.height
    if (currentHeight > lastHeight + 1) {
      println(s"Error: $lastHeight -> $currentHeight")
      sys.exit(1)
    }
    println(currentHeight)
    lastHeight = currentHeight
  }
}
