package com.wavesplatform

import com.wavesplatform.events.api.grpc.protobuf.{BlockchainUpdatesApiGrpc, SubscribeRequest}
import io.grpc.{Channel, ManagedChannelBuilder}

object Test extends App {
  val channel: Channel = ManagedChannelBuilder.forAddress("localhost", 6881)
    .usePlaintext()
    .build()

  val service = BlockchainUpdatesApiGrpc.blockingStub(channel)
  service.subscribe(SubscribeRequest.of(1, Int.MaxValue)).foreach { se ⇒
    println(se.getUpdate.height)
  }
}
