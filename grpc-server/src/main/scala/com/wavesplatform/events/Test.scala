package com.wavesplatform.events

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.events.api.grpc.protobuf.{BlockchainUpdatesApiGrpc, SubscribeRequest}
import com.wavesplatform.events.protobuf.serde._
import io.grpc.{Channel, ManagedChannelBuilder}

object Test extends App {
  AddressScheme.current = new AddressScheme {
    val chainId: Byte = 'E'
  }

  val channel: Channel  = ManagedChannelBuilder.forAddress("localhost", 6881).usePlaintext().build()
  val blockchainUpdates = BlockchainUpdatesApiGrpc.blockingStub(channel)

  blockchainUpdates.subscribe(SubscribeRequest(1)).foreach { event =>
    println(event.getUpdate.vanilla.get)
  }
}
