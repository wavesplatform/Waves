package com.wavesplatform.events

package object protobuf {
  type PBStateUpdated = com.wavesplatform.events.protobuf.StateUpdated
  val PBStateUpdated = com.wavesplatform.events.protobuf.StateUpdated

  type PBBlockchainUpdated = com.wavesplatform.events.protobuf.BlockchainUpdated
  val PBBlockchainUpdated = com.wavesplatform.events.protobuf.BlockchainUpdated

  type VanillaStateUpdated = com.wavesplatform.state.StateUpdated
  val VanillaStateUpdated = com.wavesplatform.state.StateUpdated

  type VanillaBlockchainUpdated = com.wavesplatform.state.BlockchainUpdated
}
