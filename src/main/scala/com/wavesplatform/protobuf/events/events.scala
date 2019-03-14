package com.wavesplatform.protobuf

package object events {
  type PBStateUpdated = com.wavesplatform.protobuf.events.StateUpdated
  val PBStateUpdated = com.wavesplatform.protobuf.events.StateUpdated

  type PBBlockchainUpdated = com.wavesplatform.protobuf.events.BlockchainUpdated
  val PBBlockchainUpdated = com.wavesplatform.protobuf.events.BlockchainUpdated

  type VanillaStateUpdated = com.wavesplatform.state.StateUpdated
  val VanillaStateUpdated = com.wavesplatform.state.StateUpdated

  type VanillaBlockchainUpdated = com.wavesplatform.state.BlockchainUpdated
}
