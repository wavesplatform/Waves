package com.wavesplatform.events

package object protobuf {
  type PBStateUpdate = com.wavesplatform.events.protobuf.StateUpdate
  val PBStateUpdate = com.wavesplatform.events.protobuf.StateUpdate

  type PBBlockchainUpdated = com.wavesplatform.events.protobuf.BlockchainUpdated
  val PBBlockchainUpdated = com.wavesplatform.events.protobuf.BlockchainUpdated

  type VanillaStateUpdate = com.wavesplatform.state.StateUpdate
  val VanillaStateUpdate = com.wavesplatform.state.StateUpdate

  type VanillaBlockchainUpdated = com.wavesplatform.state.BlockchainUpdated
}
