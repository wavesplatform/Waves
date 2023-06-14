package com.wavesplatform.events.fixtures

sealed trait BlockchainUpdateTrait

object BlockchainUpdateTrait {
  case object Subscribe extends BlockchainUpdateTrait
  case object GetBlockUpdate extends BlockchainUpdateTrait
  case object GetBlockUpdateRange extends BlockchainUpdateTrait
}
