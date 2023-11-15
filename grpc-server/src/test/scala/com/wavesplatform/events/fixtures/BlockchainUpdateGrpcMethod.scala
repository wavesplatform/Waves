package com.wavesplatform.events.fixtures

sealed trait BlockchainUpdateGrpcMethod

object BlockchainUpdateGrpcMethod {
  case object Subscribe extends BlockchainUpdateGrpcMethod
  case object GetBlockUpdate extends BlockchainUpdateGrpcMethod
  case object GetBlockUpdateRange extends BlockchainUpdateGrpcMethod
}
