package com.wavesplatform.features

sealed trait BlockchainFeatureStatus

object BlockchainFeatureStatus{
  case object Undefined extends BlockchainFeatureStatus
  case object Accepted extends BlockchainFeatureStatus
  case object Activated extends BlockchainFeatureStatus

  def promote(status: BlockchainFeatureStatus): BlockchainFeatureStatus = {
    status match {
      case Undefined => Accepted
      case Accepted => Activated
      case Activated => Activated
    }
  }
}

