package com.wavesplatform.features

sealed trait BlockchainFeatureStatus

object BlockchainFeatureStatus {
  case object Undefined extends BlockchainFeatureStatus
  case object Approved  extends BlockchainFeatureStatus
  case object Activated extends BlockchainFeatureStatus

  def promote(status: BlockchainFeatureStatus): BlockchainFeatureStatus = {
    status match {
      case Undefined => Approved
      case Approved  => Activated
      case Activated => Activated
    }
  }
}
