package com.wavesplatform.features

sealed trait BlockchainFeature {
  def id: Short
}

object BlockchainFeatures {
  case object SmallerMinimalGeneratingBalance extends BlockchainFeature {
    override def id: Short = 1
  }

  case object NG extends BlockchainFeature {
    override def id: Short = 2
  }

  def implemented: Set[Short] = Set(
    SmallerMinimalGeneratingBalance.id,
    NG.id
  )
}
