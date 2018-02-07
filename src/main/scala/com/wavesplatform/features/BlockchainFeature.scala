package com.wavesplatform.features

case class BlockchainFeature private(id: Short)

object BlockchainFeatures {

  val SmallerMinimalGeneratingBalance = BlockchainFeature(1)
  val NG = BlockchainFeature(2)

  val implemented: Set[Short] = Set(SmallerMinimalGeneratingBalance, NG).map(_.id)

}
