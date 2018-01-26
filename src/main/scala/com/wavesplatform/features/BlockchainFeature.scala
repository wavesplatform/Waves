package com.wavesplatform.features

case class BlockchainFeature private(id: Short)

object BlockchainFeatures {

  val SmallerMinimalGeneratingBalance = BlockchainFeature(1)
  val NG = BlockchainFeature(2)
  val MassTransfer = BlockchainFeature(3)

  val implemented: Set[Short] = Set(SmallerMinimalGeneratingBalance, NG, MassTransfer).map(_.id)



}
