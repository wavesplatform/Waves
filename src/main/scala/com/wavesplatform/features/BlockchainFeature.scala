package com.wavesplatform.features

case class BlockchainFeature private(id: Short)

object BlockchainFeatures {

  val SmallerMinimalGeneratingBalance = BlockchainFeature(1)
  val NG = BlockchainFeature(2)
  val MassTransfer = BlockchainFeature(3)

  //TODO: MassTransfer: add MassTransfer to implemented to actvate
  val implemented: Set[Short] = Set(SmallerMinimalGeneratingBalance, NG).map(_.id)

}
