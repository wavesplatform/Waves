package com.wavesplatform.features

trait BlockchainFunctionality {
  def available(): Boolean
}

class BlockchainFunctionalities(val provider: FeatureProvider) {
  import BlockchainFunctionalities._

  val smallerMinimalGeneratingBalance: BlockchainFunctionality = () => activated(provider, 1)
  val ng: BlockchainFunctionality = () => activated(provider, 2)
}

object BlockchainFunctionalities {
  private def activated(provider: FeatureProvider, id: Short): Boolean = provider.status(id) == FeatureStatus.Activated
}