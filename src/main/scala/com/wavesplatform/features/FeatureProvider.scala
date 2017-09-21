package com.wavesplatform.features

trait FeatureProvider {
  def status(feature: Short): FeatureStatus
}

object FeatureProvider {

  implicit class FeatureProviderExt(provider: FeatureProvider) {
    def activated(id: Short): Boolean = {
      provider.status(id) == FeatureStatus.Activated
    }
  }

}