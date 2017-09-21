package com.wavesplatform.features

import scala.language.implicitConversions

trait FeatureProvider {
  def status(feature: Short): FeatureStatus
}

object FeatureProviderExtensions {

  class ExtendedFeatureProvider(provider: FeatureProvider) {
    def activated(id: Short): Boolean = {
      provider.status(id) == FeatureStatus.Activated
    }
  }

  implicit def extend(provider: FeatureProvider): ExtendedFeatureProvider = new ExtendedFeatureProvider(provider)
}