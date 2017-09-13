package com.wavesplatform.features

trait FeatureProvider {
  def status(feature: Int): FeatureStatus
}
