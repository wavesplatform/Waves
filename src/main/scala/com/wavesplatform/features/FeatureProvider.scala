package com.wavesplatform.features

trait FeatureProvider {
  def status(feature: Short): FeatureStatus
}
