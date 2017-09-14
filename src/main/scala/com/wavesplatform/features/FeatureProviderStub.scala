package com.wavesplatform.features

//TODO Remove when merged with NODE-105
class FeatureProviderStub extends FeatureProvider{
  def status(feature: Short): FeatureStatus = FeatureStatus.Defined
}
