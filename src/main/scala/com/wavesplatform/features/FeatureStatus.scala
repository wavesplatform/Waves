package com.wavesplatform.features

sealed trait FeatureStatus {
  def value: Int
}

case object FeatureDefined extends FeatureStatus {
  val value = 0
}

case object FeatureAccepted extends FeatureStatus {
  val value = 1
}

case object FeatureActivated extends FeatureStatus {
  val value = 2
}