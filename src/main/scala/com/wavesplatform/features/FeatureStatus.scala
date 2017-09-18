package com.wavesplatform.features

sealed trait FeatureStatus {
  def status: Byte
}

case object FeatureDefined extends FeatureStatus {
  override val status = 0
}

case object FeatureAccepted extends FeatureStatus {
  override val status = 1
}

case object FeatureActivated extends FeatureStatus {
  override val status = 2
}

object FeatureStatus {
  def apply(status: Byte): FeatureStatus = status match {
    case 1 => FeatureAccepted
    case 2 => FeatureActivated
    case _ => FeatureDefined
  }
}
