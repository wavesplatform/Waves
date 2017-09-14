package com.wavesplatform.features

sealed case class FeatureStatus(status: Byte)

object FeatureStatus {
  object Defined extends FeatureStatus(0)
  object Accepted extends FeatureStatus(1)
  object Activated extends FeatureStatus(2)

  val values = Seq(Defined, Accepted, Activated)
}
