package com.wavesplatform.features

sealed trait FeatureStatus {
  def status: Byte
}

object FeatureStatus {

  case object Defined extends FeatureStatus {
    override def status: Byte = 0
  }

  case object Accepted extends FeatureStatus {
    override def status: Byte = 1
  }

  case object Activated extends FeatureStatus {
    override def status: Byte = 2
  }

  def apply(status: Byte): FeatureStatus = status match {
    case 1 => Accepted
    case 2 => Activated
    case _ => Defined
  }
}
