package com.wavesplatform.features.api

sealed trait NodeFeatureStatus

object NodeFeatureStatus{
  case object Unsupported extends NodeFeatureStatus
  case object Supported extends NodeFeatureStatus
}
