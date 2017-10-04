package com.wavesplatform.features.api

trait NodeFeatureStatus

object NodeFeatureStatus{
  case object Unsupported extends NodeFeatureStatus
  case object Supported extends NodeFeatureStatus
}