package com.wavesplatform.utils

sealed abstract class ApplicationStopReason(val code: Int)
case object Default                  extends ApplicationStopReason(1)
case object ErrorStartingMatcher     extends ApplicationStopReason(10)
case object UnsupportedFeature       extends ApplicationStopReason(38)
case object Misconfiguration         extends ApplicationStopReason(61)
case object BaseTargetReachedMaximum extends ApplicationStopReason(72)
case object FatalDBError             extends ApplicationStopReason(74)
