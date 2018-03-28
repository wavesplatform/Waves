package com.wavesplatform.utils

sealed abstract class ApplicationStopReason(val code: Int)
case object Default            extends ApplicationStopReason(1)
case object UnsupportedFeature extends ApplicationStopReason(38)
