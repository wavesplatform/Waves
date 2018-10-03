package com.wavesplatform.lang.v1.traits.domain

sealed trait OrdType
object OrdType {
  case object Buy  extends OrdType
  case object Sell extends OrdType
}
