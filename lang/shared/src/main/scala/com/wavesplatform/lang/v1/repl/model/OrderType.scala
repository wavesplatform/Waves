package com.wavesplatform.lang.v1.repl.model

sealed trait OrderType
case object Buy  extends OrderType
case object Sell extends OrderType