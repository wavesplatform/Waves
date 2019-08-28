package com.wavesplatform.lang.v1.repl.model

sealed trait OrderStatus
case object Accepted        extends OrderStatus
case object Filled          extends OrderStatus
case object PartiallyFilled extends OrderStatus
case object Canceled        extends OrderStatus
case object NotFound        extends OrderStatus