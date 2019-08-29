package com.wavesplatform.lang.v1.repl.model

import com.fasterxml.jackson.annotation.JsonCreator

sealed trait OrderStatus
case object Accepted        extends OrderStatus
case object Filled          extends OrderStatus
case object PartiallyFilled extends OrderStatus
case object Canceled        extends OrderStatus
case object NotFound        extends OrderStatus

object OrderStatus {
  @JsonCreator
  def fromString(json: String): OrderStatus =
    if (json == null) null
    else json.toUpperCase match {
      case "ACCEPTED"         => Accepted
      case "ORDERACCEPTED"    => Accepted
      case "FILLED"           => Filled
      case "PARTIALLYFILLED"  => PartiallyFilled
      case "CANCELLED"        => Canceled
      case "NOTFOUND"         => NotFound
      case _                  => throw new IllegalArgumentException("Bad status value: " + json)
    }
}