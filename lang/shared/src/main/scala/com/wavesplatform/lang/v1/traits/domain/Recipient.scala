package com.wavesplatform.lang.v1.traits.domain

import com.wavesplatform.common.state.ByteStr

sealed trait Recipient
object Recipient {
  case class Address(bytes: ByteStr) extends Recipient
  case class Alias(name: String)     extends Recipient
}
