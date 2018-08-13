package com.wavesplatform.lang.v1.traits.domain

import scodec.bits.ByteVector

trait Recipient
object Recipient {
  case class Address(bytes: ByteVector) extends Recipient
  case class Alias(name: String)        extends Recipient
}
