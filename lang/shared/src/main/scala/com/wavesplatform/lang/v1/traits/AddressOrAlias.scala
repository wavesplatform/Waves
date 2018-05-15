package com.wavesplatform.lang.v1.traits

import scodec.bits.ByteVector

trait AddressOrAlias {
  def byteVector: ByteVector
}
