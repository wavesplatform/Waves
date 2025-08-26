package com.wavesplatform.bls

import com.wavesplatform.common.utils.Base64
import play.api.libs.json.{Format, Reads}

opaque type BlsSignature = Array[Byte]

object BlsSignature {
  def apply(x: Array[Byte]): BlsSignature = x

  extension (self: BlsSignature) {
    def arr: Array[Byte] = self

    def base64Raw: String = Base64.encode(arr) // TODO: cache?
  }

  given Format[BlsSignature] = Format.of[String].bimap(Base64.decode, _.base64Raw)
}
