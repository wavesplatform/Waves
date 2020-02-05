package com.wavesplatform.serialization

import monix.eval.Coeval

trait BytesSerializable {
  val bytes: Coeval[Array[Byte]]
}
