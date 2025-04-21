package com.wavesplatform.common

package object merkle {
  type LeafData = Array[Byte]
  type Message  = Array[Byte]
  type Digest   = Array[Byte]
  type Level    = Seq[Digest]
}
