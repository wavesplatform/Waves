package com.wavesplatform

package object http {
  def sameSignature(target: Array[Byte])(actual: Array[Byte]): Boolean = target sameElements actual
}
