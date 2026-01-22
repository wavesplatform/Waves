package com.wavesplatform.crypto.bls

object TestBlsKeyPair {
  def unsafe(wavesPrivateKey: Array[Byte]): BlsKeyPair = new BlsSeedKeyPair(wavesPrivateKey)
}
