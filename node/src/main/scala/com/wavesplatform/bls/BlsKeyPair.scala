package com.wavesplatform.bls

import com.wavesplatform.account.PrivateKey as WavesPrivateKey
import com.wavesplatform.bls
import supranational.blst.SecretKey as BlstSecretKey

import java.util

sealed trait BlsKeyPair {
  def privateKey: BlsPrivateKey
  def publicKey: BlsPublicKey
}

object BlsKeyPair {
  def apply(wavesPrivateKey: WavesPrivateKey): BlsKeyPair = BlsSeedKeyPair(wavesPrivateKey.arr)
}

private final class BlsSeedKeyPair(val wavesPrivateKey: Array[Byte]) extends BlsKeyPair {
  lazy val privateKey: BlsPrivateKey = BlsSeedKeyPair.privateKeyFrom(wavesPrivateKey)
  lazy val publicKey: BlsPublicKey   = privateKey.createPublicKey()

  override def equals(other: Any): Boolean = other match {
    case other: BlsSeedKeyPair => util.Arrays.equals(other.wavesPrivateKey, wavesPrivateKey)
    case _                     => false
  }

  private lazy val hc          = util.Arrays.hashCode(wavesPrivateKey)
  override def hashCode(): Int = hc
}

private object BlsSeedKeyPair {
  private def privateKeyFrom(seed: Array[Byte]): BlsPrivateKey = {
    val sk = new BlstSecretKey()
    sk.keygen(seed)
    BlsPrivateKey(sk)
  }
}
