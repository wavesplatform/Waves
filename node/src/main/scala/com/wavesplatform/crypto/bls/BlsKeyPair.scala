package com.wavesplatform.crypto.bls

import com.wavesplatform.account.PrivateKey as WavesPrivateKey
import com.wavesplatform.common.state.ByteStr
import supranational.blst

import java.util

sealed trait BlsKeyPair {
  def publicKey: BlsPublicKey

  def sign(message: Array[Byte]): BlsSignature
}

object BlsKeyPair {
  def apply(wavesPrivateKey: WavesPrivateKey): BlsKeyPair = new BlsSeedKeyPair(wavesPrivateKey.arr)
}

private final class BlsSeedKeyPair(private val wavesPrivateKey: Array[Byte]) extends BlsKeyPair {
  private lazy val sk: blst.SecretKey = BlsUtils.mkSecretKey(wavesPrivateKey)
  lazy val publicKey: BlsPublicKey    = BlsPublicKey.unchecked(ByteStr(BlsUtils.mkPublicKey(sk)))

  def sign(message: Array[Byte]): BlsSignature = BlsSignature.unsafe(ByteStr(BlsUtils.signBasic(sk, message)))

  override def equals(other: Any): Boolean = other match {
    case other: BlsSeedKeyPair => util.Arrays.equals(other.wavesPrivateKey, wavesPrivateKey)
    case _                     => false
  }

  private lazy val hc          = util.Arrays.hashCode(wavesPrivateKey)
  override def hashCode(): Int = hc
}
