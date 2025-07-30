package com.wavesplatform.bls

import com.wavesplatform.account.PrivateKey as WavesPrivateKey
import com.wavesplatform.bls
import supranational.blst

import java.util

sealed trait BlsKeyPair {
  def publicKey: BlsPublicKey

  // TODO: move to package?
  def sign(message: Array[Byte]): Array[Byte]
  def verify(message: Array[Byte], signature: Array[Byte]): Boolean = publicKey.verify(message, signature)
}

object BlsKeyPair {
  def apply(wavesPrivateKey: WavesPrivateKey): BlsKeyPair = new BlsSeedKeyPair(wavesPrivateKey.arr)
}

private final class BlsSeedKeyPair(private val wavesPrivateKey: Array[Byte]) extends BlsKeyPair {
  private lazy val privateKey: blst.SecretKey = {
    val sk = new blst.SecretKey()
    sk.keygen(wavesPrivateKey)
    sk
  }

  lazy val publicKey: BlsPublicKey = {
    val pk = new blst.P1(privateKey)
    BlsPublicKey(pk.compress()) // .serialize() // TODO compressed vs default
  }

  def sign(message: Array[Byte]): Array[Byte] = { // TODO: Types
    val sig = new blst.P2()
    sig
      .hash_to(message, BlsDomainSeparationTag, publicKey.asByteStr.arr)
      .sign_with(privateKey)
      .compress() // .serialize() // TODO compressed vs default
  }

  override def equals(other: Any): Boolean = other match {
    case other: BlsSeedKeyPair => util.Arrays.equals(other.wavesPrivateKey, wavesPrivateKey)
    case _                     => false
  }

  private lazy val hc          = util.Arrays.hashCode(wavesPrivateKey)
  override def hashCode(): Int = hc
}
