package com.wavesplatform.bls

import com.wavesplatform.account.PrivateKey as WavesPrivateKey
import com.wavesplatform.bls
import com.wavesplatform.bls.BlsKeyPair.*
import supranational.blst

import java.util

sealed trait BlsKeyPair {
  def publicKey: BlsPublicKey

  def sign(message: Array[Byte]): Array[Byte]
  def verify(message: Array[Byte], signature: Array[Byte]): Boolean
}

object BlsKeyPair {
  val DomainSeparationTag = "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_" // TODO

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
      .hash_to(message, DomainSeparationTag, publicKey.asByteStr.arr)
      .sign_with(privateKey)
      .compress() // .serialize() // TODO compressed vs default
  }

  def verify(message: Array[Byte], signature: Array[Byte]): Boolean = {
    val _sig = new blst.P2_Affine(signature)
    val _pk  = new blst.P1_Affine(publicKey.asByteStr.arr)
    if (!_pk.in_group()) throw new java.lang.RuntimeException("disaster") // TODO:

    val ctx = new blst.Pairing(true, DomainSeparationTag)
    ctx.aggregate(_pk, _sig, message, publicKey.asByteStr.arr)
    ctx.commit()
    ctx.finalverify()
  }

  override def equals(other: Any): Boolean = other match {
    case other: BlsSeedKeyPair => util.Arrays.equals(other.wavesPrivateKey, wavesPrivateKey)
    case _                     => false
  }

  private lazy val hc          = util.Arrays.hashCode(wavesPrivateKey)
  override def hashCode(): Int = hc
}
