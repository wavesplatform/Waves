package com.wavesplatform.bls

import com.wavesplatform.account.{KeyPair, PrivateKey}
import com.wavesplatform.test.FreeSpec
import supranational.blst

import scala.util.Random

type BAggPublicKey = blst.P1
type BAggSig       = blst.P2

def mkBlsPrivateKey(wavesPrivateKey: PrivateKey): blst.SecretKey = {
  val sk = new blst.SecretKey()
  sk.keygen(wavesPrivateKey.arr)
  sk
}

type BPublicKey = blst.P1
def mkBlsPublicKey(privateKey: blst.SecretKey): BPublicKey = {
  new blst.P1(privateKey)
  // pk.compress() // .serialize() // TODO compressed vs default
}

type BSig = blst.P2
def sign(privateKey: blst.SecretKey, publicKey: BPublicKey, message: Array[Byte]): BSig = { // TODO: Types
  val sig = new blst.P2()
  sig
    .hash_to(message, BlsDomainSeparationTag, publicKey.compress()) // TODO: aug?
    .sign_with(privateKey)
  // .compress() // .serialize() // TODO compressed vs default
}

def signBasic(sk: blst.SecretKey, message: Array[Byte]) =
  new blst.P2()
    .hash_to(message, BlsDomainSeparationTag /*, aug = null */ )
    .sign_with(sk)

def verify(signature: BSig, message: Array[Byte], publicKey: BPublicKey): Boolean = {
  val _pk = new blst.P1_Affine(publicKey)
  if (!_pk.in_group()) throw new java.lang.RuntimeException("disaster") // TODO:

  val _sig = new blst.P2_Affine(signature)

  val ctx = new blst.Pairing(true, BlsDomainSeparationTag)
  ctx.aggregate(_pk, _sig, message, publicKey.compress()) // TODO aug?
  ctx.commit()
  ctx.finalverify()
}

class BlsKeyPairTest extends FreeSpec {
  "sig/verify" in {
    val wavesKP    = mkRandomWavesKeyPair()
    val privateKey = mkBlsPrivateKey(wavesKP.privateKey)
    val publicKey  = mkBlsPublicKey(privateKey)

    val message   = "assertion".getBytes()
    val signature = sign(privateKey, publicKey, message)
    verify(signature, message, publicKey) shouldBe true
  }

  "agg" in {
    val wavesKP1    = mkRandomWavesKeyPair()
    val privateKey1 = mkBlsPrivateKey(wavesKP1.privateKey)
    val publicKey1  = mkBlsPublicKey(privateKey1).compress()

    val wavesKP2    = mkRandomWavesKeyPair()
    val privateKey2 = mkBlsPrivateKey(wavesKP2.privateKey)
    val publicKey2  = mkBlsPublicKey(privateKey2).compress()

    val message = "assertion".getBytes()
    val sig1    = signBasic(privateKey1, message).compress()
    val sig2    = signBasic(privateKey2, message).compress()

    println(s"publicKey1.size=${publicKey1.size}, publicKey2.size=${publicKey2.size}")
    println(s"sig1.size=${sig1.size}, sig2.size=${sig2.size}")

    val aggPk  = new blst.P1().add(new blst.P1(publicKey1)).add(new blst.P1(publicKey2))
    val aggSig = new blst.P2().add(new blst.P2(sig1)).add(new blst.P2(sig2))

    // Verify
    val ctx = new blst.Pairing(true, BlsDomainSeparationTag)
    ctx.aggregate(new blst.P1_Affine(aggPk), new blst.P2_Affine(aggSig), message)
    ctx.commit()
    val r = ctx.finalverify()
    // End Verify

    r shouldBe true
  }

  private def mkRandomWavesKeyPair(): KeyPair = KeyPair(Array.fill(32)(Random.nextInt().toByte))
}
