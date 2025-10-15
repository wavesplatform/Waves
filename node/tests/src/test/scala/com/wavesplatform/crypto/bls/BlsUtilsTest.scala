package com.wavesplatform.crypto.bls

import com.wavesplatform.account.KeyPair
import com.wavesplatform.crypto.bls.BlsUtils.*
import com.wavesplatform.test.FreeSpec
import org.scalatest.EitherValues
import supranational.blst.SecretKey

import scala.util.Random

class BlsUtilsTest extends FreeSpec with EitherValues {
  private val privateKey1 = mkRandomSecretKey()
  private val publicKey1  = mkBlsPublicKey(privateKey1)

  private val privateKey2 = mkRandomSecretKey()
  private val publicKey2  = mkBlsPublicKey(privateKey2)

  private val privateKey3 = mkRandomSecretKey()
  private val publicKey3  = mkBlsPublicKey(privateKey3)

  private val message = "assertion".getBytes()

  private val sig1 = signBasic(privateKey1, message)
  private val sig2 = signBasic(privateKey2, message)
  private val sig3 = signBasic(privateKey3, message)

  "aggregation in verifyAgg" - {
    "aggregation of two same signatures" in {
      val aggSig = BlsUtils.aggSign(BlsUtils.aggSign(sig1, sig2), sig1)

      BlsUtils.verifyAgg(aggSig, message, Seq(publicKey1, publicKey2, publicKey1)).value shouldBe true
      BlsUtils.verifyAgg(aggSig, message, Seq(publicKey1, publicKey2)).value shouldBe false
    }

    "different order of signatures and keys" in {
      val aggSig = BlsUtils.aggSign(sig1, sig2)

      BlsUtils.verifyAgg(aggSig, message, Seq(publicKey2, publicKey1)).value shouldBe true
    }

    "associativity" in {
      val aggSig = Seq(sig1, sig2, sig3).reduceLeft(BlsUtils.aggSign)

      BlsUtils.verifyAgg(aggSig, message, Seq(publicKey2, publicKey1, publicKey3)).value shouldBe true
    }
  }

  private def mkRandomSecretKey(): SecretKey  = mkBlsSecretKey(mkRandomWavesKeyPair().privateKey.arr)
  private def mkRandomWavesKeyPair(): KeyPair = KeyPair(Array.fill(32)(Random.nextInt().toByte))
}
