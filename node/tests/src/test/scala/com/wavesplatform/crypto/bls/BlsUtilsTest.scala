package com.wavesplatform.crypto.bls

import com.wavesplatform.account.KeyPair
import com.wavesplatform.crypto.bls.BlsUtils.*
import com.wavesplatform.test.FreeSpec
import org.scalatest.EitherValues
import supranational.blst
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

  "zero secret/public keys and signatures" - {
    val message = "test".getBytes()

    val zeroSk = BlsUtils.mkBlsSecretKey(Array.fill[Byte](31)(1))
    val zeroPk = new blst.P1(zeroSk)
    val zeroSig = new blst.P2()
      .hash_to(message, BlsDomainSeparationTag)
      .sign_with(zeroSk)

    val okSk = BlsUtils.mkBlsSecretKey(Array.fill[Byte](32)(0))
    val okPk = new blst.P1(okSk)
    val okSig = new blst.P2()
      .hash_to(message, BlsDomainSeparationTag)
      .sign_with(okSk)

    "can't create pk from zero bytes" in {
      val bytes = Array.fill[Byte](zeroPk.serialize().length)(0)
      intercept[RuntimeException] { new blst.P1(bytes) }.getMessage should include("point is not on curve")
    }

    "zeroSk" in {
      zeroSk.to_bendian() shouldBe Array.fill[Byte](32)(0)
    }

    "zeroPk in group" in {
      zeroPk.is_inf() shouldBe true
      zeroPk.in_group() shouldBe true
    }

    "zeroSk in group" in {
      zeroSig.is_inf() shouldBe true
      zeroSig.in_group() shouldBe true
    }

    "zeroSig not verified" - {
      "by zeroPk" in {
        BlsUtils.verifyBasic(zeroSig.serialize(), message, zeroPk.serialize()) shouldBe false
      }

      "by okPk" in {
        BlsUtils.verifyBasic(zeroSig.serialize(), message, okPk.serialize()) shouldBe false
      }
    }

    "okSig not verified by zeroPk" in {
      BlsUtils.verifyBasic(okSig.serialize(), message, zeroPk.serialize()) shouldBe false
    }

    "aggregated pk" - {
      "okPk + zeroPk == okPk" in {
        okPk.dup().add(zeroPk).is_equal(okPk) shouldBe true
      }

      "zeroPk + okPk == okPk" in {
        zeroPk.dup().add(okPk).is_equal(okPk) shouldBe true
      }
    }

    "aggSig" - {
      "okSig + zeroSig == okSig" in {
        okSig.dup().add(zeroSig).is_equal(okSig) shouldBe true
      }

      "zeroSig + okSig == okSig" in {
        zeroSig.dup().add(okSig).is_equal(okSig) shouldBe true
      }
    }

    "aggSig verification with zeroSk" in {
      val aggSig = okSig.dup().add(zeroSig)
      BlsUtils.verifyAgg(aggSig.serialize(), message, Seq(okPk.serialize(), zeroPk.serialize())).value shouldBe true
    }
  }

  private def mkRandomSecretKey(): SecretKey  = mkBlsSecretKey(mkRandomWavesKeyPair().privateKey.arr)
  private def mkRandomWavesKeyPair(): KeyPair = KeyPair(Array.fill(32)(Random.nextInt().toByte))
}
