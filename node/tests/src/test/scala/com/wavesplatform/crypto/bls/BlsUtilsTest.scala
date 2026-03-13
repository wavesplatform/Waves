package com.wavesplatform.crypto.bls

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.crypto.bls.BlsUtils.*
import com.wavesplatform.test.{FreeSpec, produce}
import org.scalatest.EitherValues
import supranational.blst
import supranational.blst.{P1, SecretKey}

import java.nio.charset.StandardCharsets
import scala.util.Random

class BlsUtilsTest extends FreeSpec with EitherValues {
  private val sk1 = mkRandomSecretKey()
  private val pk1 = mkPublicKey(sk1)

  private val sk2 = mkRandomSecretKey()
  private val pk2 = mkPublicKey(sk2)

  private val sk3 = mkRandomSecretKey()
  private val pk3 = mkPublicKey(sk3)

  private val message = "assertion".getBytes()

  private val sig1 = signBasic(sk1, message)
  private val sig2 = signBasic(sk2, message)
  private val sig3 = signBasic(sk3, message)

  "aggregation in verifyAgg" - {
    "signed with one" - {
      "verify with same" in {
        BlsUtils.verifyAgg(sig1, message, Seq(pk1)) should beRight
      }

      "verify with other" in {
        BlsUtils.verifyAgg(sig1, message, Seq(pk2)) should produce("Wrong BLS signature")
      }
    }

    "signed with multiple" - {
      "verify with one known" in {
        val aggSig = Seq(sig1, sig2).reduceLeft(aggSig2)
        BlsUtils.verifyAgg(aggSig, message, Seq(pk2)) should produce("Wrong BLS signature")
      }

      "verify with one unknown" in {
        val aggSig = Seq(sig1, sig2).reduceLeft(aggSig2)
        BlsUtils.verifyAgg(aggSig, message, Seq(pk3)) should produce("Wrong BLS signature")
      }

      "verify with all" in {
        val aggSig = Seq(sig1, sig2).reduceLeft(aggSig2)
        BlsUtils.verifyAgg(aggSig, message, Seq(pk1, pk2)) should beRight
      }

      "verify with all and unknown" in {
        val aggSig = Seq(sig1, sig2).reduceLeft(aggSig2)
        BlsUtils.verifyAgg(aggSig, message, Seq(pk1, pk2, pk3)) should produce("Wrong BLS signature")
      }

      "verify with less" in {
        val aggSig = Seq(sig1, sig2, sig3).reduceLeft(aggSig2)
        BlsUtils.verifyAgg(aggSig, message, Seq(pk1, pk2)) should produce("Wrong BLS signature")
      }
    }

    "aggregation of two same signatures" in {
      val aggSig = aggSig2(aggSig2(sig1, sig2), sig1)

      BlsUtils.verifyAgg(aggSig, message, Seq(pk1, pk2, pk1)) should beRight
      BlsUtils.verifyAgg(aggSig, message, Seq(pk1, pk2)) should produce("Wrong BLS signature")
    }

    "different order of signatures and keys" in {
      val aggSig = aggSig2(sig1, sig2)
      BlsUtils.verifyAgg(aggSig, message, Seq(pk2, pk1)) should beRight
    }

    "associativity" in {
      val aggSig = Seq(sig1, sig2, sig3).reduceLeft(aggSig2)
      BlsUtils.verifyAgg(aggSig, message, Seq(pk2, pk1, pk3)) should beRight
    }
  }

  "signBasic" - {
    "zero message" in {
      val message = Array.emptyByteArray
      val sig     = BlsUtils.signBasic(sk1, message)
      BlsUtils.verifyBasic(sig, message, pk1) should beRight
    }
  }

  "verifyBasic" - {
    "same pk" in {
      BlsUtils.verifyBasic(sig1, message, pk1) should beRight
    }

    "other pk" in {
      BlsUtils.verifyBasic(sig1, message, pk2) should produce("Wrong BLS signature")
    }
  }

  "zero secret/public keys and signatures" - {
    val message = "test".getBytes()

    val zeroSk = BlsUtils.mkSecretKey(Array.fill[Byte](31)(1)) // Still zero if even less bytes
    val zeroPk = new blst.P1(zeroSk)
    val zeroSig = new blst.P2()
      .hash_to(message, BlsDomainSeparationTag)
      .sign_with(zeroSk)

    val okSk = BlsUtils.mkSecretKey(Array.fill[Byte](32)(0))
    val okPk = new blst.P1(okSk)
    val okSig = new blst.P2()
      .hash_to(message, BlsDomainSeparationTag)
      .sign_with(okSk)

    "can't create pk from zero bytes" in {
      val bytes = Array.fill[Byte](zeroPk.compress().length)(0)
      intercept[RuntimeException] { new blst.P1(bytes) }.getMessage should include("bad point encoding")
    }

    "zeroSk" in {
      zeroSk.to_bendian() shouldBe Array.fill[Byte](32)(0)
    }

    "zeroPk in group" in {
      zeroPk.is_inf() shouldBe true
      zeroPk.in_group() shouldBe true
    }

    "zeroSig in group" in {
      zeroSig.is_inf() shouldBe true
      zeroSig.in_group() shouldBe true
    }

    "zeroSig not verified" - {
      "by zeroPk" in {
        BlsUtils.verifyBasic(zeroSig.compress(), message, zeroPk.compress()) should produce("BLST_PK_IS_INFINITY")
      }

      "by okPk" in {
        BlsUtils.verifyBasic(zeroSig.compress(), message, okPk.compress()) should produce("Wrong BLS signature")
      }
    }

    "okSig not verified by zeroPk" in {
      BlsUtils.verifyBasic(okSig.compress(), message, zeroPk.compress()) should produce("BLST_PK_IS_INFINITY")
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
      BlsUtils.verifyAgg(aggSig.compress(), message, Seq(okPk.compress(), zeroPk.compress())) should beRight
    }
  }

  "expected public keys" in forAll(
    Table(
      ("seed", "expected sk in base64", "expected pk in base64"),
      (
        "-EXACTLY-32-BYTES-LENGTH-STRING-",
        "ELIahWN5dDHoS9hScLMgGSNwF1qpuikaqNrdxZHCIuE=",
        "qSUdS6J92V1nNOdx4TafRu4U17qhqwVXKNyy2IVV9GWnUzUYlk/uH4l8fOoupSJj"
      ),
      (
        "a string longer than 32 bytes is used as the seed here",
        "TmpPD8kiXQtRzvpQ+TJm6RUqjy5N3t9WZlv40iA66cw=",
        "o2DzLHA7PG7BvHXTqnz4c8arX/tjiU11YuHsQnfUH0Lo/+ksy1toSYXFFy5auEJT"
      )
    )
  ) { (seed, expectedSkInBase64, expectedPkInBase64) =>
    val sk = BlsUtils.mkSecretKey(seed.getBytes(StandardCharsets.UTF_8))
    Base64.encode(sk.to_bendian()) shouldBe expectedSkInBase64

    val pk = BlsUtils.mkPublicKey(sk)
    Base64.encode(pk) shouldBe expectedPkInBase64
  }

  "pk restore" in {
    val sk = BlsUtils.mkSecretKey("-EXACTLY-32-BYTES-LENGTH-STRING-".getBytes(StandardCharsets.UTF_8))
    val pk = BlsUtils.mkPublicKey(sk)

    val pkRestored = new P1(pk).compress()
    pkRestored shouldBe pk

    val skRestored = new SecretKey()
    skRestored.from_bendian(sk.to_bendian())

    val pkFromRestoredSk = BlsUtils.mkPublicKey(skRestored)
    pkFromRestoredSk shouldBe pk
  }

  private def aggSig2(sig1: Array[Byte], sig2: Array[Byte]): Array[Byte] = BlsUtils.aggSig(Seq(sig1, sig2)).value

  private def mkRandomSecretKey(): SecretKey  = mkSecretKey(mkRandomWavesKeyPair().privateKey.arr)
  private def mkRandomWavesKeyPair(): KeyPair = KeyPair(Array.fill(32)(Random.nextInt().toByte))
}
