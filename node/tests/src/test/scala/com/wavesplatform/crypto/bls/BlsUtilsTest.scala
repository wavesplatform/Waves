package com.wavesplatform.crypto.bls

import com.wavesplatform.account.KeyPair
import com.wavesplatform.crypto.bls.BlsUtils.*
import com.wavesplatform.test.FreeSpec
import org.scalatest.EitherValues
import supranational.blst.SecretKey

import scala.util.Random

class BlsUtilsTest extends FreeSpec with EitherValues {
  "aggregation in verifyAgg" - {
    "different order of signatures and keys" in {
      val privateKey1 = mkRandomSecretKey()
      val publicKey1  = mkBlsPublicKey(privateKey1)

      val privateKey2 = mkRandomSecretKey()
      val publicKey2  = mkBlsPublicKey(privateKey2)

      val message = "assertion".getBytes()
      val sig1    = signBasic(privateKey1, message)
      val sig2    = signBasic(privateKey2, message)

      val aggSig = BlsUtils.aggSign(sig1, sig2)

      BlsUtils.verifyAgg(aggSig, message, Seq(publicKey2, publicKey1)).value shouldBe true
    }

    "associativity" in {
      val privateKey1 = mkRandomSecretKey()
      val publicKey1  = mkBlsPublicKey(privateKey1)

      val privateKey2 = mkRandomSecretKey()
      val publicKey2  = mkBlsPublicKey(privateKey2)

      val privateKey3 = mkRandomSecretKey()
      val publicKey3  = mkBlsPublicKey(privateKey3)

      val message = "assertion".getBytes()
      val sig1    = signBasic(privateKey1, message)
      val sig2    = signBasic(privateKey2, message)
      val sig3    = signBasic(privateKey3, message)

      val aggSig = Seq(sig1, sig2, sig3).reduceLeft(BlsUtils.aggSign)

      BlsUtils.verifyAgg(aggSig, message, Seq(publicKey2, publicKey1, publicKey3)).value shouldBe true
    }
  }

  private def mkRandomSecretKey(): SecretKey  = mkBlsSecretKey(mkRandomWavesKeyPair().privateKey.arr)
  private def mkRandomWavesKeyPair(): KeyPair = KeyPair(Array.fill(32)(Random.nextInt().toByte))
}
