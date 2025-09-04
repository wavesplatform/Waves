package com.wavesplatform.crypto.bls

import com.wavesplatform.account.KeyPair
import com.wavesplatform.crypto.bls.BlsUtils.*
import com.wavesplatform.test.FreeSpec
import org.scalatest.EitherValues
import supranational.blst
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

      val aggSig = new blst.P2().add(new blst.P2(sig1)).add(new blst.P2(sig2)).compress()

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

      val aggSig       = new blst.P2().add(new blst.P2(sig1)).add(new blst.P2(sig2)).compress()
      val finalAggSig2 = new blst.P2_Affine(aggSig).to_jacobian().add(new blst.P2(sig3)).compress()

      BlsUtils.verifyAgg(finalAggSig2, message, Seq(publicKey2, publicKey1, publicKey3)).value shouldBe true
    }
  }

  private def mkRandomSecretKey(): SecretKey  = mkBlsSecretKey(mkRandomWavesKeyPair().privateKey.arr)
  private def mkRandomWavesKeyPair(): KeyPair = KeyPair(Array.fill(32)(Random.nextInt().toByte))
}
