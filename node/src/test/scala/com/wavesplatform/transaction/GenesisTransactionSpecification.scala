package com.wavesplatform.transaction

import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class GenesisTransactionSpecification extends PropSpec with PropertyChecks with Matchers {

  private val defaultRecipient = PublicKey(Array.fill(32)(0: Byte))

  property("GenesisTransaction Signature should be the same") {
    val balance   = 457L
    val timestamp = 2398762345L
    val signature = GenesisTransaction.generateSignature(defaultRecipient, balance, timestamp)

    val expected = "3L4zhpN1o6TysvM8FZFv1NmSEjpGSgV4V71e2iJwseFrrt65aZJiyXwqj5WpigLAn296sUrFb9yN8fdsY7GSdwwR"
    val actual   = Base58.encode(signature)

    assert(actual == expected)
  }

  property("GenesisTransaction parse from Bytes should work fine") {
    val bytes = Base58.tryDecodeWithLimit("5GoidXWjBfzuS9tZm4Fp6GAXUYFunVMsoWAew3eBnDbmaDi7WiP9yVpBD2").get

    val actualTransaction = GenesisTransaction.parseBytes(bytes).get

    val balance             = 12345L
    val timestamp           = 1234567890L
    val expectedTransaction = GenesisTransaction.create(defaultRecipient, balance, timestamp).explicitGet()

    actualTransaction should equal(expectedTransaction)
  }

  property("GenesisTransaction serialize/deserialize roundtrip") {
    forAll(Gen.listOfN(32, Arbitrary.arbitrary[Byte]).map(_.toArray), Gen.posNum[Long], Gen.posNum[Long]) {
      (recipientSeed: Array[Byte], time: Long, amount: Long) =>
        val recipient = KeyPair(recipientSeed)
        val source    = GenesisTransaction.create(recipient, amount, time).explicitGet()
        val bytes     = source.bytes()
        val dest      = GenesisTransaction.parseBytes(bytes).get

        source should equal(dest)
    }
  }

}
