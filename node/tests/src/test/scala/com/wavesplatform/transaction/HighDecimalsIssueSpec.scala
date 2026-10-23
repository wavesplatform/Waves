package com.wavesplatform.transaction

import com.google.protobuf.ByteString
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.serialization.impl.IssueTxSerializer
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class HighDecimalsIssueSpec extends AnyFreeSpec with Matchers {

  private val sender = KeyPair(Array.fill(32)(1: Byte))

  "High Decimals & ERC-20 Asset Compatibility" - {
    "allows issuing assets with up to 18 decimals (Ethereum standard)" in {
      val txEither = IssueTransaction.create(
        version = TxVersion.V2,
        sender = sender.publicKey,
        name = "ETHCOIN",
        description = "Ethereum style token with 18 decimals",
        quantity = 1000000000000000L,
        decimals = 18,
        reissuable = true,
        script = None,
        fee = 100000000L,
        timestamp = System.currentTimeMillis()
      )

      txEither.isRight shouldBe true
      val tx = txEither.explicitGet()
      tx.decimals.value shouldBe 18
      tx.name.toStringUtf8 shouldBe "ETHCOIN"
    }

    "supports all intermediate decimals from 0 to 18" in {
      for (d <- 0 to 18) {
        val tx = IssueTransaction.create(
          version = TxVersion.V2,
          sender = sender.publicKey,
          name = s"TOKEN$d",
          description = s"Token with $d decimals",
          quantity = 1000000L,
          decimals = d.toByte,
          reissuable = false,
          script = None,
          fee = 100000000L,
          timestamp = System.currentTimeMillis()
        ).explicitGet()

        tx.decimals.value shouldBe d
      }
    }

    "rejects decimals greater than 18" in {
      val txEither = IssueTransaction.create(
        version = TxVersion.V2,
        sender = sender.publicKey,
        name = "INVALID",
        description = "Should fail",
        quantity = 1000000L,
        decimals = 19,
        reissuable = false,
        script = None,
        fee = 100000000L,
        timestamp = System.currentTimeMillis()
      )

      txEither.isLeft shouldBe true
    }

    "preserves byte serialization fidelity for 18-decimal assets" in {
      val original = IssueTransaction.create(
        version = TxVersion.V2,
        sender = sender.publicKey,
        name = "ETHTOKEN",
        description = "Roundtrip byte test",
        quantity = 5000000000000L,
        decimals = 18,
        reissuable = true,
        script = None,
        fee = 100000000L,
        timestamp = 1700000000000L
      ).map(_.signWith(sender.privateKey)).explicitGet()

      val bytes = IssueTxSerializer.toBytes(original)
      val parsed = IssueTransaction.parseBytes(bytes).get

      parsed.id() shouldBe original.id()
      parsed.decimals.value shouldBe 18
      parsed.name shouldBe original.name
      parsed.quantity.value shouldBe original.quantity.value
    }

    "preserves Protobuf serialization fidelity for 18-decimal assets" in {
      val original = IssueTransaction.create(
        version = TxVersion.V3,
        sender = sender.publicKey,
        name = "PBTOKEN",
        description = "Protobuf roundtrip test",
        quantity = 9999999999L,
        decimals = 18,
        reissuable = false,
        script = None,
        fee = 100000000L,
        timestamp = 1700000000000L
      ).map(_.signWith(sender.privateKey)).explicitGet()

      val pb = PBTransactions.protobuf(original)
      val vanilla = PBTransactions.vanillaUnsafe(pb).asInstanceOf[IssueTransaction]
      vanilla.decimals.value shouldBe 18
      vanilla.id() shouldBe original.id()
    }
  }
}
