package com.wavesplatform.transaction

import cats.kernel.Monoid
import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.{Global, utils}
import com.wavesplatform.state.HistoryTest
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.{TransactionGen, WithDB, crypto}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.Json

class IssueTransactionV2Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen with WithDB with HistoryTest {

  property("IssueV2 serialization roundtrip") {
    forAll(issueV2TransactionGen()) { tx: IssueTransaction =>
      val recovered = IssueTransaction.parseBytes(tx.bytes()).get

      tx.sender.stringRepr shouldEqual recovered.sender.stringRepr
      tx.timestamp shouldEqual recovered.timestamp
      tx.decimals shouldEqual recovered.decimals
      tx.description shouldEqual recovered.description
      tx.script shouldEqual recovered.script
      tx.reissuable shouldEqual recovered.reissuable
      tx.fee shouldEqual recovered.fee
      tx.name shouldEqual recovered.name
      tx.chainByte shouldEqual recovered.chainByte
      tx.bytes() shouldEqual recovered.bytes()
    }
  }

  property("IssueV2 decode pre-encoded bytes") {
    val bytes = Base64.decode(
      "AAMCVNUoqr7DXKEA2Hx7ehKGMvrxnNRFMYGUV0RRE6MqIe8iAAhHaWdhY29pbgAIR2lnYWNvaW4AAAACVAvkAAgBAAAAAAX14QAAAAFjXdP0HQABAAEAQJgqUCQFUctLLrdJY8pUMZ3zO8sGtTL6xZhiVLDGaM8xG9r7ll2rPepblKWwbgP/QqZ0C8aAg2IMxY5E7hbUsos="
    )
    val json = Json.parse("""
        |{
        |                       "type": 3,
        |                       "id": "2ykNAo5JrvNCcL8PtCmc9pTcNtKUy2PjJkrFdRvTfUf4",
        |                       "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
        |                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
        |                       "fee": 100000000,
        |                       "feeAssetId": null,
        |                       "timestamp": 1526287561757,
        |                       "proofs": [
        |                       "43TCfWBa6t2o2ggsD4bU9FpvH3kmDbSBWKE1Z6B5i5Ax5wJaGT2zAvBihSbnSS3AikZLcicVWhUk1bQAMWVzTG5g"
        |                       ],
        |                       "version": 2,
        |                       "assetId": "2ykNAo5JrvNCcL8PtCmc9pTcNtKUy2PjJkrFdRvTfUf4",
        |                       "chainId": 84,
        |                       "name": "Gigacoin",
        |                       "quantity": 10000000000,
        |                       "reissuable": true,
        |                       "decimals": 8,
        |                       "description": "Gigacoin",
        |                       "script":null
        |                       }
        |""".stripMargin)

    val tx = IssueTransaction.serializer.parseBytes(bytes).get
    tx.json() shouldBe json
    assert(crypto.verify(tx.signature, tx.bodyBytes(), tx.sender), "signature should be valid")
  }

  property("JSON format validation") {
    val js = Json.parse("""{
                       "type": 3,
                       "id": "2ykNAo5JrvNCcL8PtCmc9pTcNtKUy2PjJkrFdRvTfUf4",
                       "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000000,
                       "feeAssetId": null,
                       "timestamp": 1526287561757,
                       "proofs": [
                       "43TCfWBa6t2o2ggsD4bU9FpvH3kmDbSBWKE1Z6B5i5Ax5wJaGT2zAvBihSbnSS3AikZLcicVWhUk1bQAMWVzTG5g"
                       ],
                       "version": 2,
                       "assetId": "2ykNAo5JrvNCcL8PtCmc9pTcNtKUy2PjJkrFdRvTfUf4",
                       "chainId": 84,
                       "name": "Gigacoin",
                       "quantity": 10000000000,
                       "reissuable": true,
                       "decimals": 8,
                       "description": "Gigacoin",
                       "script":null
                       }
    """)

    val tx = IssueTransaction
      .create(
        TxVersion.V2,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        "Gigacoin",
        "Gigacoin",
        10000000000L,
        8,
        reissuable = true,
        None,
        100000000,
        1526287561757L,
        Proofs(Seq(ByteStr.decodeBase58("43TCfWBa6t2o2ggsD4bU9FpvH3kmDbSBWKE1Z6B5i5Ax5wJaGT2zAvBihSbnSS3AikZLcicVWhUk1bQAMWVzTG5g").get))
      )
      .right
      .get

    tx.json() shouldEqual js
  }

  property("Contract script on asset isn't allowed") {
    val contract = {
      val script =
        s"""
          |{-# STDLIB_VERSION 3 #-}
          |{-# CONTENT_TYPE CONTRACT #-}
          |
          |@Verifier(txx)
          |func verify() = {
          |    true
          |}
        """.stripMargin
      Parser.parseContract(script).get.value
    }

    val ctx = {
      utils.functionCosts(V3)
      Monoid
        .combineAll(
          Seq(
            PureContext.build(Global, V3).withEnvironment[Environment],
            CryptoContext.build(Global, V3).withEnvironment[Environment],
            WavesContext.build(
              DirectiveSet(V3, Account, Expression).explicitGet()
            )
          )
        )
    }

    val script = ContractScript(V3, compiler.ContractCompiler(ctx.compilerContext, contract, V3).explicitGet())

    val tx = IssueTransaction.create(
      TxVersion.V2,
      PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
      "Gigacoin",
      "Gigacoin",
      10000000000L,
      8,
      reissuable = true,
      script.toOption,
      100000000,
      1526287561757L,
      Proofs(Seq(ByteStr.decodeBase58("43TCfWBa6t2o2ggsD4bU9FpvH3kmDbSBWKE1Z6B5i5Ax5wJaGT2zAvBihSbnSS3AikZLcicVWhUk1bQAMWVzTG5g").get))
    )

    tx shouldBe 'left
  }
}
