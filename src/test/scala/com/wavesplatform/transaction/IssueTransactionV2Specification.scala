package com.wavesplatform.transaction

import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.state.HistoryTest
import com.wavesplatform.transaction.assets.IssueTransactionV2
import com.wavesplatform.{TransactionGen, WithDB}
import com.wavesplatform.transaction.smart.WavesEnvironment
import com.wavesplatform.transaction.smart.script.ContractScript
import com.wavesplatform.lang.{ContentType, Global, ScriptType, StdLibVersion}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.utils.DirectiveSet
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.utils
import monix.eval.Coeval
import cats.kernel.Monoid
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.Json

class IssueTransactionV2Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen with WithDB with HistoryTest {

  property("SmartIssueTransaction serialization roundtrip") {
    forAll(smartIssueTransactionGen()) { tx: IssueTransactionV2 =>
      val recovered = IssueTransactionV2.parseBytes(tx.bytes()).get

      tx.sender.address shouldEqual recovered.sender.address
      tx.timestamp shouldEqual recovered.timestamp
      tx.decimals shouldEqual recovered.decimals
      tx.description shouldEqual recovered.description
      tx.script shouldEqual recovered.script
      tx.reissuable shouldEqual recovered.reissuable
      tx.fee shouldEqual recovered.fee
      tx.name shouldEqual recovered.name
      tx.chainId shouldEqual recovered.chainId
      tx.bytes() shouldEqual recovered.bytes()
    }
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

    val tx = IssueTransactionV2
      .create(
        'T',
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        "Gigacoin".getBytes,
        "Gigacoin".getBytes,
        10000000000L,
        8,
        true,
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
      utils.functionCosts(StdLibVersion.V3)
      Monoid
        .combineAll(
          Seq(
            PureContext.build(StdLibVersion.V3),
            CryptoContext.build(Global),
            WavesContext.build(
              DirectiveSet(StdLibVersion.V3, ScriptType.Account, ContentType.Expression).explicitGet(),
              new WavesEnvironment('T'.toByte, Coeval(???), Coeval(???), utils.EmptyBlockchain, Coeval(???))
            )
          ))
    }

    val script = ContractScript(StdLibVersion.V3, compiler.ContractCompiler(ctx.compilerContext, contract).explicitGet())

    val tx = IssueTransactionV2
      .create(
        'T',
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        "Gigacoin".getBytes,
        "Gigacoin".getBytes,
        10000000000L,
        8,
        true,
        script.toOption,
        100000000,
        1526287561757L,
        Proofs(Seq(ByteStr.decodeBase58("43TCfWBa6t2o2ggsD4bU9FpvH3kmDbSBWKE1Z6B5i5Ax5wJaGT2zAvBihSbnSS3AikZLcicVWhUk1bQAMWVzTG5g").get))
      )

    tx shouldBe 'left
  }
}
