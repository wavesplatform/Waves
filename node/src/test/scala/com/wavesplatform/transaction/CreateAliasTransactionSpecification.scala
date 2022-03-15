package com.wavesplatform.transaction

import com.google.common.primitives.Longs
import com.wavesplatform.account.{Alias, KeyPair, PublicKey}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.transaction.serialization.impl.CreateAliasTxSerializer
import com.wavesplatform.db.WithDomain
import com.wavesplatform.history.Domain.*
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.*
import com.wavesplatform.transaction.smart.SetScriptTransaction
import play.api.libs.json.Json

import scala.util.Random

class CreateAliasTransactionSpecification extends PropSpec with WithDomain {

  property("CreateAliasTransaction serialization roundtrip") {
    forAll(createAliasGen) { tx: CreateAliasTransaction =>
      val recovered = CreateAliasTxSerializer.parseBytes(tx.bytes()).get
      recovered shouldEqual tx
    }
  }

  property("CreateAliasTransaction serialization from TypedTransaction") {
    forAll(createAliasGen) { tx: CreateAliasTransaction =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered shouldEqual tx
    }
  }

  property("The same aliases from different senders have the same id") {
    forAll(accountGen, accountGen, aliasGen, timestampGen) {
      case (a1: KeyPair, a2: KeyPair, a: Alias, t: Long) =>
        val tx1 = CreateAliasTransaction.selfSigned(1.toByte, a1, a.name, MinIssueFee, t).explicitGet()
        val tx2 = CreateAliasTransaction.selfSigned(1.toByte, a2, a.name, MinIssueFee, t).explicitGet()
        tx1.id() shouldBe tx2.id()
    }
  }

  property("JSON format validation for CreateAliasTransactionV1") {
    val js = Json.parse("""{
                         "type": 10,
                         "id": "7acjQQWJAharrgzb4Z6jo3eeAKAGPmLkHTPtvBTKaiug",
                         "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                         "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                         "fee": 100000,
                         "feeAssetId": null,
                         "timestamp": 1526910778245,
                         "signature": "CC1jQ4qkuVfMvB2Kpg2Go6QKXJxUFC8UUswUxBsxwisrR8N5s3Yc8zA6dhjTwfWKfdouSTAnRXCxTXb3T6pJq3T",
                         "proofs": ["CC1jQ4qkuVfMvB2Kpg2Go6QKXJxUFC8UUswUxBsxwisrR8N5s3Yc8zA6dhjTwfWKfdouSTAnRXCxTXb3T6pJq3T"],
                         "version": 1,
                         "alias": "myalias"
                        }
    """)

    val tx = CreateAliasTransaction
      .create(
        Transaction.V1,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        "myalias",
        100000,
        1526910778245L,
        Proofs(ByteStr.decodeBase58("CC1jQ4qkuVfMvB2Kpg2Go6QKXJxUFC8UUswUxBsxwisrR8N5s3Yc8zA6dhjTwfWKfdouSTAnRXCxTXb3T6pJq3T").get)
      )
      .explicitGet()

    js shouldEqual tx.json()
  }

  property("JSON format validation for CreateAliasTransactionV2") {
    val js = Json.parse("""{
                       "type": 10,
                       "id": "7acjQQWJAharrgzb4Z6jo3eeAKAGPmLkHTPtvBTKaiug",
                       "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000,
                       "feeAssetId": null,
                       "timestamp": 1526910778245,
                       "proofs": [
                       "26U7rQTwpdma5GYSZb5bNygVCtSuWL6DKet1Nauf5J57v19mmfnq434YrkKYJqvYt2ydQBUT3P7Xgj5ZVDVAcc5k"
                       ],
                       "version": 2,
                       "alias": "myalias"
                        }
    """)

    val tx = CreateAliasTransaction
      .create(
        Transaction.V2,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        "myalias",
        100000,
        1526910778245L,
        Proofs(Seq(ByteStr.decodeBase58("26U7rQTwpdma5GYSZb5bNygVCtSuWL6DKet1Nauf5J57v19mmfnq434YrkKYJqvYt2ydQBUT3P7Xgj5ZVDVAcc5k").get))
      )
      .explicitGet()

    js shouldEqual tx.json()
  }

  property("Multiple proofs") {
    withDomain(DomainPresets.RideV5.copy(
      blockchainSettings = DomainPresets.RideV5.blockchainSettings.copy(
        functionalitySettings = DomainPresets.RideV5.blockchainSettings.functionalitySettings.copy(
          allowMultipleProofsInCreateAliasUntil = 2
        )
      )
    )) { d =>
      val sender = KeyPair(Longs.toByteArray(Random.nextLong()))
      d.appendBlock(
        GenesisTransaction.create(sender.toAddress, 100.waves, System.currentTimeMillis()).explicitGet(),
        SetScriptTransaction.selfSigned(2.toByte, sender, Some(TestCompiler(V5).compileExpression(
          """{-# STDLIB_VERSION 5 #-}
            |{-# CONTENT_TYPE EXPRESSION #-}
            |{-# SCRIPT_TYPE ACCOUNT #-}
            |
            |true
            |""".stripMargin)), 0.01.waves, System.currentTimeMillis()).explicitGet()
      )

      val kp1 = KeyPair(Longs.toByteArray(Random.nextLong()))
      val kp2 = KeyPair(Longs.toByteArray(Random.nextLong()))

      val cat = CreateAliasTransaction(3.toByte, sender.publicKey, "abc12345", 0.001.waves, System.currentTimeMillis(), Proofs.empty, 'T'.toByte)
      val signedCreateAlias = cat.copy(
        proofs = cat.signWith(kp1.privateKey).proofs.proofs ++ cat.signWith(kp2.privateKey).proofs.proofs
      )
      d.appendBlock(
        signedCreateAlias
      )

      d.appendBlock()

      val cat2 = CreateAliasTransaction(3.toByte, sender.publicKey, "xyz12345", 0.001.waves, System.currentTimeMillis(), Proofs.empty, 'T'.toByte)
      val signedCreateAlias2 = cat2.copy(
        proofs = cat.signWith(kp1.privateKey).proofs.proofs ++ cat.signWith(kp2.privateKey).proofs.proofs
      )

      d.blockchainUpdater
        .processBlock(d.createBlock(Block.PlainBlockVersion, Seq(signedCreateAlias2))) should produce("Invalid proofs size")
    }
  }
}
