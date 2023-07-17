package com.wavesplatform.transaction

import com.google.common.primitives.Longs
import com.wavesplatform.account.{Alias, KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.produceRejectOrFailedDiff
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.serialization.impl.{CreateAliasTxSerializer, PBTransactionSerializer}
import play.api.libs.json.Json

import scala.util.{Failure, Random, Success}

class CreateAliasTransactionSpecification extends PropSpec with WithDomain {

  property("CreateAliasTransaction serialization roundtrip") {
    forAll(createAliasGen) { (tx: CreateAliasTransaction) =>
      val recovered = CreateAliasTxSerializer.parseBytes(tx.bytes()).get
      recovered shouldEqual tx
    }
  }

  property("CreateAliasTransaction serialization from TypedTransaction") {
    forAll(createAliasGen) { (tx: CreateAliasTransaction) =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered shouldEqual tx
    }
  }

  property("CreateAliasTransaction PB serialization roundtrip") {
    val kp1 = KeyPair(Longs.toByteArray(Random.nextLong()))
    val kp2 = KeyPair(Longs.toByteArray(Random.nextLong()))

    val cat = CreateAliasTransaction(
      3.toByte,
      TxHelpers.signer(1).publicKey,
      "abc12345",
      TxPositiveAmount.unsafeFrom(0.001.waves),
      System.currentTimeMillis(),
      Proofs.empty,
      'T'.toByte
    )
    val signedCreateAlias = cat.copy(
      proofs = cat.signWith(kp1.privateKey).proofs.proofs ++ cat.signWith(kp2.privateKey).proofs.proofs
    )
    PBTransactionSerializer.parseBytes(PBTransactionSerializer.bytes(signedCreateAlias)) match {
      case Success(tx @ CreateAliasTransaction(_, _, _, _, _, proofs, _)) =>
        tx shouldBe signedCreateAlias
        proofs shouldBe signedCreateAlias.proofs
      case Success(tx)        => fail(s"Unexpected transaction type: ${tx.tpe.transactionName}")
      case Failure(exception) => fail(exception)
    }
  }

  property("The same aliases from different senders have the same id") {
    forAll(accountGen, accountGen, aliasGen, timestampGen) { case (a1: KeyPair, a2: KeyPair, a: Alias, t: Long) =>
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

  property("Multiple proofs before and after RideV6 activation") {
    val sender = TxHelpers.signer(1)
    withDomain(
      DomainPresets.RideV5.setFeaturesHeight((BlockchainFeatures.RideV6, 4)),
      AddrWithBalance.enoughBalances(sender)
    ) { d =>
      val kp1 = KeyPair(Longs.toByteArray(Random.nextLong()))
      val kp2 = KeyPair(Longs.toByteArray(Random.nextLong()))

      val cat = CreateAliasTransaction(
        3.toByte,
        sender.publicKey,
        "abc12345",
        TxPositiveAmount.unsafeFrom(0.001.waves),
        System.currentTimeMillis(),
        Proofs.empty,
        'T'.toByte
      )
      val signedCreateAlias = cat.copy(
        proofs = cat.signWith(kp1.privateKey).proofs.proofs ++ cat.signWith(kp2.privateKey).proofs.proofs
      )

      val verifier = TestCompiler(V5).compileExpression("""{-# STDLIB_VERSION 5 #-}
                                                          |{-# CONTENT_TYPE EXPRESSION #-}
                                                          |{-# SCRIPT_TYPE ACCOUNT #-}
                                                          |
                                                          |true
                                                          |""".stripMargin)
      d.appendBlock(TxHelpers.setScript(sender, verifier, version = TxVersion.V2))
      d.appendBlockE(signedCreateAlias) should produceRejectOrFailedDiff("Invalid proofs size")
      d.appendBlock()
      d.appendAndAssertSucceed(signedCreateAlias)
    }
  }

  property("Not allow transaction with multiple proofs from account without verifier before and after RideV6 activation") {
    val sender = TxHelpers.signer(1)
    withDomain(
      DomainPresets.RideV5.setFeaturesHeight((BlockchainFeatures.RideV6, 3)),
      AddrWithBalance.enoughBalances(sender)
    ) { d =>
      val kp1 = KeyPair(Longs.toByteArray(Random.nextLong()))
      val kp2 = KeyPair(Longs.toByteArray(Random.nextLong()))

      val cat = CreateAliasTransaction(
        3.toByte,
        sender.publicKey,
        "abc12345",
        TxPositiveAmount.unsafeFrom(0.001.waves),
        System.currentTimeMillis(),
        Proofs.empty,
        'T'.toByte
      )
      val signedCreateAlias = cat.copy(
        proofs = cat.signWith(kp1.privateKey).proofs.proofs ++ cat.signWith(kp2.privateKey).proofs.proofs
      )

      val errMsg = "Transactions from non-scripted accounts must have exactly 1 proof"

      d.appendBlockE(signedCreateAlias) should produceRejectOrFailedDiff(errMsg)
      d.appendBlock()
      d.appendBlockE(signedCreateAlias) should produceRejectOrFailedDiff(errMsg)
    }
  }
}
