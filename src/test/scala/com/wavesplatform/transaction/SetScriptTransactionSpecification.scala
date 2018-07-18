package com.wavesplatform.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.state._
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.transaction.smart.SetScriptTransaction

class SetScriptTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  private val versionGen: Gen[Byte] = Gen.oneOf(SetScriptTransaction.supportedVersions.toSeq)
  private val versionAndAccountGen: Gen[(Byte, PrivateKeyAccount)] = for {
    version <- versionGen
    account <- accountGen
  } yield (version, account)

  property("SetScriptTransaction serialization roundtrip") {
    forAll(setScriptTransactionGen) { tx: SetScriptTransaction =>
      val recovered = SetScriptTransaction.parseBytes(tx.bytes()).get
      assertTxs(recovered, tx)
    }
  }

  property("SetScriptTransaction serialization from TypedTransaction") {
    forAll(setScriptTransactionGen) { tx: SetScriptTransaction =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[SetScriptTransaction], tx)
    }
  }

  property("SetScriptTransaction id doesn't depend on proof") {
    forAll(versionAndAccountGen, byteArrayGen(10), byteArrayGen(15), proofsGen, proofsGen, scriptGen) {
      case ((version, acc: PrivateKeyAccount), p1, p2, proofs1, proofs2, script) =>
        val tx1 = SetScriptTransaction.create(version, acc, Some(script), 1, 1, proofs1).explicitGet()
        val tx2 = SetScriptTransaction.create(version, acc, Some(script), 1, 1, proofs2).explicitGet()
        tx1.id() shouldBe tx2.id()
    }
  }

  private def assertTxs(first: SetScriptTransaction, second: SetScriptTransaction): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.version shouldEqual second.version
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
    first.script shouldEqual second.script
  }

  property("JSON format validation") {
    val js = Json.parse("""{
                       "type": 13,
                       "id": "Cst37pKJ19WnUZSD6mjqywosMJDbqatuYm2sFAbXrysE",
                       "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000,
                       "timestamp": 1526983936610,
                       "proofs": [
                       "tcTr672rQ5gXvcA9xCGtQpkHC8sAY1TDYqDcQG7hQZAeHcvvHFo565VEv1iD1gVa3ZuGjYS7hDpuTnQBfY2dUhY"
                       ],
                       "version": 1,
                       "script": null
                       }
    """)

    val tx = SetScriptTransaction
      .create(
        1,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        None,
        100000,
        1526983936610L,
        Proofs(Seq(ByteStr.decodeBase58("tcTr672rQ5gXvcA9xCGtQpkHC8sAY1TDYqDcQG7hQZAeHcvvHFo565VEv1iD1gVa3ZuGjYS7hDpuTnQBfY2dUhY").get))
      )
      .right
      .get

    js shouldEqual tx.json()
  }
}
