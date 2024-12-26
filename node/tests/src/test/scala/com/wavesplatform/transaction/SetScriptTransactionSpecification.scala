package com.wavesplatform.transaction

import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import org.scalacheck.Gen
import play.api.libs.json.*

class SetScriptTransactionSpecification extends GenericTransactionSpecification[SetScriptTransaction] {

  def transactionParser: TransactionParser = SetScriptTransaction

  def updateProofs(tx: SetScriptTransaction, p: Proofs): SetScriptTransaction = {
    tx.copy(1.toByte, proofs = p)
  }

  def assertTxs(first: SetScriptTransaction, second: SetScriptTransaction): Unit = {
    first.sender shouldEqual second.sender
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.version shouldEqual second.version
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
    first.script shouldEqual second.script
  }

  def generator: Gen[(Seq[com.wavesplatform.transaction.Transaction], SetScriptTransaction)] = setScriptTransactionGen.map(t => (Seq(), t))

  def jsonRepr: Seq[(JsValue, SetScriptTransaction)] =
    Seq(
      (
        Json.parse("""{
                       "type": 13,
                       "id": "Cst37pKJ19WnUZSD6mjqywosMJDbqatuYm2sFAbXrysE",
                       "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000,
                       "feeAssetId": null,
                       "timestamp": 1526983936610,
                       "proofs": [
                       "tcTr672rQ5gXvcA9xCGtQpkHC8sAY1TDYqDcQG7hQZAeHcvvHFo565VEv1iD1gVa3ZuGjYS7hDpuTnQBfY2dUhY"
                       ],
                       "version": 1,
                       "chainId": 84,
                       "script": null
                       }
    """),
        SetScriptTransaction
          .create(
            1.toByte,
            PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
            None,
            100000,
            1526983936610L,
            Proofs(Seq(ByteStr.decodeBase58("tcTr672rQ5gXvcA9xCGtQpkHC8sAY1TDYqDcQG7hQZAeHcvvHFo565VEv1iD1gVa3ZuGjYS7hDpuTnQBfY2dUhY").get))
          )
          .explicitGet()
      )
    )

  def transactionName: String = "SetScriptTransaction"

  property("SetScriptTransaction id doesn't depend on proof (spec)") {
    forAll(accountGen, proofsGen, proofsGen, contractOrExpr) {
      case (acc: KeyPair, proofs1, proofs2, script) =>
        val tx1 = SetScriptTransaction.create(1.toByte, acc.publicKey, Some(script), 1, 1, proofs1).explicitGet()
        val tx2 = SetScriptTransaction.create(1.toByte, acc.publicKey, Some(script), 1, 1, proofs2).explicitGet()
        tx1.id() shouldBe tx2.id()
    }
  }

  override def preserBytesJson: Option[(Array[TxVersion], JsValue)] =
    Some(
      Base64.decode(
        "AA0BVM0TkdpiFV5gEBKCPA/ywRDiYs057r7FRwiXfwlf5tB1AQAfAQkAAGQAAAACAAAAAAAAAAABAAAAAAAAAAAB/cLTbwAAAAACODUuPMqjnZaOKXYBAAEAQIluaI2QJaNachtUD0FI1RzgcY0NmElIyp/0V06TAljDP4NlAt2XUHme3asul95ah/3/5E7JE9a/NXjvxDx4iA8="
      ) -> Json.parse(
        """
          |{
          |  "senderPublicKey" : "EoXtNDWGV5XsjiEAZXufddF57a1FdWhypJnps92CAdp8",
          |  "sender" : "3NBy87bQasxRkFTfMM8sq6MDbVUiPGS95g8",
          |  "feeAssetId" : null,
          |  "chainId" : 84,
          |  "proofs" : [ "3kNEbDaUaCZudgk5V5iJtTEY5Tm6NPLjbE2Jh8cF3ruSRtyRcSdnKqCtUWC8qQnwfpVttio3CftsTC7mbNsBsLo8" ],
          |  "fee" : 37238062,
          |  "id" : "HkZwtM5u9H5FAV8ihaKJo5nBj3rj9yYL9mJoLcuecK29",
          |  "type" : 13,
          |  "version" : 1,
          |  "script" : "base64:AQkAAGQAAAACAAAAAAAAAAABAAAAAAAAAAAB/cLTbw==",
          |  "timestamp" : 4380493484802320758
          |}
          |""".stripMargin
      )
    )
}
