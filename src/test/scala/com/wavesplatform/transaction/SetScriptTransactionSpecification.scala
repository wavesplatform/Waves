package com.wavesplatform.transaction

import com.wavesplatform.state._
import org.scalacheck.Gen
import play.api.libs.json._
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.api.http.assets.SignedSetScriptRequest
import com.wavesplatform.transaction.smart.SetScriptTransaction

class SetScriptTransactionSpecification extends GenericTransactionSpecification[SetScriptTransaction] {

  def transactionParser: com.wavesplatform.transaction.TransactionParserFor[SetScriptTransaction] = SetScriptTransaction

  def updateProofs(tx: SetScriptTransaction, p: Proofs): SetScriptTransaction = {
    tx.copy(proofs = p)
  }

  def assertTxs(first: SetScriptTransaction, second: SetScriptTransaction): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.version shouldEqual second.version
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
    first.script shouldEqual second.script
  }

  def generator: Gen[((Seq[com.wavesplatform.transaction.Transaction], SetScriptTransaction))] = setScriptTransactionGen.map(t => (Seq(), t))

  def jsonRepr: Seq[(JsValue, SetScriptTransaction)] =
    Seq(
      (Json.parse("""{
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
    """),
       SetScriptTransaction
         .create(
           1,
           PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
           None,
           100000,
           1526983936610L,
           Proofs(Seq(ByteStr.decodeBase58("tcTr672rQ5gXvcA9xCGtQpkHC8sAY1TDYqDcQG7hQZAeHcvvHFo565VEv1iD1gVa3ZuGjYS7hDpuTnQBfY2dUhY").get))
         )
         .right
         .get))

  def transactionName: String = "SetScriptTransaction"

  private val versionGen: Gen[Byte] = Gen.oneOf(SetScriptTransaction.supportedVersions.toSeq)
  private val versionAndAccountGen: Gen[(Byte, PrivateKeyAccount)] = for {
    version <- versionGen
    account <- accountGen
  } yield (version, account)

  property("SetScriptTransaction id doesn't depend on proof (spec)") {
    forAll(versionAndAccountGen, proofsGen, proofsGen, scriptGen) {
      case ((version, acc: PrivateKeyAccount), proofs1, proofs2, script) =>
        val tx1 = SetScriptTransaction.create(version, acc, Some(script), 1, 1, proofs1).explicitGet()
        val tx2 = SetScriptTransaction.create(version, acc, Some(script), 1, 1, proofs2).explicitGet()
        tx1.id() shouldBe tx2.id()
    }
  }

  property("parse valid contract json") {
    val json =
      """
          |      {
          |  "type": 13,
          |  "version": 1,
          |  "senderPublicKey": "Cq5itmx4wbYuogySAoUp58MimLLkQrFFLr1tpJy2BYp1",
          |  "chainId": 87,
          |  "fee": 1000000,
          |  "timestamp": 1545230299891,
          |  "proofs": [
          |    "4vbZ5drLQ3r35QMb5ECUeBoVP5XyJRqoyPsKne6DgRo9PWS9U6SgcEbQt7WN6R6638GMgEauvsNaz3STrMGyxZvH"
          |  ],
          |  "id": "GGBbSvzeBkZeMWB9BQ9BTvpveKzK8c9rBZ9rgmMVisnP",
          |  "script": "base64:AwAAAAAAAAAAAAAAAQAAAAZzZW5kZXIAAAAAAAAAA2ZvbwAAAAEAAAABYQAAAGIJAQAAAAhXcml0ZVNldAAAAAEJAAROAAAAAgkBAAAACURhdGFFbnRyeQAAAAICAAAAAWEFAAAAAWEJAQAAAAlEYXRhRW50cnkAAAACAgAAAAZzZW5kZXIFAAAABnNlbmRlcgAAAAAr+/Gd"
          |}
      """.stripMargin

    val res = Json.parse(json).as[SignedSetScriptRequest]
    println(res)
    res.toTx shouldBe 'right

  }

}
