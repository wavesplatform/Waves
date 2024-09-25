package com.wavesplatform.transaction

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.{IssueTransaction, ReissueTransaction}
import org.scalacheck.Gen
import play.api.libs.json._

class ReissueTransactionV2Specification extends GenericTransactionSpecification[ReissueTransaction] {
  def transactionParser: TransactionParser = ReissueTransaction

  def updateProofs(tx: ReissueTransaction, p: Proofs): ReissueTransaction = {
    tx.copy(proofs = p)
  }

  def assertTxs(first: ReissueTransaction, second: ReissueTransaction): Unit = {
    first.sender shouldEqual second.sender
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.version shouldEqual second.version
    first.quantity shouldEqual second.quantity
    first.reissuable shouldEqual second.reissuable
    first.asset shouldEqual second.asset
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }

  def generator: Gen[(Seq[com.wavesplatform.transaction.Transaction], ReissueTransaction)] =
    for {
      (sender, assetName, description, quantity, decimals, _, iFee, timestamp) <- issueParamGen
      fee                                                                      <- smallFeeGen
      reissuable                                                               <- Gen.oneOf(true, false)
    } yield {
      val issue = IssueTransaction
        .selfSigned(
          TxVersion.V1,
          sender,
          new String(assetName),
          new String(description),
          quantity,
          decimals,
          reissuable = true,
          script = None,
          iFee,
          timestamp
        )
        .explicitGet()
      val reissue1 = ReissueTransaction
        .selfSigned(2.toByte, sender, issue.asset, quantity, reissuable = reissuable, fee, timestamp)
        .explicitGet()
      (Seq(issue), reissue1)
    }

  def jsonRepr: Seq[(JsValue, ReissueTransaction)] =
    Seq(
      (
        Json.parse("""{
                       "type": 5,
                       "id": "HbQ7gMoDyRxSU6LbLLBVNTbxASaR8rm4Zck6eYvWVUkB",
                       "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000000,
                       "feeAssetId": null,
                       "timestamp": 1526287561757,
                       "proofs": [
                       "4DFEtUwJ9gjMQMuEXipv2qK7rnhhWEBqzpC3ZQesW1Kh8D822t62e3cRGWNU3N21r7huWnaty95wj2tZxYSvCfro"
                       ],
                       "version": 2,
                       "chainId": 84,
                       "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
                       "quantity": 100000000,
                       "reissuable": true
                    }
    """),
        ReissueTransaction
          .create(
            2.toByte,
            PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
            IssuedAsset(ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get),
            100000000L,
            true,
            100000000L,
            1526287561757L,
            Proofs(Seq(ByteStr.decodeBase58("4DFEtUwJ9gjMQMuEXipv2qK7rnhhWEBqzpC3ZQesW1Kh8D822t62e3cRGWNU3N21r7huWnaty95wj2tZxYSvCfro").get))
          )
          .explicitGet()
      )
    )

  def transactionName: String = "ReissueTransactionV2"

  override def preserBytesJson: Option[(Array[Byte], JsValue)] = Some(
    Base64.decode(
      "AAUCVOCOBZFLfURuJuj21iJgASjQvAM23mv7tVou6+6LYG9l3/w8Zu3PQZcoDmkz3yU/mqiFs096Lu5f6yVf+kZXIlsAy3rx4HUSowAAAAAAAqOonQAAF8vvN3seAQABAEDckjd4IQexe7RX1/MU8IOp6W/n3DpUa1BgOKRENUYcKTCELzSA71sNELc3tWG8YYX8EWHUbdmnD8VDbJQMOi0M"
    ) ->
      Json.parse("""{
                   |  "senderPublicKey" : "G7ZvtLKf7FitWFevdEaqs9fPaVnrLF4JrxteRwBSt3uE",
                   |  "quantity" : 57274599543739043,
                   |  "fee" : 44279965,
                   |  "type" : 5,
                   |  "version" : 2,
                   |  "reissuable" : false,
                   |  "sender" : "3N33A9YpVvk2eNCdUfABmUCTFAKDjvzhL23",
                   |  "feeAssetId" : null,
                   |  "chainId" : 84,
                   |  "proofs" : [ "5Qmz5hWdaTD4jNSPvkF1dyZob3sesJp9ruHBNtGCp7fXgmUaXGtv6GWBt2Ed6Yf4gpMAF6PKXzfmKVhjUWq919Co" ],
                   |  "assetId" : "G5LzjMfuvAYNEX3kcS1tDyBiaTgE6ta1nNTq8HVnYryY",
                   |  "id" : "6G3Vne25wTUA9fC6H1tjXhXhYQkBvfPVgSZKn1a5mUPH",
                   |  "timestamp" : 26164659190558
                   |}""".stripMargin)
  )
}
