package com.wavesplatform.transaction

import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.{IssueTransactionV1, ReissueTransactionV2}
import org.scalacheck.Gen
import play.api.libs.json._

class ReissueTransactionV2Specification extends GenericTransactionSpecification[ReissueTransactionV2] {

  def transactionParser: com.wavesplatform.transaction.TransactionParserFor[ReissueTransactionV2] = ReissueTransactionV2

  def updateProofs(tx: ReissueTransactionV2, p: Proofs): ReissueTransactionV2 = {
    tx.copy(proofs = p)
  }

  def assertTxs(first: ReissueTransactionV2, second: ReissueTransactionV2): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.version shouldEqual second.version
    first.quantity shouldEqual second.quantity
    first.reissuable shouldEqual second.reissuable
    first.asset shouldEqual second.asset
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }

  def generator: Gen[((Seq[com.wavesplatform.transaction.Transaction], ReissueTransactionV2))] =
    for {
      (sender, assetName, description, quantity, decimals, _, iFee, timestamp) <- issueParamGen
      fee                                                                      <- smallFeeGen
      reissuable                                                               <- Gen.oneOf(true, false)
    } yield {
      val issue = IssueTransactionV1.selfSigned(sender, assetName, description, quantity, decimals, reissuable = true, iFee, timestamp).explicitGet()
      val reissue1 = ReissueTransactionV2
        .selfSigned(AddressScheme.current.chainId, sender, IssuedAsset(issue.assetId), quantity, reissuable = reissuable, fee, timestamp)
        .explicitGet()
      (Seq(issue), reissue1)
    }

  def jsonRepr: Seq[(JsValue, ReissueTransactionV2)] =
    Seq(
      (Json.parse("""{
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
       ReissueTransactionV2
         .create(
           'T',
           PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
           IssuedAsset(ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get),
           100000000L,
           true,
           100000000L,
           1526287561757L,
           Proofs(Seq(ByteStr.decodeBase58("4DFEtUwJ9gjMQMuEXipv2qK7rnhhWEBqzpC3ZQesW1Kh8D822t62e3cRGWNU3N21r7huWnaty95wj2tZxYSvCfro").get))
         )
         .right
         .get))

  def transactionName: String = "ReissueTransactionV2"
}
