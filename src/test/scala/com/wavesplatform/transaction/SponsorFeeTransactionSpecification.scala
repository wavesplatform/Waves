package com.wavesplatform.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.settings.Constants
import com.wavesplatform.state.diffs._
import com.wavesplatform.state.{ByteStr, EitherExt2}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.transaction.assets.{IssueTransactionV1, SponsorFeeTransaction}

class SponsorFeeTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("SponsorFee serialization roundtrip") {
    forAll(sponsorFeeGen) { transaction: SponsorFeeTransaction =>
      val recovered = SponsorFeeTransaction.parseBytes(transaction.bytes()).get
      recovered.bytes() shouldEqual transaction.bytes()
    }
  }

  property("SponsorFee serialization from TypedTransaction") {
    forAll(sponsorFeeGen) { transaction: SponsorFeeTransaction =>
      val recovered = TransactionParsers.parseBytes(transaction.bytes()).get
      recovered.bytes() shouldEqual transaction.bytes()
    }
  }

  property("JSON format validation") {
    val js = Json.parse("""{
 "type": 14,
 "id": "Gobt7AiyQAfduRkW8Mk3naWbzH67Zsv9rdmgRNmon1Mb",
 "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
 "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
 "fee": 100000000,
 "timestamp": 1520945679531,
 "proofs": [
 "3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7"
 ],
 "version": 1,
 "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
 "minSponsoredAssetFee": 100000
                       }
    """)

    val tx = SponsorFeeTransaction
      .create(
        1,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get,
        Some(100000),
        100000000L,
        1520945679531L,
        Proofs(Seq(ByteStr.decodeBase58("3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7").get))
      )
      .right
      .get
    js shouldEqual tx.json()
  }

  val invalidFee =
    Table(
      "fee",
      -1 * Constants.UnitsInWave,
      0
    )

  property("sponsorship negative fee") {
    forAll(invalidFee) { fee: Long =>
      for {
        sender                                                                       <- accountGen
        (_, assetName, description, quantity, decimals, reissuable, iFee, timestamp) <- issueParamGen
        issue = IssueTransactionV1
          .selfSigned(sender, assetName, description, quantity, decimals, reissuable = reissuable, iFee, timestamp)
          .right
          .get
        minFee <- smallFeeGen
        assetId = issue.assetId()
      } yield SponsorFeeTransaction.selfSigned(1, sender, assetId, Some(minFee), fee, timestamp) should produce("insufficient fee")
    }
  }

  property("cancel sponsorship negative fee") {
    forAll(invalidFee) { fee: Long =>
      for {
        sender                                                                       <- accountGen
        (_, assetName, description, quantity, decimals, reissuable, iFee, timestamp) <- issueParamGen
        issue = IssueTransactionV1
          .selfSigned(sender, assetName, description, quantity, decimals, reissuable = reissuable, iFee, timestamp)
          .right
          .get
        minFee  = None
        assetId = issue.assetId()
      } yield SponsorFeeTransaction.selfSigned(1, sender, assetId, minFee, fee, timestamp) should produce("insufficient fee")
    }
  }

}
