package scorex.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.state.ByteStr
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json
import scorex.account.PublicKeyAccount
import scorex.transaction.assets.SponsorFeeTransaction

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
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").right.get,
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

}
