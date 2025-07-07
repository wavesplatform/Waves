package com.wavesplatform.transaction

import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.finalization.BlsPublicKey
import com.wavesplatform.state.Height
import com.wavesplatform.test.PropSpec
import play.api.libs.json.Json

class CommitToGenerationTransactionsSpec extends PropSpec {
  property("JSON format validation") {
    val js = Json.parse("""{
      "type": 20,
      "id": "85pcpRZXje2UXvPEeXov9newAhuV4kDMZqqzc9QqniU1",
      "fee": 100000000,
      "feeAssetId": null,
      "timestamp": 1526287561757,
      "version": 1,
      "chainId": 84,
      "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
      "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
      "proofs": [
        "28kE1uN1pX2bwhzr9UHw5UuB9meTFEDFgeunNgy6nZWpHX4pzkGYotu8DhQ88AdqUG6Yy5wcXgHseKPBUygSgRMJ"
      ],
      "generationPeriodStart": 10000,
      "endorsementPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z"
    }""")

    val tx = CommitToGenerationTransaction(
      sender = PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
      fee = TxPositiveAmount.unsafeFrom(100000000),
      timestamp = 1526287561757L,
      proofs = Proofs(ByteStr.decodeBase58("28kE1uN1pX2bwhzr9UHw5UuB9meTFEDFgeunNgy6nZWpHX4pzkGYotu8DhQ88AdqUG6Yy5wcXgHseKPBUygSgRMJ").get),
      generationPeriodStart = Height(10000),
      endorsementPublicKey = BlsPublicKey(ByteStr.decodeBase58("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").get), // TODO:
      chainId = AddressScheme.current.chainId
    )

    tx.json() shouldEqual js
  }
}
