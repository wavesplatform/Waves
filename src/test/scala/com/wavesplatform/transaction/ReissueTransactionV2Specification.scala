package com.wavesplatform.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.state.{ByteStr, EitherExt2}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.transaction.assets.ReissueTransactionV2

class ReissueTransactionV2Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("JSON format validation") {
    val js = Json.parse("""{
                       "type": 5,
                       "id": "HbQ7gMoDyRxSU6LbLLBVNTbxASaR8rm4Zck6eYvWVUkB",
                       "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000000,
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
    """)

    val tx = ReissueTransactionV2
      .create(
        2,
        'T',
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get,
        100000000L,
        true,
        100000000L,
        1526287561757L,
        Proofs(Seq(ByteStr.decodeBase58("4DFEtUwJ9gjMQMuEXipv2qK7rnhhWEBqzpC3ZQesW1Kh8D822t62e3cRGWNU3N21r7huWnaty95wj2tZxYSvCfro").get))
      )
      .right
      .get

    js shouldEqual tx.json()
  }

}
