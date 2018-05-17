package scorex.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.state.ByteStr
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json
import scorex.account.{PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.transaction.assets.{IssueTransaction, IssueTransactionV1}

class IssueTransactionV1Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Issue serialization roundtrip") {
    forAll(issueGen) { issue: IssueTransaction =>
      val recovered = issue.builder.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

  property("Issue serialization from TypedTransaction") {
    forAll(issueGen) { issue: IssueTransaction =>
      val recovered = TransactionParsers.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

  property("JSON format validation") {
    val js = Json.parse("""{
                       "type": 3,
                       "id": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
                       "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000000,
                       "timestamp": 1526287561757,
                       "version": 1,
                       "signature": "28kE1uN1pX2bwhzr9UHw5UuB9meTFEDFgeunNgy6nZWpHX4pzkGYotu8DhQ88AdqUG6Yy5wcXgHseKPBUygSgRMJ",
                       "name": "Gigacoin",
                       "quantity": 10000000000,
                       "reissuable": true,
                       "decimals": 8,
                       "description": "Gigacoin",
                       "script":null
                       }
    """)

    val tx = IssueTransactionV1
      .create(
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").right.get,
        "Gigacoin".getBytes,
        "Gigacoin".getBytes,
        10000000000L,
        8,
        true,
        100000000,
        1526287561757L,
        ByteStr(Base58.decode("28kE1uN1pX2bwhzr9UHw5UuB9meTFEDFgeunNgy6nZWpHX4pzkGYotu8DhQ88AdqUG6Yy5wcXgHseKPBUygSgRMJ").get)
      )
      .right
      .get

    js shouldEqual tx.json()
  }

}
