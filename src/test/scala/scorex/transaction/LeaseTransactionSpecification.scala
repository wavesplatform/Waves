package scorex.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.state.ByteStr
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json
import scorex.account.{Address, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.transaction.lease.{LeaseTransaction, LeaseTransactionV1, LeaseTransactionV2}

class LeaseTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Lease transaction serialization roundtrip") {
    forAll(leaseGen) { tx: LeaseTransaction =>
      val recovered = tx.builder.parseBytes(tx.bytes()).get.asInstanceOf[LeaseTransaction]
      assertTxs(recovered, tx)
    }
  }

  property("Lease transaction from TransactionParser") {
    forAll(leaseGen) { tx: LeaseTransaction =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[LeaseTransaction], tx)
    }
  }

  private def assertTxs(first: LeaseTransaction, second: LeaseTransaction): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.recipient.stringRepr shouldEqual second.recipient.stringRepr
    first.amount shouldEqual second.amount
    first.fee shouldEqual second.fee
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }

  property("JSON format validation for LeaseTransactionV1") {
    val js = Json.parse("""{
                       "type": 8,
                       "id": "EXhjYjy8a1dURbttrGzfcft7cddDnPnoa3vqaBLCTFVY",
                       "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 1000000,
                       "timestamp": 1526646300260,
                       "signature": "iy3TmfbFds7pc9cDDqfjEJhfhVyNtm3GcxoVz8L3kJFvgRPUmiqqKLMeJGYyN12AhaQ6HvE7aF1tFgaAoCCgNJJ",
                       "version": 1,
                       "amount": 10000000,
                       "recipient": "3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd"
                       }
    """)

    val tx = LeaseTransactionV1
      .create(
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").right.get,
        10000000,
        1000000,
        1526646300260L,
        Address.fromString("3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd").right.get,
        ByteStr(Base58.decode("iy3TmfbFds7pc9cDDqfjEJhfhVyNtm3GcxoVz8L3kJFvgRPUmiqqKLMeJGYyN12AhaQ6HvE7aF1tFgaAoCCgNJJ").get)
      )
      .right
      .get

    js shouldEqual tx.json()
  }

  property("JSON format validation for LeaseTransactionV2") {
    val js = Json.parse("""{
                        "type": 8,
                        "id": "DJWkQxRyJNqWhq9qSQpK2D4tsrct6eZbjSv3AH4PSha6",
                        "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                        "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                        "fee": 1000000,
                        "timestamp": 1526646497465,
                        "proofs": [
                        "5Fr3yLwvfKGDsFLi8A8JbHqToHDojrPbdEGx9mrwbeVWWoiDY5pRqS3rcX1rXC9ud52vuxVdBmGyGk5krcgwFu9q"
                        ],
                        "version": 2,
                        "amount": 10000000,
                        "recipient": "3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd"
                       }
    """)

    val tx = LeaseTransactionV2
      .create(
        2,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").right.get,
        10000000,
        1000000,
        1526646497465L,
        Address.fromString("3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd").right.get,
        Proofs(Seq(ByteStr(Base58.decode("5Fr3yLwvfKGDsFLi8A8JbHqToHDojrPbdEGx9mrwbeVWWoiDY5pRqS3rcX1rXC9ud52vuxVdBmGyGk5krcgwFu9q").get)))
      )
      .right
      .get

    js shouldEqual tx.json()
  }
}
