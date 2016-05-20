package scorex.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.{PrivateKeyAccount, Account}
import scorex.crypto.encode.Base58

class GenesisTransactionSpecification extends PropSpec with PropertyChecks with Matchers {

  val defaultRecipient = new Account("jACSbUoHi4eWgNu6vzAnEx583NwmUAVfS")

  property("GenesisTransaction Signature should be the same") {
    val balance = 457L
    val timestamp = 2398762345L
    val signature = GenesisTransaction.generateSignature(defaultRecipient, balance, timestamp)

    val expected = "cSnrRhxLnMHcsLs2tSi7aGw4xMZzvtc2WMmC3x73emXKg9JY86XYsvhx1NPD2c1bqnCU6AF2y5E6UR7njK2FReu"
    val actual = Base58.encode(signature)

    assert(actual == expected)
  }

  property("GenesisTransaction parse from Bytes should work fine") {
    val bytes = Base58.decode("y9KVfkMyuvik3koq45snK6vP27L12VdmuNSCVBvqPMsESvpDiUjT5dyb").get

    val actualTransaction = GenesisTransaction.parse(bytes.tail)

    val balance = 149857264546L
    val timestamp = 4598723454L
    val expectedTransaction = new GenesisTransaction(defaultRecipient, balance, timestamp)

    actualTransaction should equal(expectedTransaction)
  }

  property("GenesisTransaction serialize/deserialize roundtrip") {
    forAll { (recipientSeed: Array[Byte],
              time: Long,
              amount: Long) =>
      val recipient = new PrivateKeyAccount(recipientSeed)
      val source = new GenesisTransaction(recipient, amount, time)
      val bytes = source.bytes
      val dest = GenesisTransaction.parse(bytes.tail)

      source should equal(dest)
    }
  }

}
