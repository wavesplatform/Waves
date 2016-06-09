package scorex.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.encode.Base58

class GenesisTransactionSpecification extends PropSpec with PropertyChecks with Matchers {

  val defaultRecipient = new PublicKeyAccount(Array.fill(32)(0: Byte))

  property("GenesisTransaction Signature should be the same") {
    val balance = 457L
    val timestamp = 2398762345L
    val signature = GenesisTransaction.generateSignature(defaultRecipient, balance, timestamp)

    val expected = "3dzTukYksn9UK3fCwuYQsGrjzqjqUEXyvWXCBmFQtCiKF8YZK6eutuhuBmJhzNPm1vRz6SXcEJnyMzGRnKULgdnK"
    val actual = Base58.encode(signature)

    assert(actual == expected)
  }

  property("GenesisTransaction parse from Bytes should work fine") {
    val bytes = Base58.decode("5GoidY2PcCc7ENdrcapZcmmdq2H57NuiXEdgVkpfnnzkB4o8R575WVR1Xw").get

    val actualTransaction = GenesisTransaction.parseBytes(bytes).get

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
      val dest = GenesisTransaction.parseBytes(bytes).get

      source should equal(dest)
    }
  }

}
