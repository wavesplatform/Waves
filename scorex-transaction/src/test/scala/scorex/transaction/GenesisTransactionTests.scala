package scorex.transaction

import org.joda.time.DateTime
import org.scalatest._
import scorex.account.Account

class GenesisTransactionTests extends FlatSpec with Matchers {

  val defaultRecipient = new Account("jACSbUoHi4eWgNu6vzAnEx583NwmUAVfS");

  "GenesisTransaction Signature" should "be the same" in {
    val balance = 457L
    val timestamp = 2398762345L
    val signature = GenesisTransaction.generateSignature(defaultRecipient, balance, timestamp)

    val expected = "1E90EE1ECD68C137009F64F546198E6871D199E3D97AD48A35D3B264B089BD761E90EE1ECD68C137009F64F546198E6871D199E3D97AD48A35D3B264B089BD76"
    val actual = Hex.valueOf(signature)

    assert(actual == expected)
  }

  "GenesisTransaction parse from Bytes" should "work fine" in {
    val serialized = "0100000001121AF37E01CE7293C877DC3FC457A634CE87FC0A9306B77D0108FD3A3D00000022E43063A2"

    val bytes = Hex.fromString(serialized)

    val actualTransaction = GenesisTransaction.parseTransactionData(bytes.tail)

    val balance = 149857264546L
    val timestamp = 4598723454L
    val expectedTransaction = new GenesisTransaction(defaultRecipient, balance, timestamp)

    actualTransaction should equal(expectedTransaction)
  }

  "GenesisTransaction" should "serialize/deserialize" in {
    val source = new GenesisTransaction(defaultRecipient, 3459L, 123456L)

    val bytes = source.bytes

    val dest = GenesisTransaction.parseTransactionData(bytes.tail)

    source should equal (dest)
  }

  object Hex {
    def valueOf(buf: Array[Byte]): String = buf.map("%02X" format _).mkString

    def fromString(str: String): Array[Byte] = str.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
  }

}
