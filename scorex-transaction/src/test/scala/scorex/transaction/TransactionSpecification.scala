package scorex.transaction

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.account.PrivateKeyAccount


class TransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("transaction fields should be constructed in a right way") {
    forAll(bytes32gen, bytes32gen, positiveLongGen, positiveLongGen, positiveLongGen) {
      (senderSeed: Array[Byte], recipientSeed: Array[Byte], time: Long, amount: Long, fee: Long) =>

        val sender = new PrivateKeyAccount(senderSeed)
        val recipient = new PrivateKeyAccount(recipientSeed)

        val tx = PaymentTransaction.create(sender, recipient, amount, fee, time).right.get

        tx.timestamp shouldEqual time
        tx.amount shouldEqual amount
        tx.fee shouldEqual fee
        tx.sender shouldEqual sender
        tx.recipient shouldEqual recipient
    }
  }

  property("bytes()/parse() roundtrip should preserve a transaction") {
    forAll(bytes32gen, bytes32gen, positiveLongGen, positiveLongGen, positiveLongGen) {
      (senderSeed: Array[Byte], recipientSeed: Array[Byte], time: Long, amount: Long, fee: Long) =>

        val sender = new PrivateKeyAccount(senderSeed)
        val recipient = new PrivateKeyAccount(recipientSeed)
        val tx = PaymentTransaction.create(sender, recipient, amount, fee, time).right.get
        val txAfter = PaymentTransaction.parseBytes(tx.bytes).get

        txAfter.getClass.shouldBe(tx.getClass)

        tx.signature shouldEqual txAfter.signature
        tx.sender shouldEqual txAfter.asInstanceOf[PaymentTransaction].sender
        tx.recipient shouldEqual txAfter.recipient
        tx.timestamp shouldEqual txAfter.timestamp
        tx.amount shouldEqual txAfter.amount
        tx.fee shouldEqual txAfter.fee
    }
  }

  property("PaymentTransaction should deserialize to LagonakiTransaction") {
    forAll(bytes32gen, bytes32gen, positiveLongGen, positiveLongGen, positiveLongGen) {
      (senderSeed: Array[Byte], recipientSeed: Array[Byte], time: Long, amount: Long, fee: Long) =>

        val sender = new PrivateKeyAccount(senderSeed)
        val recipient = new PrivateKeyAccount(recipientSeed)
        val tx = PaymentTransaction.create(sender, recipient, amount, fee, time).right.get
        val txAfter = TypedTransaction.parseBytes(tx.bytes).get.asInstanceOf[PaymentTransaction]

        txAfter.getClass.shouldBe(tx.getClass)

        tx.signature shouldEqual txAfter.signature
        tx.sender shouldEqual txAfter.asInstanceOf[PaymentTransaction].sender
        tx.recipient shouldEqual txAfter.recipient
        tx.timestamp shouldEqual txAfter.timestamp
        tx.amount shouldEqual txAfter.amount
        tx.fee shouldEqual txAfter.fee
    }
  }

}