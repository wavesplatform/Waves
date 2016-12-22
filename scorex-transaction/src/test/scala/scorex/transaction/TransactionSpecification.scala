package scorex.transaction

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.PrivateKeyAccount
import scorex.crypto.encode.Base58
import scorex.transaction.assets.IssueTransaction


class TransactionSpecification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers {

  property("transaction fields should be constructed in a right way") {
    forAll { (senderSeed: Array[Byte],
              recipientSeed: Array[Byte],
              time: Long,
              amount: Long,
              fee: Long) =>

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
    forAll { (senderSeed: Array[Byte],
              recipientSeed: Array[Byte],
              time: Long,
              amount: Long,
              fee: Long) =>

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

  property("PaymentTransaction vector") {
    val bytes = Base58.decode("CzPyJt4mEh3Q8rW3vJG6sdZFwwKDkoYkFDVXhTUUVVPqr3FfBW5UCJHTd3eoVrx1ukbuQiGM3953GB8VJ7yMCePHBMB5spX9D4F6vyBJWva7ZfbG9APje3iKNoBDJLSnPRQCcZUDRYBsSJYL1GGVay25CV9W8aRqjAuyvVVC54TiwExFbaNZ3MrmHaZRr2fS1rjRj38Z").get
    val actualTransaction = PaymentTransaction.parseBytes(bytes).get

    actualTransaction.fee shouldBe 2
    actualTransaction.amount shouldBe 123L
    actualTransaction.sender.address shouldBe "3MTNTc6nrfHrftucagvRyw9PfXffvgJXs6z"
    actualTransaction.recipient.address shouldBe "3MV9Z4owZGKUGuvTStvb7ByeGTmjmXUaQSU"
    actualTransaction.timestamp shouldBe 9223372036854775807L
    Base58.encode(actualTransaction.signature) shouldBe "4ZA1mJgFRSwcCiKQ1Qhb6KKgTBJ1Zt714XWWq2rW6x45ywSfBunZoAuutWduynZBFx3qCb3t8dWnGw1pgtgShuND"
  }

  property("PaymentTransaction should deserialize to LagonakiTransaction") {
    forAll {
      (senderSeed: Array[Byte],
       recipientSeed: Array[Byte],
       time: Long,
       amount: Long,
       fee: Long) =>

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