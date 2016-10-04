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

  property("transaction signature should be valid in a valid flow") {
    forAll { (senderSeed: Array[Byte],
              recipientSeed: Array[Byte],
              time: Long,
              amount: Long,
              fee: Long) =>
      val sender = new PrivateKeyAccount(senderSeed)
      val recipient = new PrivateKeyAccount(recipientSeed)

      val tx = PaymentTransaction(sender, recipient, amount, fee, time)
      tx.signatureValid should be(true)
    }
  }

  property("wrong transaction signature should be invalid") {
    forAll { (senderSeed: Array[Byte],
              recipientSeed: Array[Byte],
              time: Long,
              amount: Long,
              fee: Long) =>
      val sender = new PrivateKeyAccount(senderSeed)
      val recipient = new PrivateKeyAccount(recipientSeed)

      val sig = PaymentTransaction.generateSignature(sender, recipient, amount, fee, time)

      PaymentTransaction(sender, recipient, amount, fee + 1, time, sig).signatureValid should be(false)
      PaymentTransaction(sender, recipient, amount, fee, time + 1, sig).signatureValid should be(false)
      PaymentTransaction(sender, recipient, amount + 1, fee, time + 1, sig).signatureValid should be(false)
      PaymentTransaction(recipient, sender, amount + 1, fee, time + 1, sig).signatureValid should be(false)
      PaymentTransaction(recipient, sender, amount, fee, time, (sig.toList :+ 1.toByte).toArray).signatureValid should be(false)
    }
  }

  property("transaction fields should be constructed in a right way") {
    forAll { (senderSeed: Array[Byte],
              recipientSeed: Array[Byte],
              time: Long,
              amount: Long,
              fee: Long) =>

      val sender = new PrivateKeyAccount(senderSeed)
      val recipient = new PrivateKeyAccount(recipientSeed)

      val tx = PaymentTransaction(sender, recipient, amount, fee, time)

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
      val tx = PaymentTransaction(sender, recipient, amount, fee, time)
      val txAfter = PaymentTransaction.parseBytes(tx.bytes).get

      txAfter.getClass.shouldBe(tx.getClass)

      tx.dataLength shouldEqual txAfter.dataLength
      tx.signature shouldEqual txAfter.signature
      tx.sender shouldEqual txAfter.asInstanceOf[PaymentTransaction].sender
      tx.recipient shouldEqual txAfter.recipient
      tx.timestamp shouldEqual txAfter.timestamp
      tx.amount shouldEqual txAfter.amount
      tx.fee shouldEqual txAfter.fee
      txAfter.signatureValid shouldEqual true
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
        val tx = PaymentTransaction(sender, recipient, amount, fee, time)
        val txAfter = TypedTransaction.parseBytes(tx.bytes).get.asInstanceOf[LagonakiTransaction]

        txAfter.getClass.shouldBe(tx.getClass)

        tx.dataLength shouldEqual txAfter.dataLength
        tx.signature shouldEqual txAfter.signature
        tx.sender shouldEqual txAfter.asInstanceOf[PaymentTransaction].sender
        tx.recipient shouldEqual txAfter.recipient
        tx.timestamp shouldEqual txAfter.timestamp
        tx.amount shouldEqual txAfter.amount
        tx.fee shouldEqual txAfter.fee
        txAfter.signatureValid shouldEqual true
    }
  }

}