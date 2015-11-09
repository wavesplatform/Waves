package scorex.transaction.props

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.{PropertyChecks, GeneratorDrivenPropertyChecks}
import scorex.account.PrivateKeyAccount
import scorex.transaction.{LagonakiTransaction, PaymentTransaction}


class TransactionSpecification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers {

  property("transaction signature should be valid in a valid flow") {
    forAll { (senderSeed: Array[Byte],
              recipientSeed: Array[Byte],
              time:Long,
              amount:Long,
              fee:Long) =>
      val sender = new PrivateKeyAccount(senderSeed)
      val recipient = new PrivateKeyAccount(recipientSeed)

      val tx = PaymentTransaction(sender, recipient, amount, fee, time)
      tx.isSignatureValid() should be (true)
    }
  }

  property("wrong transaction signature should be invalid") {
    forAll { (senderSeed: Array[Byte],
              recipientSeed: Array[Byte],
              time:Long,
              amount:Long,
              fee:Long) =>
      val sender = new PrivateKeyAccount(senderSeed)
      val recipient = new PrivateKeyAccount(recipientSeed)

      val sig = PaymentTransaction.generateSignature(sender, recipient, amount, fee, time)

      PaymentTransaction(sender, recipient, amount, fee+1, time, sig).isSignatureValid() should be (false)
      PaymentTransaction(sender, recipient, amount, fee, time+1, sig).isSignatureValid() should be (false)
      PaymentTransaction(sender, recipient, amount+1, fee, time+1, sig).isSignatureValid() should be (false)
      PaymentTransaction(recipient, sender, amount+1, fee, time+1, sig).isSignatureValid() should be (false)
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
      val txAfter = LagonakiTransaction.parse(tx.bytes())

      txAfter.getClass.shouldBe(tx.getClass)

      tx.dataLength shouldEqual txAfter.dataLength
      tx.signature shouldEqual txAfter.signature
      tx.sender shouldEqual txAfter.asInstanceOf[PaymentTransaction].sender
      tx.recipient shouldEqual txAfter.recipient
      tx.timestamp shouldEqual txAfter.timestamp
      tx.amount shouldEqual txAfter.amount
      tx.fee shouldEqual txAfter.fee
      txAfter.isSignatureValid() shouldEqual true
    }
  }
}