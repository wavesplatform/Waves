package scorex.unit

import org.scalatest.FunSuite
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction.{LagonakiTransaction, PaymentTransaction}

import scala.util.Random

class TransactionSpecification extends FunSuite {

  test("tx construction") {
    val sender = new PrivateKeyAccount(Random.nextString(32).getBytes)
    val recipient = new PublicKeyAccount(Random.nextString(32).getBytes)
    val time = System.currentTimeMillis()

    val amount = 5
    val fee = 1
    val sig = PaymentTransaction.generateSignature(sender, recipient, amount, fee, time)

    val tx = PaymentTransaction(sender, recipient, amount, fee, time, sig)

    assert(tx.recipient == recipient)
    assert(tx.sender == sender)
    assert(tx.amount == amount)
    assert(tx.fee == fee)
  }

  test("tx signing") {
    val sender = new PrivateKeyAccount(Random.nextString(32).getBytes)
    val recipient = new PublicKeyAccount(Random.nextString(32).getBytes)

    val time = System.currentTimeMillis()

    val sig = PaymentTransaction.generateSignature(sender, recipient, 5, 1, time)
    val sig2 = PaymentTransaction.generateSignature(sender, recipient, 5, 10, time)

    val tx = PaymentTransaction(sender, recipient, 5, 1, time, sig)
    val tx2 = PaymentTransaction(sender, recipient, 5, 1, time, sig2)
    val tx3 = PaymentTransaction(sender, recipient, 5, 1, time)

    assert(tx3.signature.sameElements(sig))

    assert(tx.isSignatureValid())
    assert(!tx2.isSignatureValid())
  }

  test("toBytes/parse roundtrip") {
    val sender = new PrivateKeyAccount(Random.nextString(32).getBytes)
    val recipient = new PublicKeyAccount(Random.nextString(32).getBytes)
    val time = System.currentTimeMillis()

    val amount = 5
    val fee = 1
    val tx = PaymentTransaction(sender, recipient, amount, fee, time)

    val txAfter = LagonakiTransaction.parse(tx.bytes())
    assert(tx.fee == txAfter.fee)
    assert(tx.amount == txAfter.amount)
  }
}