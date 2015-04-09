package scorex.test

import org.scalatest.FunSuite
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction.PaymentTransaction

import scala.util.Random

class TransactionSpecification extends FunSuite {

  test("tx construction") {
    val sender = new PrivateKeyAccount(Random.nextString(32).getBytes)
    val recipient = new PublicKeyAccount(Random.nextString(32).getBytes)
    val time = System.currentTimeMillis()

    val amount = BigDecimal(5)
    val fee = BigDecimal(1)
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

    val sig = PaymentTransaction.generateSignature(sender, recipient, BigDecimal(5), BigDecimal(1), time)

    val tx = PaymentTransaction(sender, recipient, BigDecimal(5), BigDecimal(1), time, sig)

    assert(tx.isSignatureValid())
  }

}