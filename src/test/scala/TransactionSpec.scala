package scorex.test

import org.scalatest.FunSuite
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction.PaymentTransaction

import scala.util.Random

class TransactionSpec extends FunSuite {

  test("transaction signing") {
    val sender = new PrivateKeyAccount(Random.nextString(32).getBytes)
    val recipient = new PublicKeyAccount(Random.nextString(32).getBytes)

    val time = System.currentTimeMillis()

    val sig = PaymentTransaction.generateSignature(sender, recipient, BigDecimal(5), BigDecimal(1), time)

    val tx = PaymentTransaction(sender, recipient, BigDecimal(5), BigDecimal(1), time, sig)

    assert(tx.isSignatureValid())
  }
  
}