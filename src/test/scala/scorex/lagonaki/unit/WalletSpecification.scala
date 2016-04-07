package scorex.lagonaki.unit

import org.scalatest.{Matchers, FunSuite}
import scorex.crypto.encode.Base58
import scorex.wallet.Wallet

import scala.util.Random

class WalletSpecification extends FunSuite with Matchers {

  private val walletSize = 10
  val w = new Wallet(None, "cookies", Base58.decode("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").toOption)

  test("wallet - acc creation") {
    w.generateNewAccounts(walletSize)

    w.privateKeyAccounts().size shouldBe walletSize
    w.privateKeyAccounts().map(_.address) shouldBe Seq("o1y1pkQyXCDWrtNQZ9K4u6KW1RiFZTnJ1", "UNGBSkyvNyGzdv6CKjEndDnChfpzWJ56E", "jACSbUoHi4eWgNu6vzAnEx583NwmUAVfS", "j6Y2aJEV7bQeU7updafs9kNvYVuHRWgcu", "kNem1scYzD5z3NDRdG2aQVfjAukMDZHCC", "aptcN9CfZouX7apreDB6WG2cJVbkos881", "iCu95ftKuid99F4GsXMhMeBiPTrs9KLfq", "kVVAu6F21Ax2Ugddms4p5uXz4kdZfAp8g", "bGbB5M5h9NBg2UM6KschsMky1SGm2Gdum", "npsGKCQW5cQLheWNtUwvqdkb3snPUo25b")

  }

  test("wallet - acc deletion") {

    val head = w.privateKeyAccounts().head
    w.deleteAccount(head)
    assert(w.privateKeyAccounts().size == walletSize - 1)

    w.deleteAccount(w.privateKeyAccounts().head)
    assert(w.privateKeyAccounts().size == walletSize - 2)

    w.privateKeyAccounts().foreach(w.deleteAccount)

    assert(w.privateKeyAccounts().isEmpty)
  }

  test("reopening") {

    //todo read folder from settings
    val walletFile = new java.io.File(s"/tmp/wallet${Random.nextLong()}.dat")

    val w = new Wallet(Some(walletFile), "cookies", Base58.decode("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").toOption)
    w.generateNewAccounts(10)
    val nonce = w.nonce()
    w.close()
    assert(w.exists())

    val w2 = new Wallet(Some(walletFile), "cookies", None)
    w2.privateKeyAccounts().head.address should not be null
    w2.nonce() shouldBe nonce
  }
}