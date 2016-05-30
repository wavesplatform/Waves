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
    w.privateKeyAccounts().map(_.address) shouldBe Seq("3Mb4mR4taeYS3wci78SntztFwLoaS6Wbg81", "3MkcXZiczXxqEQYVVhRqkUnVaTfzaeMs15e", "3MoFqpXVnxVpMwN1egMj2yH6EEWWYVruvfn", "3MjLQqH3cU6DZ4yFC6SAPmXkHf3ajWmV3LK", "3MbWTyn6Tg7zL6XbdN8TLcFMfhWX77hKcmc", "3MUc8z9WMuMbRR93NU8Jxh1DgwCkfHr78Fu", "3Mo4k51wnV4EYmCmneh4fqDdfKYxcgZgCUT", "3MiSmwnRGJtLmZeNLYg75bje6StYhCEKCrX", "3MjQ5FJEBGggnwBWBNjZtgcwyrYTn4r4shg", "3MkjMybqczyo6SyouuaSniTuSiZqTsMhQQx")

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