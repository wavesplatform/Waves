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
    w.privateKeyAccounts().map(_.address) shouldBe Seq("3MhJGoYuqRYd6WFghDAbG6AxCXzBUVHSJ35", "3MVqKV9kwTgoGbx1eZ6QLT5Dz4BKTX1sUNs", "3MeR3pXBRix3TRW1R2kyttqQazyZziGP1x2", "3Mc4YWPC8BMaQ2GoCrssvVAKGAzvfw1fkEi", "3MWLKYDGfdiuuG19XDFwHYNXL9qFqdbN81C", "3MaYTuhKH4YevEBsbmVB7Nf8izJhzAdeuds", "3MWbthBrqXLq2wXf1qCFRxxcRTkZyQ8xU1j", "3MS2LxBCnCrKZQn9QVPMow58YVubvoBJRam", "3Mm67gNx1BWtLxmo6ZCCQFFuFFUujg2vVXS", "3MSTv53kBZ15bJBNwGyXmh5j1FrwBgAo9Pb")

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