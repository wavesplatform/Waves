package scorex.lagonaki.unit

import org.scalatest.FunSuite
import scorex.crypto.Base58
import scorex.wallet.Wallet

import scala.util.Random

class WalletSpecification extends FunSuite {

  private val walletSize = 10

  test("wallet - acc deletion") {

    val w = new Wallet(None, "cookies", Base58.decode("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").get)

    w.generateNewAccounts(walletSize)

    assert(w.privateKeyAccounts().size == walletSize)

    val head = w.privateKeyAccounts().head
    w.deleteAccount(head)
    assert(w.privateKeyAccounts().size == walletSize - 1)

    w.deleteAccount(w.privateKeyAccounts().head)
    assert(w.privateKeyAccounts().size == walletSize - 2)

    w.privateKeyAccounts().foreach(w.deleteAccount)

    assert(w.privateKeyAccounts().isEmpty)
  }

  test("reopening"){

    //todo read folder from settings
    val walletFile = new java.io.File(s"/tmp/wallet${Random.nextLong()}.dat")

    val w = new Wallet(Some(walletFile), "cookies", Base58.decode("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").get)
    w.generateNewAccounts(10)
    w.close()
    assert(w.exists())

    val w2 = new Wallet(Some(walletFile), "cookies", Base58.decode("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").get)
    assert(w2.privateKeyAccounts().head.address != null)
  }
}