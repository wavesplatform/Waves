package scorex.unit

import org.scalatest.FunSuite
import scorex.crypto.Base58
import scorex.wallet.Wallet

import scala.util.Random

class WalletSpecification extends FunSuite {
  private val walletSize = 10



  test("wallet deletion") {
    val wf = new java.io.File(s"/tmp/wallet${Random.nextLong()}.dat")

    val w = new Wallet(wf, "cookies", Base58.decode("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").get)

    w.generateNewAccounts(walletSize)

    assert(w.privateKeyAccounts().size == walletSize)

    val head = w.privateKeyAccounts().head
    w.deleteAccount(head)
    assert(w.privateKeyAccounts().size == walletSize - 1)

    w.deleteAccount(w.privateKeyAccounts().head)
    assert(w.privateKeyAccounts().size == walletSize - 2)

    w.privateKeyAccounts().foreach(w.deleteAccount)

    assert(w.privateKeyAccounts().size == 0)
  }


  /*

  todo: report MapDb bug with reopening a database

  test("double creation"){
    val wf = new java.io.File(s"/tmp/wallet${Random.nextLong()}.dat")

    val w = new Wallet(wf, "cookies", Base58.decode("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz"))
    w.generateNewAccounts(10)
    w.close()
    assert(w.exists())

    val w2 = new Wallet(wf, "cookies", Base58.decode("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz"))
    assert(w2.privateKeyAccounts().head.address != null)
  }

  */
}