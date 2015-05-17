package scorex.unit

import org.scalatest.FunSuite
import scorex.crypto.Base58
import scorex.wallet.Wallet

import scala.util.Random

class WalletSpecification extends FunSuite {
  private val wf = new java.io.File(s"/tmp/wallet${Random.nextLong()}.dat")
  private val walletSize = 10

  test("double creation"){
    Wallet.create(Base58.decode("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz"), "cookies", walletSize, wf)
    Wallet.close()
    Wallet.create(Base58.decode("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz"), "cookies", walletSize, wf)
    assert(Wallet.privateKeyAccounts().head.address != null)
  }

  test("wallet deletion") {

    Wallet.create(Base58.decode("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz"), "cookies", walletSize, wf)
    assert(Wallet.privateKeyAccounts().size == walletSize)

    val head = Wallet.privateKeyAccounts().head
    Wallet.deleteAccount(head)
    assert(Wallet.privateKeyAccounts().size == walletSize - 1)

    Wallet.deleteAccount(Wallet.privateKeyAccounts().head)
    assert(Wallet.privateKeyAccounts().size == walletSize - 2)

    Wallet.privateKeyAccounts().foreach(Wallet.deleteAccount)

    assert(Wallet.privateKeyAccounts().size == 0)
  }

}