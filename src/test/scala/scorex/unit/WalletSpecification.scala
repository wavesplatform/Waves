package scorex.unit

import org.scalatest.FunSuite
import scorex.crypto.Base58
import scorex.wallet.Wallet

class WalletSpecification extends FunSuite {

  test("wallet deletion") {
    val size0 = 10
    Wallet.create(Base58.decode("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz"), "cookies", 10)
    assert(Wallet.privateKeyAccounts().size == size0)

    println(Wallet.privateKeyAccounts().mkString(" : "))
    val head = Wallet.privateKeyAccounts().head
    assert(head != null)
    Wallet.deleteAccount(head)
    assert(Wallet.privateKeyAccounts().size == size0 - 1)

    Wallet.deleteAccount(Wallet.privateKeyAccounts().head)
    assert(Wallet.privateKeyAccounts().size == size0 - 2)
  }

}