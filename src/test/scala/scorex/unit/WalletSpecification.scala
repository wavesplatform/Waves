package scorex.unit

import org.scalatest.FunSuite
import scorex.crypto.Base58
import scorex.wallet.Wallet

import scala.util.Random

class WalletSpecification extends FunSuite {
  val wf = new java.io.File(s"/tmp/wallet${Random.nextLong()}.dat")

  //todo: double creation test - check for null after reopening

  test("wallet deletion") {
    val size0 = 10
    Wallet.create(Base58.decode("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz"), "cookies", 10, wf)
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