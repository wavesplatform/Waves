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
    w.privateKeyAccounts().map(_.address) shouldBe Seq("3MqMwwHW4v2nSEDHVWoh8RCQL8QrsWLkkeB", "3MuwVgJA8EXHukxo6rcakT5tD6FpvACtitG", "3MuAvUG4EAsG9RP9jaWjewCVmggaQD2t39B", "3MqoX4A3UGBYU7cX2JPs6BCzntNC8K8FBR4", "3N1Q9VVVQtY3GqhwHtJDEyHb3oWBcerZL8X", "3NARifVFHthMDnCwBacXijPB2szAgNTeBCz", "3N6dsnfD88j5yKgpnEavaaJDzAVSRBRVbMY", "3MufvXKZxLuNn5SHcEgGc2Vo7nLWnKVskfJ", "3Myt4tocZmj7o3d1gnuWRrnQWcoxvx5G7Ac", "3N3keodUiS8WLEw9W4BKDNxgNdUpwSnpb3K")

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

    val w2 = new Wallet(Some(walletFile), "cookies", None)
    w2.privateKeyAccounts().head.address should not be null
    w2.nonce() shouldBe nonce
  }
}