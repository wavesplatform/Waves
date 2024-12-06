package com.wavesplatform.lagonaki.unit

import java.io.File
import java.nio.file.Files

import cats.syntax.option.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.settings.WalletSettings
import com.wavesplatform.test.FunSuite
import com.wavesplatform.wallet.Wallet

class WalletSpecification extends FunSuite {

  private val walletSize = 10
  val w                  = Wallet(WalletSettings(None, "cookies".some, ByteStr.decodeBase58("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").toOption))

  test("wallet - acc creation") {
    w.generateNewAccounts(walletSize)

    w.privateKeyAccounts.size shouldBe walletSize
    w.privateKeyAccounts.map(_.toAddress.toString) shouldBe Seq(
      "3MqMwwHW4v2nSEDHVWoh8RCQL8QrsWLkkeB",
      "3MuwVgJA8EXHukxo6rcakT5tD6FpvACtitG",
      "3MuAvUG4EAsG9RP9jaWjewCVmggaQD2t39B",
      "3MqoX4A3UGBYU7cX2JPs6BCzntNC8K8FBR4",
      "3N1Q9VVVQtY3GqhwHtJDEyHb3oWBcerZL8X",
      "3NARifVFHthMDnCwBacXijPB2szAgNTeBCz",
      "3N6dsnfD88j5yKgpnEavaaJDzAVSRBRVbMY",
      "3MufvXKZxLuNn5SHcEgGc2Vo7nLWnKVskfJ",
      "3Myt4tocZmj7o3d1gnuWRrnQWcoxvx5G7Ac",
      "3N3keodUiS8WLEw9W4BKDNxgNdUpwSnpb3K"
    )
  }

  test("wallet - acc deletion") {

    val head = w.privateKeyAccounts.head
    w.deleteAccount(head)
    assert(w.privateKeyAccounts.lengthCompare(walletSize - 1) == 0)

    w.deleteAccount(w.privateKeyAccounts.head)
    assert(w.privateKeyAccounts.lengthCompare(walletSize - 2) == 0)

    w.privateKeyAccounts.foreach(w.deleteAccount)

    assert(w.privateKeyAccounts.isEmpty)
  }

  test("reopening") {
    val walletFile = Some(createTestTemporaryFile("wallet", ".dat"))

    val w1 = Wallet(WalletSettings(walletFile, "cookies".some, ByteStr.decodeBase58("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").toOption))
    w1.generateNewAccounts(10)
    val w1PrivateKeys = w1.privateKeyAccounts
    val w1nonce       = w1.nonce

    val w2 = Wallet(WalletSettings(walletFile, "cookies".some, None))
    w2.privateKeyAccounts.nonEmpty shouldBe true
    w2.privateKeyAccounts shouldEqual w1PrivateKeys
    w2.nonce shouldBe w1nonce

    val seedError = intercept[IllegalArgumentException](Wallet(WalletSettings(walletFile, "cookies".some, ByteStr.decodeBase58("fake").toOption)))
    seedError.getMessage should include("Seed from config doesn't match the actual seed")
  }

  test("reopen with incorrect password") {
    val file = Some(createTestTemporaryFile("wallet", ".dat"))
    val w1   = Wallet(WalletSettings(file, "password".some, ByteStr.decodeBase58("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").toOption))
    w1.generateNewAccounts(3)

    assertThrows[IllegalArgumentException] {
      Wallet(WalletSettings(file, "incorrect password".some, None))
    }
  }

  def createTestTemporaryFile(name: String, ext: String): File = {
    val file = Files.createTempFile(name, ext).toFile
    file.deleteOnExit()

    file
  }
}
