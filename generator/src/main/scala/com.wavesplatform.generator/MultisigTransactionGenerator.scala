package com.wavesplatform.generator
import cats.Show
import com.wavesplatform.crypto
import com.wavesplatform.generator.DynamicWideTransactionGenerator.Settings
import com.wavesplatform.generator.MultisigTransactionGenerator.Settings
import com.wavesplatform.generator.utils.Gen
import com.wavesplatform.state._
import scorex.account.PrivateKeyAccount
import scorex.transaction.{Proofs, Transaction}
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.smart.script.Script
import scorex.transaction.transfer.{TransferTransactionV1, TransferTransactionV2}

import scala.util.Random

class MultisigTransactionGenerator(settings: Settings, val accounts: Seq[PrivateKeyAccount]) extends TransactionGenerator {

  override def next(): Iterator[Transaction] = {
    Iterator.continually(generate).flatten
  }

  private def generate: Seq[Transaction] = {

    val newAccount = createAccount()

    val bank   = accounts.head
    val owners = Seq(accounts(1), accounts(2), accounts(3))

    val enoughFee               = 5000000
    val totalAmountOnNewAccount = 100000000000L
    val fundNewAccount = TransferTransactionV2
      .selfSigned(2, None, bank, newAccount.toAddress, totalAmountOnNewAccount, System.currentTimeMillis(), None, enoughFee, Array.emptyByteArray)
      .explicitGet()

    val script: Script = Gen.multiSigScript(owners, 3)

    val setScript = SetScriptTransaction.selfSigned(1, newAccount, Some(script), enoughFee, System.currentTimeMillis()).explicitGet()

    val transferBackToBank: TransferTransactionV2 = {
      val tx = TransferTransactionV2
        .create(2,
                None,
                newAccount,
                bank,
                totalAmountOnNewAccount - 2 * enoughFee,
                System.currentTimeMillis(),
                None,
                enoughFee,
                Array.emptyByteArray,
                Proofs.empty)
        .explicitGet()
      val signatures = owners.map(crypto.sign(_, tx.bodyBytes())).map(ByteStr(_))
      tx.copy(proofs = Proofs(signatures))
    }

    Seq(fundNewAccount, setScript, transferBackToBank)
  }

  private def createAccount() = PrivateKeyAccount.fromSeed(Random.alphanumeric.take(32).mkString).explicitGet()
}

object MultisigTransactionGenerator {
  final case class Settings()

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      import x._
      "<empty multisig settings>"
    }
  }
}
