package com.wavesplatform.generator

import cats.Show
import com.wavesplatform.generator.OracleTransactionGenerator.Settings
import com.wavesplatform.generator.utils.Gen
import scorex.account.PrivateKeyAccount
import scorex.transaction.{DataTransaction, Transaction}
import scorex.transaction.smart.SetScriptTransaction
import com.wavesplatform.it.util._
import com.wavesplatform.state._
import scorex.transaction.transfer.TransferTransactionV1

class OracleTransactionGenerator(settings: Settings, val accounts: Seq[PrivateKeyAccount]) extends TransactionGenerator {
  override def next(): Iterator[Transaction] = generate(settings).toIterator

  def generate(settings: Settings): Seq[Transaction] = {
    val oracle = accounts.last

    val scriptedAccount = accounts.head

    val script = Gen.oracleScript(oracle)

    val enoughFee = 0.005.waves

    val setScript =
      SetScriptTransaction
        .selfSigned(1, scriptedAccount, Some(script), enoughFee, System.currentTimeMillis())
        .explicitGet()

    val setFlag = DataTransaction
      .selfSigned(1, oracle, List(BooleanDataEntry("transfers", settings.enabled)), enoughFee, System.currentTimeMillis())
      .explicitGet()

    val transactions =
      List
        .fill(settings.transactions) {
          TransferTransactionV1
            .selfSigned(
              None,
              scriptedAccount,
              oracle,
              1.waves,
              System.currentTimeMillis(),
              None,
              enoughFee,
              Array.emptyByteArray
            )
            .explicitGet()
        }

    setScript +: setFlag +: transactions
  }
}

object OracleTransactionGenerator {
  final case class Settings(transactions: Int, enabled: Boolean)

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      s"Transactions: ${x.transactions}" +
        s"Enabled: ${x.enabled}"
    }
  }
}
