package com.wavesplatform.generator

import cats.Show
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.generator.OracleTransactionGenerator.Settings
import com.wavesplatform.generator.config.ConfigReaders
import com.wavesplatform.generator.utils.Gen
import com.wavesplatform.generator.utils.Implicits.DoubleExt
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.state.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.{Transaction, TxHelpers}
import pureconfig.ConfigReader

class OracleTransactionGenerator(settings: Settings, val accounts: Seq[KeyPair], estimator: ScriptEstimator) extends TransactionGenerator {
  override def next(): Iterator[Transaction] = generate(settings).iterator

  def generate(settings: Settings): Seq[Transaction] = {
    val oracle = accounts.last

    val scriptedAccount = accounts.head

    val script = Gen.oracleScript(oracle, settings.requiredData, estimator)

    val enoughFee = 0.005.waves

    val setScript: Transaction =
      TxHelpers.setScript(scriptedAccount, script, enoughFee)

    val setDataTx: Transaction = TxHelpers.data(oracle, settings.requiredData.toSeq, enoughFee)

    val now = System.currentTimeMillis()
    val transactions: List[Transaction] = (1 to settings.transactions).map { i =>
      TxHelpers.transfer(scriptedAccount, oracle.toAddress, 1.waves, Waves, enoughFee, Waves, ByteStr.empty, now + i, 2.toByte)
    }.toList

    setScript +: setDataTx +: transactions
  }
}

object OracleTransactionGenerator extends ConfigReaders {
  final case class Settings(transactions: Int, requiredData: Set[DataEntry[?]]) derives ConfigReader

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      s"Transactions: ${x.transactions}\n" +
        s"DataEntries: ${x.requiredData}\n"
    }
  }
}
