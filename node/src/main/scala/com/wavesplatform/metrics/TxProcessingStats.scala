package com.wavesplatform.metrics

import com.google.common.base.CaseFormat
import com.wavesplatform.settings.Constants
import kamon.Kamon
import kamon.metric.Metric
import supertagged._

object TxProcessingStats {
  val typeToName: Map[Byte, String] = {
    def timerName(name: String): String =
      CaseFormat.UPPER_CAMEL
        .converterTo(CaseFormat.LOWER_HYPHEN)
        .convert(name.replace("Transaction", ""))

    Constants.TransactionNames.view.mapValues(timerName).toMap
  }

  object TxTimer extends TaggedType[Metric.Timer]

  type TxTimer = TxTimer.Type

  implicit class TxTimerExt(val t: TxTimer) extends AnyVal {
    def measureForType[A](typeId: Byte)(f: => A): A = {
      val start  = t.withTag("transaction-type", typeToName(typeId)).start()
      val result = f
      start.stop()
      result
    }
  }

  val invokedScriptExecution: TxTimer    = TxTimer(Kamon.timer("tx.processing.script-execution.invoked"))
  val accountScriptExecution: TxTimer    = TxTimer(Kamon.timer("tx.processing.script-execution.account"))
  val assetScriptExecution: TxTimer      = TxTimer(Kamon.timer("tx.processing.script-execution.asset"))
  val signatureVerification: TxTimer     = TxTimer(Kamon.timer("tx.processing.validation.signature"))
  val balanceValidation: TxTimer         = TxTimer(Kamon.timer("tx.processing.validation.balance"))
  val commonValidation: TxTimer          = TxTimer(Kamon.timer("tx.processing.validation.common"))
  val transactionDiffValidation: TxTimer = TxTimer(Kamon.timer("tx.processing.validation.diff"))
  val orderValidation: TxTimer           = TxTimer(Kamon.timer("tx.processing.validation.order"))
}
