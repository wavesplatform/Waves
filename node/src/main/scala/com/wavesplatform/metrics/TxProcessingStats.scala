package com.wavesplatform.metrics

import com.google.common.base.CaseFormat
import com.wavesplatform.transaction.{Transaction, TransactionType}
import kamon.Kamon
import kamon.metric.Metric
import supertagged.*

object TxProcessingStats {
  private val typeToName = {
    val converter = CaseFormat.UPPER_CAMEL.converterTo(CaseFormat.LOWER_HYPHEN)
    TransactionType.values.map { t =>
      t -> converter.convert(t.toString)
    }.toMap
  }

  object TxTimer extends TaggedType[Metric.Timer]

  type TxTimer = TxTimer.Type

  implicit class TxTimerExt(val t: TxTimer) extends AnyVal {
    def measureForType[A](tpe: Transaction.Type)(f: => A): A =
      t.withTag("transaction-type", typeToName(tpe)).measure(f)
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
