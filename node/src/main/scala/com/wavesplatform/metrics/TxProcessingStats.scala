package com.wavesplatform.metrics

import com.google.common.base.CaseFormat
import com.wavesplatform.transaction.{Transaction, TransactionType}
import kamon.Kamon
import kamon.metric.Metric

object TxProcessingStats {
  private val typeToName = {
    val converter = CaseFormat.UPPER_CAMEL.converterTo(CaseFormat.LOWER_HYPHEN)
    TransactionType.values.map { t =>
      t -> converter.convert(t.toString)
    }.toMap
  }

  extension (t: Metric.Timer)
    def measureForType[A](tpe: Transaction.Type)(f: => A): A =
      t.withTag("transaction-type", typeToName(tpe)).measure(f)

  val invokedScriptExecution: Metric.Timer    = Kamon.timer("tx.processing.script-execution.invoked")
  val accountScriptExecution: Metric.Timer    = Kamon.timer("tx.processing.script-execution.account")
  val assetScriptExecution: Metric.Timer      = Kamon.timer("tx.processing.script-execution.asset")
  val signatureVerification: Metric.Timer     = Kamon.timer("tx.processing.validation.signature")
  val balanceValidation: Metric.Timer         = Kamon.timer("tx.processing.validation.balance")
  val commonValidation: Metric.Timer          = Kamon.timer("tx.processing.validation.common")
  val transactionDiffValidation: Metric.Timer = Kamon.timer("tx.processing.validation.diff")
  val orderValidation: Metric.Timer           = Kamon.timer("tx.processing.validation.order")
}
