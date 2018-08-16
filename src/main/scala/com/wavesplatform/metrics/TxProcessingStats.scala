package com.wavesplatform.metrics

import com.wavesplatform.transaction.TransactionParsers
import kamon.Kamon
import kamon.metric.instrument.{Counter, Histogram}

final case class TxProcessingStats(component: String) {

  final case class TxStatsHolder(txTypeName: String) {
    private def mkCounter(name: String) =
      Kamon.metrics.counter(name, Map("transaction-type" -> txTypeName, "component" -> component))

    private def mkHistogram(name: String) =
      Kamon.metrics.histogram(name, Map("transaction-type" -> txTypeName, "component" -> component))

    private[TxProcessingStats] val scriptExecutionTimer         = mkHistogram("tx-script-execution")
    private[TxProcessingStats] val scriptExecutionCounter       = mkCounter("tx-scripts-executed")
    private[TxProcessingStats] val assetScriptExecutionTimer    = mkHistogram("tx-asset-script-execution")
    private[TxProcessingStats] val assetScriptsExecuted         = mkCounter("tx-asset-scripts-executed")
    private[TxProcessingStats] val signatureVerificationTimer   = mkHistogram("tx-signature-validation")
    private[TxProcessingStats] val signatureVerificationCounter = mkCounter("tx-signatures-validated")
    private[TxProcessingStats] val balanceValidationTimer       = mkHistogram("tx-balance-validation")
    private[TxProcessingStats] val balanceValidationCounter     = mkCounter("tx-balances-validated")
    private[TxProcessingStats] val transactionDiffTime          = mkHistogram("tx-diff-time")
    private[TxProcessingStats] val transactionDiffCounter       = mkCounter("tx-diff-counter")
    private[TxProcessingStats] val utxProcessingTimer           = mkHistogram("utx-transaction-processing-time")
  }

  private val txIdentifiers = TransactionParsers.all.values.toList.map { builder =>
    val txTypeName = builder.typeId match {
      case 1  => "Genesis"
      case 2  => "Payment"
      case 3  => "Issue"
      case 4  => "Transfer"
      case 5  => "Reissue"
      case 6  => "Burn"
      case 7  => "Exchange"
      case 8  => "Lease"
      case 9  => "LeaseCancel"
      case 10 => "Alias"
      case 11 => "MassTransfer"
      case 12 => "Data"
      case 13 => "SetScript"
      case 14 => "SetSponsorship"
    }

    (builder.typeId, TxStatsHolder(txTypeName))
  }.toMap

  def scriptExecutionTime(typeId: Byte): Histogram       = txIdentifiers(typeId).scriptExecutionTimer
  def scriptsExecuted(typeId: Byte): Counter             = txIdentifiers(typeId).scriptExecutionCounter
  def signatureVerificationTime(typeId: Byte): Histogram = txIdentifiers(typeId).signatureVerificationTimer
  def signaturesVerified(typeId: Byte): Counter          = txIdentifiers(typeId).signatureVerificationCounter
  def balanceValidationTime(typeId: Byte): Histogram     = txIdentifiers(typeId).balanceValidationTimer
  def balancesValidated(typeId: Byte): Counter           = txIdentifiers(typeId).balanceValidationCounter
  def assetScriptExecutionTimer(typeId: Byte): Histogram = txIdentifiers(typeId).assetScriptExecutionTimer
  def assetScriptsExecuted(typeId: Byte): Counter        = txIdentifiers(typeId).assetScriptsExecuted
  def transactionDiffTime(typeId: Byte): Histogram       = txIdentifiers(typeId).transactionDiffTime
  def transactionDiffCounter(typeId: Byte): Counter      = txIdentifiers(typeId).transactionDiffCounter
  def utxProcessingTimer(typeId: Byte): Histogram        = txIdentifiers(typeId).utxProcessingTimer
}
