package com.wavesplatform.api.common

import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.state.{Height, InvokeScriptResult, TxMeta}
import com.wavesplatform.transaction.{EthereumTransaction, Transaction}
import com.wavesplatform.transaction.smart.InvokeTransaction

sealed trait TransactionMeta {
  def height: Height
  def transaction: Transaction
  def status: TxMeta.Status
  def spentComplexity: Long
}

object TransactionMeta {

  def unapply(tm: TransactionMeta): Option[(Height, Transaction, TxMeta.Status)] =
    Some((tm.height, tm.transaction, tm.status))

  def create(
      height: Height,
      transaction: Transaction,
      status: TxMeta.Status,
      spentComplexity: Long,
      loadStateChanges: Transaction => Option[InvokeScriptResult],
      loadEthereumMetadata: EthereumTransaction => Option[EthereumTransactionMeta]
  ): TransactionMeta =
    transaction match {
      case ist: InvokeTransaction =>
        Invoke(height, ist, status, spentComplexity, loadStateChanges(ist))

      case et: EthereumTransaction =>
        Ethereum(height, et, status, spentComplexity, loadEthereumMetadata(et), loadStateChanges(et))

      case _ =>
        Default(height, transaction, status, spentComplexity)
    }

  sealed trait HasStateChanges { self: TransactionMeta =>
    def invokeScriptResult: Option[InvokeScriptResult]
  }

  final case class Default(height: Height, transaction: Transaction, status: TxMeta.Status, spentComplexity: Long) extends TransactionMeta

  final case class Invoke(
      height: Height,
      transaction: InvokeTransaction,
      status: TxMeta.Status,
      spentComplexity: Long,
      invokeScriptResult: Option[InvokeScriptResult]
  ) extends TransactionMeta
      with HasStateChanges

  final case class Ethereum(
      height: Height,
      transaction: EthereumTransaction,
      status: TxMeta.Status,
      spentComplexity: Long,
      meta: Option[EthereumTransactionMeta],
      invokeScriptResult: Option[InvokeScriptResult]
  ) extends TransactionMeta
      with HasStateChanges
}
