package com.wavesplatform.api.common

import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.state.{Height, InvokeScriptResult}
import com.wavesplatform.transaction.{EthereumTransaction, Transaction}
import com.wavesplatform.transaction.smart.InvokeTransaction

sealed trait TransactionMeta {
  def height: Height
  def transaction: Transaction
  def succeeded: Boolean
  def spentComplexity: Long
}

object TransactionMeta {

  def unapply(tm: TransactionMeta): Option[(Height, Transaction, Boolean)] =
    Some((tm.height, tm.transaction, tm.succeeded))

  def create(
      height: Height,
      transaction: Transaction,
      succeeded: Boolean, spentComplexity: Long,
      loadStateChanges: Transaction => Option[InvokeScriptResult],
      loadEthereumMetadata: EthereumTransaction => Option[EthereumTransactionMeta]
  ): TransactionMeta =
    transaction match {
      case ist: InvokeTransaction =>
        Invoke(height, ist, succeeded, spentComplexity, loadStateChanges(ist))

      case et: EthereumTransaction =>
        Ethereum(height, et, succeeded, spentComplexity, loadEthereumMetadata(et), loadStateChanges(et))

      case _ =>
        Default(height, transaction, succeeded, spentComplexity)
    }

  final case class Default(height: Height, transaction: Transaction, succeeded: Boolean, spentComplexity: Long) extends TransactionMeta

  final case class Invoke(height: Height, transaction: InvokeTransaction, succeeded: Boolean, spentComplexity: Long, invokeScriptResult: Option[InvokeScriptResult])
      extends TransactionMeta

  final case class Ethereum(
      height: Height,
      transaction: EthereumTransaction,
      succeeded: Boolean, spentComplexity: Long,
      meta: Option[EthereumTransactionMeta],
      invokeScriptResult: Option[InvokeScriptResult]
  ) extends TransactionMeta
}
