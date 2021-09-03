package com.wavesplatform.api.common

import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.state.{Height, InvokeScriptResult}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.{EthereumTransaction, Transaction}

sealed trait TransactionMeta {
  def height: Height
  def transaction: Transaction
  def succeeded: Boolean
}

object TransactionMeta {

  def unapply(tm: TransactionMeta): Option[(Height, Transaction, Boolean)] =
    Some((tm.height, tm.transaction, tm.succeeded))

  def create(
      height: Height,
      transaction: Transaction,
      succeeded: Boolean,
      loadStateChanges: Transaction => Option[InvokeScriptResult],
      loadEthereumMetadata: EthereumTransaction => Option[EthereumTransactionMeta]
  ): TransactionMeta =
    transaction match {
      case ist: InvokeScriptTransaction =>
        Invoke(height, ist, succeeded, loadStateChanges(ist))

      case et: EthereumTransaction =>
        Ethereum(height, et, succeeded, loadEthereumMetadata(et), loadStateChanges(et))

      case _ =>
        Default(height, transaction, succeeded)
    }

  final case class Default(height: Height, transaction: Transaction, succeeded: Boolean) extends TransactionMeta

  final case class Invoke(height: Height, transaction: InvokeScriptTransaction, succeeded: Boolean, invokeScriptResult: Option[InvokeScriptResult])
      extends TransactionMeta

  final case class Ethereum(
      height: Height,
      transaction: EthereumTransaction,
      succeeded: Boolean,
      meta: Option[EthereumTransactionMeta],
      invokeScriptResult: Option[InvokeScriptResult]
  ) extends TransactionMeta

}
