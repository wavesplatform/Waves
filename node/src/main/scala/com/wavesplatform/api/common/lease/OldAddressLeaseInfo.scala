package com.wavesplatform.api.common.lease

import com.wavesplatform.account.Address
import com.wavesplatform.api.common.LeaseInfo.Status.Active
import com.wavesplatform.api.common.TransactionMeta.Ethereum
import com.wavesplatform.api.common.{LeaseInfo, TransactionMeta, addressTransactions}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.RDB
import com.wavesplatform.state.TxMeta.Status.Succeeded
import com.wavesplatform.state.{Blockchain, Height, InvokeScriptResult, StateSnapshot}
import com.wavesplatform.transaction.EthereumTransaction.Invocation
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.{EthereumTransaction, TransactionType as Type}
import monix.reactive.Observable

object OldAddressLeaseInfo {
  def activeLeases(
      rdb: RDB,
      snapshot: => StateSnapshot,
      blockchain: Blockchain,
      address: Address
  ): Observable[LeaseInfo] = {
    addressTransactions(
      rdb,
      Some(Height(blockchain.height) -> snapshot),
      address,
      None,
      Set(Type.Lease, Type.InvokeScript, Type.InvokeExpression, Type.Ethereum),
      None
    ).flatMapIterable {
      case TransactionMeta(leaseHeight, lt: LeaseTransaction, Succeeded) if blockchain.leaseDetails(lt.id()).exists(_.isActive) =>
        Seq(
          LeaseInfo(
            lt.id(),
            lt.id(),
            lt.sender.toAddress,
            blockchain.resolveAlias(lt.recipient).explicitGet(),
            lt.amount.value,
            leaseHeight,
            Active
          )
        )
      case TransactionMeta.Invoke(invokeHeight, originTransaction, Succeeded, _, Some(scriptResult)) =>
        extractLeases(blockchain, address, scriptResult, originTransaction.id(), invokeHeight)
      case Ethereum(height, tx @ EthereumTransaction(_: Invocation, _, _, _), Succeeded, _, _, Some(scriptResult)) =>
        extractLeases(blockchain, address, scriptResult, tx.id(), height)
      case _ =>
        Seq()
    }
  }

  private def extractLeases(blockchain: Blockchain, subject: Address, result: InvokeScriptResult, txId: ByteStr, height: Height): Seq[LeaseInfo] = {
    val current = for {
      lease   <- result.leases
      details <- blockchain.leaseDetails(lease.id) if details.isActive
      sender = details.sender.toAddress
      recipient <- blockchain.resolveAlias(lease.recipient).toOption if subject == sender || subject == recipient
    } yield LeaseInfo(lease.id, txId, sender, recipient, lease.amount, height, Active)
    val nested = result.invokes.flatMap(i => extractLeases(blockchain, subject, i.stateChanges, txId, height))
    current ++ nested
  }
}
