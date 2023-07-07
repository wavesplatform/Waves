package com.wavesplatform.state

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.transaction.smart.InvokeTransaction
import com.wavesplatform.transaction.{EthereumTransaction, Transaction}

case class NewTransactionInfo(
    transaction: Transaction,
    snapshot: StateSnapshot,
    affected: Set[Address],
    applied: Boolean,
    spentComplexity: Long
)

object NewTransactionInfo {
  def create(
      tx: Transaction,
      applied: Boolean,
      snapshot: StateSnapshot,
      blockchain: Blockchain
  ): NewTransactionInfo = {
    val calledScripts = snapshot.scriptResults.values.flatMap(inv => InvokeScriptResult.Invocation.calledAddresses(inv.invokes))
    val maybeDApp = tx match {
      case i: InvokeTransaction =>
        i.dApp match {
          case alias: Alias     => snapshot.aliases.get(alias).orElse(blockchain.resolveAlias(alias).toOption)
          case address: Address => Some(address)
        }
      case et: EthereumTransaction =>
        et.payload match {
          case EthereumTransaction.Invocation(dApp, _) => Some(dApp)
          case _                                       => None
        }
      case _ =>
        None
    }
    val affectedAddresses =
      snapshot.balances.keySet.map(_._1) ++
        snapshot.leaseBalances.keySet ++
        snapshot.accountData.keySet ++
        calledScripts ++
        maybeDApp
    NewTransactionInfo(tx, snapshot, affectedAddresses, applied, snapshot.scriptsComplexity)
  }
}
