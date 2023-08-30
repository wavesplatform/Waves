package com.wavesplatform.state

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease.*
import com.wavesplatform.transaction.smart.*
import com.wavesplatform.transaction.transfer.*

case class NewTransactionInfo(
    transaction: Transaction,
    snapshot: StateSnapshot,
    affected: Set[Address],
    status: Status,
    spentComplexity: Long
)

object NewTransactionInfo {
  def create(
      tx: Transaction,
      status: Status,
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
      if (status == Status.Elided)
        elidedAffectedAddresses(tx, blockchain) ++ maybeDApp
      else
        snapshot.balances.keySet.map(_._1) ++
          snapshot.zeroBalanceAffected ++
          snapshot.leaseBalances.keySet ++
          snapshot.accountData.keySet ++
          calledScripts ++
          maybeDApp
    NewTransactionInfo(tx, snapshot, affectedAddresses, status, snapshot.scriptsComplexity)
  }

  private def elidedAffectedAddresses(tx: Transaction, blockchain: Blockchain): Set[Address] =
    tx match {
      case t: BurnTransaction        => Set(t.sender.toAddress)
      case t: CreateAliasTransaction => Set(t.sender.toAddress)
      case t: DataTransaction        => Set(t.sender.toAddress)
      case t: EthereumTransaction =>
        Set(t.sender.toAddress) ++
          (t.payload match {
            case EthereumTransaction.Transfer(_, _, recipient) => Set(recipient)
            case _                                             => Set.empty
          })
      case t: ExchangeTransaction     => Set(t.sender.toAddress, t.order1.sender.toAddress, t.order2.sender.toAddress)
      case t: InvokeScriptTransaction => Set(t.sender.toAddress)
      case t: IssueTransaction        => Set(t.sender.toAddress)
      case t: LeaseCancelTransaction =>
        Set(t.sender.toAddress) ++ blockchain
          .leaseDetails(t.leaseId)
          .flatMap(_.recipient match {
            case alias: Alias     => blockchain.resolveAlias(alias).toOption
            case address: Address => Some(address)
          })
          .toSet
      case t: LeaseTransaction =>
        Set(t.sender.toAddress) ++ (t.recipient match {
          case alias: Alias     => blockchain.resolveAlias(alias).toOption.toSet
          case address: Address => Set(address)
        })
      case t: MassTransferTransaction =>
        Set(t.sender.toAddress) ++ (t.transfers.flatMap {
          _.address match {
            case alias: Alias     => blockchain.resolveAlias(alias).toOption.toSet
            case address: Address => Set(address)
          }
        })
      case t: ReissueTransaction         => Set(t.sender.toAddress)
      case t: SetAssetScriptTransaction  => Set(t.sender.toAddress)
      case t: SetScriptTransaction       => Set(t.sender.toAddress)
      case t: SponsorFeeTransaction      => Set(t.sender.toAddress)
      case t: UpdateAssetInfoTransaction => Set(t.sender.toAddress)
      case t: TransferTransaction =>
        Set(t.sender.toAddress) ++ (t.recipient match {
          case alias: Alias     => blockchain.resolveAlias(alias).toOption.toSet
          case address: Address => Set(address)
        })
      case _ => Set.empty
    }
}
