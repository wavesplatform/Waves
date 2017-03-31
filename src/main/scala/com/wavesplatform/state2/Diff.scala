package com.wavesplatform.state2

import cats._
import cats.implicits._
import scorex.account.{Account, Alias}
import scorex.transaction.assets.{IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.{GenesisTransaction, PaymentTransaction, SignedTransaction, Transaction}

case class BlockDiff(txsDiff: Diff, heightDiff: Int, effectiveBalanceSnapshots: Seq[EffectiveBalanceSnapshot])

object BlockDiff {

  implicit class BlockDiffExt(bd: BlockDiff) {
    lazy val maxPaymentTransactionTimestamp: Map[Account, Long] = bd.txsDiff.transactions.toList
      .collect({ case (_, (_, ptx: PaymentTransaction, _)) => ptx.sender.toAccount -> ptx.timestamp })
      .groupBy(_._1)
      .map { case (acc, list) => acc -> list.map(_._2).max }
  }

}

case class Diff(transactions: Map[ByteArray, (Int, Transaction, Set[Account])],
                portfolios: Map[Account, Portfolio],
                issuedAssets: Map[ByteArray, AssetInfo],
                aliases: Map[Alias, Account])

object Diff {
  def apply(height: Int, tx: Transaction,
            portfolios: Map[Account, Portfolio],
            assetInfos: Map[ByteArray, AssetInfo] = Map.empty,
            aliases: Map[Alias, Account] = Map.empty
           ): Diff = Diff(
    transactions = Map(EqByteArray(tx.id) -> (height, tx, portfolios.keys.toSet)),
    portfolios = portfolios,
    issuedAssets = assetInfos,
    aliases = aliases)

  implicit class DiffExt(d: Diff) {
    def asBlockDiff: BlockDiff = BlockDiff(d, 0, Seq.empty)

    lazy val accountTransactionIds: Map[Account, List[ByteArray]] = {
      val map: List[(Account, List[ByteArray])] = d.transactions.toList.flatMap { case (id, (h, tx, accs)) => accs.map(_ -> List(id)) }
      map.foldLeft(Map.empty[Account, List[ByteArray]]) { case (m, (acc, list)) => m.combine(Map(acc -> list)) }
    }

    lazy val paymentTransactionIdsByHashes: Map[ByteArray, ByteArray] = d.transactions
      .collect { case (_, (_, ptx: PaymentTransaction, _)) => EqByteArray(ptx.hash) -> EqByteArray(ptx.id) }
  }

}

case class EffectiveBalanceSnapshot(acc: Account, height: Int, prevEffectiveBalance: Long, effectiveBalance: Long)

case class Portfolio(balance: Long, effectiveBalance: Long, assets: Map[ByteArray, Long])

case class AssetInfo(isReissuable: Boolean, volume: Long)
