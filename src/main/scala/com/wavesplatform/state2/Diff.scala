package com.wavesplatform.state2

import cats._
import cats.implicits._
import scorex.account.Account
import scorex.transaction.assets.{IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.{GenesisTransaction, PaymentTransaction, SignedTransaction, Transaction}

case class BlockDiff(txsDiff: Diff, heightDiff: Int, effectiveBalanceSnapshots: Seq[EffectiveBalanceSnapshot])


case class Diff(transactions: Map[ByteArray, (Int, Transaction)],
                portfolios: Map[Account, Portfolio],
                issuedAssets: Map[ByteArray, AssetInfo])

object Diff {
  def apply(height: Int, tx: Transaction, portfolios: Map[Account, Portfolio],
            assetInfos: Map[ByteArray, AssetInfo]): Diff = Diff(
    transactions = Map(EqByteArray(tx.id) -> (height, tx)),
    portfolios = portfolios,
    issuedAssets = assetInfos
  )

  implicit class DiffExt(d: Diff) {
    def asBlockDiff: BlockDiff = BlockDiff(d, 0, Seq.empty)


    // TODO: Make part of diff so each Differ could calc this
    lazy val accountTransactionIds: Map[ByteArray, List[ByteArray]] = {
      d.transactions.map { case (id, (h, tx)) =>
        val senderBytes = tx match {
          case stx: SignedTransaction => stx.sender.bytes
          case ptx: PaymentTransaction => ptx.sender.bytes
          case gtx: GenesisTransaction => Array.empty[Byte]
          case tx =>
            println(tx)
            ???
        }

        val recipientBytes = tx match {
          case gtx: GenesisTransaction => gtx.recipient.bytes
          case ptx: PaymentTransaction => ptx.recipient.bytes
          case itx: IssueTransaction => Array.empty[Byte]
          case itx: ReissueTransaction => Array.empty[Byte]
          case ttx: TransferTransaction => ttx.recipient.asInstanceOf[Account].bytes
          case tx =>
            println(tx)
            ???
        }
        //      looks like we don't need explicit duplicate entries,
        //      therefore Map(.->.,.->.) instead of `combine`

        //      if (senderBytes sameElements recipientBytes)
        //        Map(EqByteArray(senderBytes) -> List(id, id))
        //      else
        Map(EqByteArray(senderBytes) -> List(id)).combine(Map(EqByteArray(recipientBytes) -> List(id)))
      }.foldLeft(Map.empty[ByteArray, List[ByteArray]]) { case (agg, m) => m.combine(agg) }
    }

    lazy val paymentTransactionIdsByHashes: Map[ByteArray, ByteArray] = d.transactions
      .collect { case (_, (_, ptx: PaymentTransaction)) => EqByteArray(ptx.hash) -> EqByteArray(ptx.id) }
  }

}

case class EffectiveBalanceSnapshot(acc: Account, height: Int, prevEffectiveBalance: Long, effectiveBalance: Long)

case class Portfolio(balance: Long, effectiveBalance: Long, assets: Map[ByteArray, Long])

case class AssetInfo(isReissuable: Boolean, volume: Long)
