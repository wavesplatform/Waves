package com.wavesplatform.state2

import cats._
import cats.implicits._
import scorex.account.Account
import scorex.transaction.assets.{IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.{GenesisTransaction, PaymentTransaction, SignedTransaction, Transaction}

case class BlockDiff(txsDiff: Diff, heightDiff: Int, effectiveBalanceSnapshots: Seq[EffectiveBalanceSnapshot])

object BlockDiff {

  implicit class BlockDiffExt(bd: BlockDiff) {
    lazy val maxPaymentTransactionTimestamp: Map[Account, Long] = bd.txsDiff.transactions.toList
      .collect({ case (_, (_, ptx: PaymentTransaction)) => ptx.sender.toAccount -> ptx.timestamp })
      .groupBy(_._1)
      .map { case (acc, list) => acc -> list.map(_._2).max }
  }

}

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

    lazy val accountTransactionIds: Map[ByteArray, List[ByteArray]] = {
      val accTxIds = d.transactions.map { case (id, (h, tx)) =>
        val senderBytes = tx match {
          case stx: SignedTransaction => Some(stx.sender.bytes)
          case ptx: PaymentTransaction => Some(ptx.sender.bytes)
          case gtx: GenesisTransaction => None
          case _ => ???
        }

        val recipientBytes = tx match {
          case gtx: GenesisTransaction => Some(gtx.recipient.bytes)
          case ptx: PaymentTransaction => Some(ptx.recipient.bytes)
          case itx: IssueTransaction => None
          case itx: ReissueTransaction => None
          case ttx: TransferTransaction => Some(ttx.recipient.asInstanceOf[Account].bytes)
          case _ => ???
        }

        Seq(senderBytes, recipientBytes).flatten.map(bytes => EqByteArray(bytes) -> List(id)).toMap
      }
      Monoid[Map[ByteArray, List[ByteArray]]].combineAll(accTxIds)
    }

    lazy val paymentTransactionIdsByHashes: Map[ByteArray, ByteArray] = d.transactions
      .collect { case (_, (_, ptx: PaymentTransaction)) => EqByteArray(ptx.hash) -> EqByteArray(ptx.id) }
  }

}

case class EffectiveBalanceSnapshot(acc: Account, height: Int, prevEffectiveBalance: Long, effectiveBalance: Long)

case class Portfolio(balance: Long, effectiveBalance: Long, assets: Map[ByteArray, Long])

case class AssetInfo(isReissuable: Boolean, volume: Long)
