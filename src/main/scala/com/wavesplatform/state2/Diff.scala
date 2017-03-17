package com.wavesplatform.state2

import cats._
import cats.implicits._
import scorex.account.Account
import scorex.transaction.Transaction

case class BlockDiff(txsDiff: Diff, heightDiff: Int, effectiveBalanceSnapshots: Seq[EffectiveBalanceSnapshot])


case class Diff(transactions: Map[ByteArray, (Int, Transaction)],
                portfolios: Map[Account, Portfolio],
                issuedAssets: Map[ByteArray, AssetInfo])

object Diff {
  def apply(height: Int, tx: Transaction, portfolios: Map[Account, Portfolio],
            issuedAssets: Map[ByteArray, AssetInfo]): Diff = Diff(
    transactions = Map(EqByteArray(tx.id) -> (height, tx)),
    portfolios = portfolios,
    issuedAssets = issuedAssets
  )

  implicit class DiffExt(d: Diff) {
    def asBlockDiff: BlockDiff = BlockDiff(d, 0, Seq.empty)
  }

}

case class EffectiveBalanceSnapshot(acc: Account, height: Int, prevEffectiveBalance: Long, effectiveBalance: Long)

case class Portfolio(balance: Long, effectiveBalance: Long, assets: Map[ByteArray, Long])

case class AssetInfo(isReissuable: Boolean, totalVolume: Long)
