package com.wavesplatform.state2

import cats._
import cats.implicits._
import scorex.account.Account
import scorex.transaction.Transaction

case class BlockDiff(txsDiff: Diff, heightDiff: Int)

object BlockDiff {
  def apply(txsDiff: Diff): BlockDiff = BlockDiff(txsDiff, 0)
}


case class Diff(transactions: Map[ByteArray, (Int, Transaction)],
                portfolios: Map[Account, Portfolio],
                issuedAssets: Map[ByteArray, AssetInfo])

object Diff {
  def apply(height: Int, tx: Transaction, portfolios: Map[Account, Portfolio],
            issuedAssets: Map[ByteArray, AssetInfo]): Diff = Diff(
    transactions = Map(EqByteArray(tx.id) -> (height, tx)),
    portfolios = portfolios, issuedAssets = issuedAssets)

}

case class Portfolio(balance: Long, effectiveBalance: Long, assets: Map[ByteArray, Long])

case class AssetInfo(isReissuableOverride: Boolean, totalVolumeOverride: Long)
