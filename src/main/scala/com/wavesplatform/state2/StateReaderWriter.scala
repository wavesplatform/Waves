package com.wavesplatform.state2

import cats._
import cats.implicits._
import cats.Monoid

trait StateWriter {
  def applyBlockDiff(blockDiff: BlockDiff): Unit
}

class StateWriterImpl(p: JavaMapStorage) extends StateReaderImpl(p) with StateWriter {

  override def applyBlockDiff(blockDiff: BlockDiff): Unit = {
    val txsDiff = blockDiff.txsDiff
    txsDiff.transactions.foreach { case (id, (h, tx)) =>
      p.transactions.put(id.arr, (h, tx.bytes))
    }

    txsDiff.portfolios.foreach { case (account, portfolioDiff) =>
      val updatedPortfolio = accountPortfolio(account).combine(portfolioDiff)
      p.portfolios.put(account.bytes,
        (updatedPortfolio.balance,
          updatedPortfolio.effectiveBalance,
          updatedPortfolio.assets.map { case (k, v) => k.arr -> v }))
    }

    txsDiff.issuedAssets.foreach { case (id, assetInfo) =>
      p.assets.put(id.arr, (assetInfo.isReissuableOverride, assetInfo.totalVolumeOverride))
    }

    p.setHeight(p.getHeight + blockDiff.heightDiff)
  }
}


