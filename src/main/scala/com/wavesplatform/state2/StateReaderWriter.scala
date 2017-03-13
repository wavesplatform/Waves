package com.wavesplatform.state2

import cats._
import cats.implicits._
import cats.Monoid

trait StateWriter {
  def applyDiff(d: Diff): Unit
}

class StateWriterImpl(p: JavaMapStorage) extends StateWriter {
  this: StateReader =>

  override def applyDiff(d: Diff): Unit = {
    d.transactions.foreach { case (id, (h, tx)) =>
      p.transactions.put(id.arr, (h, tx.bytes))
    }

    d.portfolios.foreach { case (account, portfolioDiff) =>
      val updatedPortfolio = accountPortfolio(account).combine(portfolioDiff)
      p.portfolios.put(account.bytes,
        (updatedPortfolio.balance,
          updatedPortfolio.effectiveBalance,
          updatedPortfolio.assets.map { case (k, v) => k.arr -> v }))
    }

    d.issuedAssets.foreach { case (id, assetInfo) =>
      p.assets.put(id.arr, (assetInfo.isReissuableOverride, assetInfo.totalVolumeOverride))
    }

    p.setHeight(p.getHeight + d.height)
  }
}


