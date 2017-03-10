package com.wavesplatform.state2

trait StateWriter {
  def applyDiff(d: Diff): Unit
}

class StateWriterImpl(p: JavaMapStorage) extends StateWriter {

  override def applyDiff(d: Diff): Unit = {
    d.transactions.foreach { case (id, (h, tx)) =>
      p.txs.put(id, (h, tx.bytes))
    }

    d.portfolios.foreach { case (account, portfolio) =>
      p.portfolios.put(account.bytes, (portfolio.balance, portfolio.effectiveBalance, portfolio.assets))
    }
  }
}


