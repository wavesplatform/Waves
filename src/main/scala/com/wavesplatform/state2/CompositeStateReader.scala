package com.wavesplatform.state2

import cats._
import cats.implicits._
import scorex.account.Account
import scorex.transaction.Transaction
import Primitives._

class CompositeStateReader(s: StateReader, d: Diff) extends StateReader {
  override def getTxInfo(id: Array[Byte]): Option[(Int, Transaction)] = d.transactions.get(id).orElse(s.getTxInfo(id))

  override def accountPortfolio(a: Account): Portfolio = {
    s.accountPortfolio(a).combine(d.portfolios.get(a).toM)
  }
}
