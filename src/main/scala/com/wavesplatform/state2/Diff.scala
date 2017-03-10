package com.wavesplatform.state2

import cats._
import cats.implicits._
import scorex.account.Account
import scorex.transaction.Transaction

case class Diff(transactions: Map[Array[Byte], (Int, Transaction)],
                portfolios: Map[Account, Portfolio]
               )


case class Portfolio(balance: Long, effectiveBalance: Long, assets: Map[Array[Byte], Long])

object Primitives {

  implicit class OptExt[T](opt: Option[T]) {
    def toM()(implicit m: Monoid[T]): T = m.empty
  }

  implicit val portfolioMonoid = new Monoid[Portfolio] {
    override def empty: Portfolio = Portfolio(0L, 0L, Map.empty)

    override def combine(older: Portfolio, newer: Portfolio): Portfolio
    = Portfolio(older.balance + newer.balance, older.effectiveBalance + newer.effectiveBalance, older.assets.combine(newer.assets))
  }

  implicit val diffMonoid = new Monoid[Diff] {
    override def empty: Diff = Diff(Map.empty, Map.empty)

    override def combine(older: Diff, newer: Diff): Diff = Diff(older.transactions ++ newer.transactions, older.portfolios.combine(newer.portfolios))
  }
}