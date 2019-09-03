package com.wavesplatform.state.extensions

import com.wavesplatform.account.Address
import com.wavesplatform.state.Portfolio

trait AccountAggregations {
  def portfolio(a: Address): Portfolio
  def accountDataKeys(address: Address): Set[String]
}

object AccountAggregations {
  val empty: AccountAggregations = new AccountAggregations {
    override def portfolio(a: Address): Portfolio = Portfolio.empty
    override def accountDataKeys(address: Address): Set[String] = Set.empty
  }
}
