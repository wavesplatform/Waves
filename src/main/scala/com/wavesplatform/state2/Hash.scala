package com.wavesplatform.state2

import scorex.account.Address

object Hash {
  def accountPortfolios(accountPortfolios: Map[Address, Portfolio]): Int = {
    def accountPortfolioHash(accP: (Address, Portfolio)): Int = {
      val (account, portfolio) = accP
      val h = if (portfolio.balance == 0 && portfolio.leaseInfo == LeaseInfo.empty) 0 else account.hashCode() + portfolio.balance.hashCode() + portfolio.leaseInfo.hashCode()
      portfolio.assets.foldLeft(h) {
        case (h, (acc, balance)) =>
          h + acc.hashCode() + balance.hashCode()
      }
    }

    accountPortfolios.foldLeft(0) { case (hash, (account, portfolio)) =>
      hash + accountPortfolioHash((account, portfolio))
    }
  }
}
