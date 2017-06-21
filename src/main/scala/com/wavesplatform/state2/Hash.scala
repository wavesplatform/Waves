package com.wavesplatform.state2

import scorex.account.Account

object Hash {
  def accountPortfolios(accountPortfolios: Map[Account, Portfolio]): Int = {
    def accountPortfolioHash(accP: (Account, Portfolio)): Int = {
      val (account, portfolio) = accP
      val h = account.hashCode() + portfolio.balance.hashCode() + portfolio.leaseInfo.hashCode()
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
