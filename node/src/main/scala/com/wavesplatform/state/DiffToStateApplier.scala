package com.wavesplatform.state

import cats.syntax.monoid._

import com.wavesplatform.account.Address
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves

/**
  * A set of functions that apply diff
  * to the blockchain and return new
  * state values (only changed ones)
  */
object DiffToStateApplier {
  def balances(blockchain: Blockchain, diff: Diff): Map[(Address, Asset), Long] =
    for {
      (address, portfolioDiff) <- diff.portfolios
      wavesUpdate = {
        if (portfolioDiff.balance != 0)
          Some(Waves -> portfolioDiff.balance)
        else None
      }
      (asset, balanceDiff) <- portfolioDiff.assets ++ wavesUpdate
      newBalance = balanceDiff + blockchain.balance(address, asset)
    } yield (address, asset) -> newBalance

  def leases(blockchain: Blockchain, diff: Diff): Map[Address, LeaseBalance] =
    diff.portfolios
      .withFilter { case (_, portfolio) => portfolio.lease != LeaseBalance.empty }
      .map { case (address, portfolio) => address -> blockchain.leaseBalance(address).combine(portfolio.lease) }
}
