package com.wavesplatform.state

import cats.Id
import com.wavesplatform.account.Address
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves

import scala.collection.immutable.VectorMap

/**
  * A set of functions that apply diff
  * to the blockchain and return new
  * state values (only changed ones)
  */
object DiffToStateApplier {
  case class PortfolioUpdates(
      balances: Map[Address, Map[Asset, Long]],
      leases: Map[Address, LeaseBalance]
  )

  def portfolios(blockchain: Blockchain, diff: Diff): PortfolioUpdates = {
    val balances = Map.newBuilder[Address, Map[Asset, Long]]
    val leases   = Map.newBuilder[Address, LeaseBalance]

    for ((address, portfolioDiff) <- diff.portfolios) {
      // balances for address
      val bs = VectorMap.newBuilder[Asset, Long]

      if (portfolioDiff.balance != 0) {
        bs += Waves -> (blockchain.balance(address, Waves) + portfolioDiff.balance)
      }

      portfolioDiff.assets.collect {
        case (asset, balanceDiff) if balanceDiff != 0 =>
          bs += asset -> (blockchain.balance(address, asset) + balanceDiff)
      }

      balances += address -> bs.result()

      // leases
      if (portfolioDiff.lease != LeaseBalance.empty) {
        leases += address -> blockchain.leaseBalance(address).combineF[Id](portfolioDiff.lease)
      }
    }

    PortfolioUpdates(balances.result(), leases.result())
  }
}
