package com.wavesplatform

import com.wavesplatform.settings.{GenesisSettings, GenesisTransactionSettings}
import scorex.account.Address

import scala.concurrent.duration._

object TestHelpers {
  def genesisSettings(balances: Map[Address, Long], blockTimestamp: Long = System.currentTimeMillis()): GenesisSettings = {
    val totalAmount = balances.values.sum
    val transactions = balances.map { case (account, amount) =>
      GenesisTransactionSettings(account.address, amount)
    }.toSeq

    GenesisSettings(blockTimestamp, blockTimestamp, totalAmount, None, transactions, 1000, 60.seconds)
  }
}
