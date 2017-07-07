package com.wavesplatform

import com.wavesplatform.settings.{GenesisSettings, GenesisTransactionSettings}
import scorex.account.Account

import scala.concurrent.duration._

object TestHelpers {
  def genesisSettings(balances: Map[Account, Long], blockTimestamp: Long = System.currentTimeMillis()) = {
    val totalAmount = balances.values.sum
    val transactions = balances.map { case (account, amount) =>
      GenesisTransactionSettings(account.address, amount)
    }.toSeq

    GenesisSettings(blockTimestamp, blockTimestamp, totalAmount, None, transactions, 1000, 60.seconds)
  }
}
