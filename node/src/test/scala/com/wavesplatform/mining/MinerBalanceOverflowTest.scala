package com.wavesplatform.mining

import com.wavesplatform.db.WithDomain
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.matchers.should.Matchers

class MinerBalanceOverflowTest extends FlatSpec with Matchers with WithDomain {
  "Miner balance" should "not overflow" in withDomain(DomainPresets.RideV4WithRewards) { d =>
    d.helpers.creditWavesToDefaultSigner(Long.MaxValue - 6_0000_0000L)
    for (_ <- 1 to 10) intercept[RuntimeException](d.appendBlock()).toString should include("Diff contains negative applied balances")
    val minerBalance = d.blockchain.balance(TxHelpers.defaultAddress)
    minerBalance shouldBe >(0L)
  }
}
