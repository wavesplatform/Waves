package com.wavesplatform.mining

import com.wavesplatform.db.WithDomain
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.matchers.should.Matchers

class MinerBalanceOverflowTest extends FlatSpec with Matchers with WithDomain {
  "Miner balance" should "not overflow" in withDomain(DomainPresets.RideV4WithRewards) { d =>
    d.helpers.creditWavesToDefaultSigner(Long.MaxValue - 6.waves)
    d.appendBlockE() should produce("Waves balance sum overflow")
    val minerBalance = d.blockchain.balance(TxHelpers.defaultAddress)
    minerBalance shouldBe >(0L)
  }
}
