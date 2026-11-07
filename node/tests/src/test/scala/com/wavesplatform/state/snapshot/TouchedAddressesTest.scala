package com.wavesplatform.state.snapshot

import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.state.{Portfolio, StateSnapshot}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.TxHelpers.defaultAddress

class TouchedAddressesTest extends PropSpec with WithDomain {
  property("zero-delta portfolios are recorded as touched without changing snapshot equality") {
    withDomain() { d =>
      val snapshot = StateSnapshot.build(d.blockchain, Map(defaultAddress -> Portfolio.empty)).explicitGet()
      snapshot.touchedAddresses.value shouldBe Set(defaultAddress)
      snapshot shouldBe StateSnapshot.empty
      snapshot.hashCode() shouldBe StateSnapshot.empty.hashCode()
    }
  }

  property("addBalances records zero-delta portfolios as touched") {
    withDomain() { d =>
      val snapshot = StateSnapshot.empty.addBalances(Map(defaultAddress -> Portfolio.empty), d.blockchain).explicitGet()
      snapshot.touchedAddresses.value shouldBe Set(defaultAddress)
      snapshot.balances shouldBe empty
    }
  }
}
