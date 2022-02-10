package com.wavesplatform.state

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.test.FreeSpec

class CommonSpec extends FreeSpec with WithDomain {
  private val AssetIdLength = 32

  "Common Conditions" - {
    "Zero balance of absent asset" in forAll(accountGen, positiveLongGen, byteArrayGen(AssetIdLength)) {
      case (sender, initialBalance, assetId) =>
        withDomain(balances = Seq(AddrWithBalance(sender.toAddress, initialBalance))) { d =>
          d.balance(sender.toAddress, IssuedAsset(ByteStr(assetId))) shouldEqual 0L
        }
    }
  }
}
