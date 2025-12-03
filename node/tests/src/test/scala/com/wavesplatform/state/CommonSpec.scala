package com.wavesplatform.state

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers

class CommonSpec extends FreeSpec with WithDomain {

  "Common Conditions" - {
    "Zero balance of absent asset" in {
      val sender         = TxHelpers.signer(1)
      val initialBalance = 1000
      val assetId        = Array.fill(32)(1.toByte)

      withDomain(balances = Seq(AddrWithBalance(sender.toAddress, initialBalance))) { d =>
        d.balance(sender.toAddress, IssuedAsset(ByteStr(assetId))) shouldEqual 0L
      }
    }
  }
}
