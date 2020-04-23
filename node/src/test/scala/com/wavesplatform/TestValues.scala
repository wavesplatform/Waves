package com.wavesplatform

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset.IssuedAsset

object TestValues {
  val keyPair: KeyPair   = KeyPair.fromSeed("matcher").right.get
  val address: Address   = keyPair.toAddress
  val asset: IssuedAsset = IssuedAsset(ByteStr(("A" * 32).getBytes("ASCII")))
  val fee                = 1000000
  val bigMoney = com.wavesplatform.state.diffs.ENOUGH_AMT
  val timestamp = System.currentTimeMillis()
}
