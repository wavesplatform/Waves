package com.wavesplatform

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.transaction.Asset.IssuedAsset

object TestValues {
  val keyPair: KeyPair = KeyPair.fromSeed("matcher").right.get
  val address: Address = keyPair.toAddress
  val asset: IssuedAsset = IssuedAsset(("A" * 32).getBytes("ASCII"))
}
