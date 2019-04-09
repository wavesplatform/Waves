package com.wavesplatform.transaction

import com.wavesplatform.account.Address

case class AssetAcc(account: Address, assetId: Option[Asset])
