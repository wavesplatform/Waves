package scorex.transaction

import scorex.account.Address

case class AssetAcc(account: Address, assetId: Option[AssetId])
