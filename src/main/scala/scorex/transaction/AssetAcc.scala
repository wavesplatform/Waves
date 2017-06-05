package scorex.transaction

import scorex.account.Account

case class AssetAcc(account: Account, assetId: Option[AssetId]) {
  lazy val key: String = assetId match {
    case None => account.address
    case Some(id) => account.address + id.base58
  }
}

