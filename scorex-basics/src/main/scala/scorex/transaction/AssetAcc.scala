package scorex.transaction

import scorex.account.Account
import scorex.crypto.encode.Base58

case class AssetAcc(account: Account, assetId: Option[AssetId]) {
  //unique key for account + assetId pair
  lazy val key: String = assetId match {
    case None => account.address
    case Some(id) => account.address + Base58.encode(id)
  }
}

