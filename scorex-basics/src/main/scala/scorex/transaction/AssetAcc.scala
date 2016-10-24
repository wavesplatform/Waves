package scorex.transaction

import scorex.account.Account
import scorex.crypto.encode.Base58

case class AssetAcc(account: Account, assetId: Option[AssetId]) {
  //unique key for account + assetId pair
  lazy val key: String = assetId match {
    case None => account.address
    case Some(id) => account.address + Base58.encode(id)
  }

  override def hashCode(): Int = key.hashCode
  override def equals(obj: scala.Any): Boolean = obj match {
    case a: AssetAcc => key.equals(a.key)
    case _ => false
  }
  override def toString: String = s"AssetAcc[account: '${account.address}', assetId '${assetId.map(Base58.encode)}']"
}

