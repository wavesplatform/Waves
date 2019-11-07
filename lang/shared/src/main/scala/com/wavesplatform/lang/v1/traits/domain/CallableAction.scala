package com.wavesplatform.lang.v1.traits.domain

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address

sealed trait CallableAction

case class AssetTransfer(
    recipient: Address,
    amount: Long,
    assetId: Option[ByteStr]
) extends CallableAction

case class Issue(
    compiledScript: Option[ByteStr],
    decimals: Int,
    description: String,
    isReissuable: Boolean,
    name: String,
    quantity: Long
) extends CallableAction

case class Reissue(
    assetId: ByteStr,
    isReissuable: Boolean,
    quantity: Long
) extends CallableAction

case class Burn(
    assetId: ByteStr,
    quantity: Long
) extends CallableAction

sealed trait DataItem[T] extends CallableAction {
  val key: String
  val value: T
}

object DataItem {
  case class Lng(k: String, v: Long)     extends DataItem[Long]    { val key = k; val value = v }
  case class Bool(k: String, v: Boolean) extends DataItem[Boolean] { val key = k; val value = v }
  case class Bin(k: String, v: ByteStr)  extends DataItem[ByteStr] { val key = k; val value = v }
  case class Str(k: String, v: String)   extends DataItem[String]  { val key = k; val value = v }
}
