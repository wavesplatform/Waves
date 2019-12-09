package com.wavesplatform.lang.v1.traits.domain

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import scorex.crypto.hash.Blake2b256

import monix.eval.Coeval

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
) extends CallableAction {
  import com.wavesplatform.lang.utils.Serialize._
  import java.io.ByteArrayOutputStream

  def id : Coeval[ByteStr] = Coeval.evalOnce {
    val out = new ByteArrayOutputStream()
    out.writeString(name)
    out.writeString(description)
    out.writeInt(decimals)
    out.writeLong(quantity)
    out.writeShort((if(isReissuable) { 1 } else { 0 }))
    out.writeLong(0L) // Nonce
    ByteStr(Blake2b256.hash(out.toByteArray))
  }
}

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
  case class Delete(k: String)           extends DataItem[Null]    { val key = k; val value = null }
}
