package com.wavesplatform.lang.v1.traits.domain

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import scorex.crypto.hash.Blake2b256

sealed trait CallableAction

case class AssetTransfer(
    recipient: Address,
    amount: Long,
    assetId: Option[ByteStr]
) extends CallableAction

case class Issue(
    id: ByteStr,
    compiledScript: Option[ByteStr],
    decimals: Int,
    description: String,
    isReissuable: Boolean,
    name: String,
    quantity: Long,
    nonce: Long
) extends CallableAction

object Issue {
  import com.wavesplatform.lang.utils.Serialize._
  import java.io.ByteArrayOutputStream

  def create( compiledScript: Option[ByteStr],
              decimals: Int,
              description: String,
              isReissuable: Boolean,
              name: String,
              quantity: Long,
              nonce: Long,
              patent: ByteStr) = {
    val out = new ByteArrayOutputStream()
    out.writeString(name)
    out.writeString(description)
    out.writeInt(decimals)
    out.writeLong(quantity)
    out.writeShort((if(isReissuable) { 1 } else { 0 }))
    out.writeLong(0l) // Nonce
    Issue(ByteStr(Blake2b256.hash(out.toByteArray)),
          compiledScript,
          decimals: Int,
          description: String,
          isReissuable: Boolean,
          name: String,
          quantity: Long,
          nonce: Long)
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
