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
  import java.io.ByteArrayOutputStream

  import com.wavesplatform.lang.utils.Serialize._
  import com.wavesplatform.lang.v1.BaseGlobal
  private val Global: BaseGlobal = com.wavesplatform.lang.Global // Hack for IDEA

  def create(
      compiledScript: Option[ByteStr],
      decimals: Int,
      description: String,
      isReissuable: Boolean,
      name: String,
      quantity: Long,
      nonce: Long,
      parent: ByteStr
  ): Issue = {
    val id = calculateId(decimals, description, isReissuable, name, quantity, nonce, parent)
    Issue(id, compiledScript, decimals, description, isReissuable, name, quantity, nonce)
  }

  def calculateId(
      decimals: Int,
      description: String,
      isReissuable: Boolean,
      name: String,
      quantity: Long,
      nonce: Long,
      parent: ByteStr
  ): ByteStr = {
    val out = new ByteArrayOutputStream()
    out.writeString(name)
    out.writeString(description)
    out.writeInt(decimals)
    out.writeLong(quantity)
    out.writeShort(if (isReissuable) 1 else 0)
    out.writeLong(nonce)
    out.write(parent.arr)
    ByteStr(Global.blake2b256(out.toByteArray))
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

case class SponsorFee(
    assetId: ByteStr,
    minSponsoredAssetFee: Option[Long]
) extends CallableAction

sealed trait DataOp extends CallableAction {
  val key: String
}

sealed trait DataItem[T] extends DataOp {
  val value: T
}

object DataItem {
  case class Lng(k: String, v: Long)     extends DataItem[Long]    { val key = k; val value = v    }
  case class Bool(k: String, v: Boolean) extends DataItem[Boolean] { val key = k; val value = v    }
  case class Bin(k: String, v: ByteStr)  extends DataItem[ByteStr] { val key = k; val value = v    }
  case class Str(k: String, v: String)   extends DataItem[String]  { val key = k; val value = v    }
  case class Delete(key: String)         extends DataOp
}
