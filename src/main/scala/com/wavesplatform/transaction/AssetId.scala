package com.wavesplatform.transaction

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import play.api.libs.json._

import scala.util.Success

sealed trait AssetId {
  def isWaves: Boolean
  def isAsset: Boolean
}
object AssetId {
  final case class Asset(id: ByteStr) extends AssetId {
    override val isWaves: Boolean = false
    override val isAsset: Boolean = true
  }
  case object Waves extends AssetId {
    override val isWaves: Boolean = true
    override val isAsset: Boolean = false
  }

  implicit val assetReads: Reads[Asset] = Reads {
    case JsString(str) if str.length != AssetIdStringLength => JsError("invalid.feeAssetId")
    case JsString(str) if str.length == AssetIdStringLength =>
      Base58.decode(str) match {
        case Success(arr) => JsSuccess(Asset(ByteStr(arr)))
        case _            => JsError("Expected base58-encoded assetId")
      }
    case _ => JsError("Expected base58-encoded assetId")
  }
  implicit val assetWrites: Writes[Asset] = Writes { asset =>
    JsString(asset.id.base58)
  }

  implicit val assetIdReads: Reads[AssetId] = Reads {
    case json: JsString => assetReads.reads(json)
    case JsNull         => JsSuccess(Waves)
    case _              => JsError("Expected base58-encoded assetId or null")
  }
  implicit val assetIdWrites: Writes[AssetId] = Writes { assetId =>
    assetId.maybeBase58Repr
      .map(JsString)
      .getOrElse(JsNull)
  }

  implicit val assetJsonFormat: Format[Asset]     = Format(assetReads, assetWrites)
  implicit val assetIdJsonFormat: Format[AssetId] = Format(assetIdReads, assetIdWrites)

  def fromCompatId(maybeBStr: Option[ByteStr]): AssetId = {
    maybeBStr.map(Asset).getOrElse(Waves)
  }

  implicit class AssetIdOps(val ai: AssetId) extends AnyVal {
    def byteRepr: Array[Byte] = ai match {
      case Waves     => Array(0: Byte)
      case Asset(id) => (1: Byte) +: id.arr
    }

    def compatId: Option[ByteStr] = ai match {
      case Waves     => None
      case Asset(id) => Some(id)
    }

    def maybeBase58Repr: Option[String] = ai match {
      case Waves     => None
      case Asset(id) => Some(id.base58)
    }

    def fold[A](onWaves: => A)(onAsset: Asset => A): A = ai match {
      case Waves            => onWaves
      case asset @ Asset(_) => onAsset(asset)
    }
  }
}
