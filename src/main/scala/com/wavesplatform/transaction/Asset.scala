package com.wavesplatform.transaction

import com.google.protobuf.ByteString
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.transaction.assets.exchange.AssetPair
import net.ceedubs.ficus.readers.ValueReader
import play.api.libs.json._

import scala.util.Success

sealed trait Asset
object Asset {
  final case class IssuedAsset(id: ByteStr) extends Asset
  case object Waves                         extends Asset

  implicit val assetReads: Reads[IssuedAsset] = Reads {
    case JsString(str) if str.length > AssetIdStringLength => JsError("invalid.feeAssetId")
    case JsString(str) =>
      Base58.tryDecodeWithLimit(str) match {
        case Success(arr) => JsSuccess(IssuedAsset(ByteStr(arr)))
        case _            => JsError("Expected base58-encoded assetId")
      }
    case _ => JsError("Expected base58-encoded assetId")
  }
  implicit val assetWrites: Writes[IssuedAsset] = Writes { asset =>
    JsString(asset.id.base58)
  }

  implicit val assetIdReads: Reads[Asset] = Reads {
    case json: JsString => assetReads.reads(json)
    case JsNull         => JsSuccess(Waves)
    case _              => JsError("Expected base58-encoded assetId or null")
  }
  implicit val assetIdWrites: Writes[Asset] = Writes {
    case Waves           => JsNull
    case IssuedAsset(id) => JsString(id.base58)
  }

  implicit val assetJsonFormat: Format[IssuedAsset] = Format(assetReads, assetWrites)
  implicit val assetIdJsonFormat: Format[Asset]     = Format(assetIdReads, assetIdWrites)

  implicit val assetReader: ValueReader[Asset] = { (cfg, path) =>
    AssetPair.extractAssetId(cfg getString path).fold(ex => throw new Exception(ex.getMessage), identity)
  }

  def fromCompatId(maybeBStr: Option[ByteStr]): Asset = {
    maybeBStr.map(IssuedAsset).getOrElse(Waves)
  }

  def fromProtoId(byteStr: ByteString): Asset = {
    if (byteStr.isEmpty) Waves
    else IssuedAsset(byteStr.toByteArray)
  }

  implicit class AssetIdOps(val ai: Asset) extends AnyVal {
    def byteRepr: Array[Byte] = ai match {
      case Waves           => Array(0: Byte)
      case IssuedAsset(id) => (1: Byte) +: id.arr
    }

    def protoId: ByteString = ai match {
      case IssuedAsset(id) => ByteString.copyFrom(id)
      case Waves           => ByteString.EMPTY
    }

    def compatId: Option[ByteStr] = ai match {
      case Waves           => None
      case IssuedAsset(id) => Some(id)
    }

    def maybeBase58Repr: Option[String] = ai match {
      case Waves           => None
      case IssuedAsset(id) => Some(id.base58)
    }

    def fold[A](onWaves: => A)(onAsset: IssuedAsset => A): A = ai match {
      case Waves                  => onWaves
      case asset @ IssuedAsset(_) => onAsset(asset)
    }
  }
}
