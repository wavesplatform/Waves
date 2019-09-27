package com.wavesplatform.transaction

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.transaction.assets.exchange.AssetPair
import net.ceedubs.ficus.readers.ValueReader
import play.api.libs.json._

import scala.util.{Failure, Success}

sealed trait Asset
object Asset {
  final case class IssuedAsset(id: ByteStr) extends Asset
  case object Waves                         extends Asset

  implicit val jsonFormat: Format[Asset] = {
    val issuedAssetFormat = Format[IssuedAsset](
      Reads {
        case JsString(str) if str.length > AssetIdStringLength => JsError("invalid.feeAssetId")
        case JsString(str) =>
          Base58.tryDecodeWithLimit(str) match {
            case Success(arr) => JsSuccess(IssuedAsset(ByteStr(arr)))
            case Failure(err) => JsError(s"Expected base58-encoded assetId (${Option(err.getMessage).getOrElse("unknown")})")
          }
        case _ => JsError("Expected base58-encoded assetId")
      },
      Writes(asset => JsString(asset.id.base58))
    )

    Format(
      Reads {
        case assetId: JsString => issuedAssetFormat.reads(assetId)
        case JsNull            => JsSuccess(Waves)
        case _                 => JsError("Expected base58-encoded assetId or null")
      },
      Writes {
        case Waves               => JsNull
        case ia @ IssuedAsset(_) => issuedAssetFormat.writes(ia)
      }
    )
  }

  implicit val valueReader: ValueReader[Asset] = { (cfg, path) =>
    AssetPair.extractAssetId(cfg getString path).fold(ex => throw new Exception(ex.getMessage), identity)
  }

  def fromString(maybeStr: Option[String]): Asset =
    maybeStr.map(x => IssuedAsset(ByteStr.decodeBase58(x).get)).getOrElse(Waves)

  def fromCompatId(maybeBStr: Option[ByteStr]): Asset =
    maybeBStr.fold[Asset](Waves)(IssuedAsset)

  implicit class AssetIdOps(private val ai: Asset) extends AnyVal {
    def bytesRepr: Array[Byte] = ai match {
      case Waves           => Array(0: Byte)
      case IssuedAsset(id) => (1: Byte) +: id.arr
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
