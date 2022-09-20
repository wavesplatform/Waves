package com.wavesplatform.ride

import com.google.protobuf.ByteString
import com.google.protobuf.UnsafeByteOperations.unsafeWrap
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.InvokeScriptResult.DataEntry
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, AssetScriptInfo, Height}
import com.wavesplatform.transaction.Asset
import play.api.libs.json.*

import scala.util.Try

case class RideRunnerInput(
    scriptAddress: Address,
    trace: Boolean,
    request: JsObject,
    accountScript: Map[Address, AccountScriptInfo],
    height: Int,
    activatedFeatures: Map[Short, Int],
    accountData: Map[Address, Map[String, DataEntry]],
    hasData: Map[Address, Boolean],
    resolveAlias: Map[Alias, Address],
    balance: Map[Address, Map[Asset, Long]],
    assetDescription: Map[Asset, AssetDescription]
)
object RideRunnerInput {

  // TODO numericMapFormat
  implicit def shortMapFormat[T: Format]: Format[Map[Short, T]] = mapFormat[Short, T](
    _.toString,
    x => x.toShortOption.fold[JsResult[Short]](JsError(s"Can't parse int: $x"))(JsSuccess(_))
  )

  implicit def intMapFormat[T: Format]: Format[Map[Int, T]] = mapFormat[Int, T](
    _.toString,
    x => x.toIntOption.fold[JsResult[Int]](JsError(s"Can't parse int: $x"))(JsSuccess(_))
  )

  implicit def addressMapFormat[T: Format]: Format[Map[Address, T]] = mapFormat[Address, T](
    _.toString,
    x => Address.fromString(x).fold[JsResult[Address]](e => JsError(s"Can't parse Address '$x': $e"), JsSuccess(_))
  )

  implicit def aliasMapFormat[T: Format]: Format[Map[Alias, T]] = mapFormat[Alias, T](
    _.name,
    x => Alias.fromString(x).fold[JsResult[Alias]](e => JsError(s"Can't parse Alias '$x': $e"), JsSuccess(_))
  )

  implicit def assetMapFormat[T: Format]: Format[Map[Asset, T]] = mapFormat[Asset, T](
    _.toString,
    { str =>
      val compatStr = if (str == "WAVES") None else Some(str)
      Try(Asset.fromString(compatStr)).fold[JsResult[Asset]](e => JsError(s"Can't parse Asset '$str': ${e.getMessage}"), JsSuccess(_))
    }
  )

  implicit val scriptFormat: Format[Script] = implicitly[Format[String]]
    .bimap(
      Script.fromBase64String(_).explicitGet(), // TODO JsError instead
      _.bytes().base64Raw
    )
  implicit val accountScriptInfoFormat: OFormat[AccountScriptInfo] = Json.format

  implicit val aliasFormat: Format[Alias] = implicitly[Format[String]]
    .bimap(
      Alias.fromString(_).explicitGet(), // TODO JsError instead
      _.name
    )

  implicit val byteStringFormat: Format[ByteString] = implicitly[Format[String]]
    .bimap(
      str =>
        unsafeWrap(
          if (str.startsWith("base58:")) Base58.decode(str.substring(7))
          else if (str.startsWith(Base64.Prefix)) Base64.decode(str)
          else Base58.decode(str)
        ),
      x => s"${Base64.Prefix}${Base64.encode(x.toByteArray)}"
    )

  // IDEA and the compiler don't know that it is used
  implicit val byteStrFormat = com.wavesplatform.api.http.requests.byteStrFormat

  implicit val heightFormat: Format[Height] = Height.lift

  implicit val assetScriptInfoFormat: OFormat[AssetScriptInfo] = Json.format

  implicit val assetDescriptionFormat: OFormat[AssetDescription] = Json.format

  implicit val rideRunnerInputFormat: OFormat[RideRunnerInput] = Json.format

  def mapFormat[K, V: Format](stringifyKey: K => String, parseKey: String => JsResult[K])(implicit vFormat: Format[V]): Format[Map[K, V]] = {
    Format(
      fjs = Reads {
        case JsObject(xs) =>
          xs.foldLeft[JsResult[Map[K, V]]](JsSuccess(Map.empty[K, V])) { case (r, (k, v)) =>
            for {
              r <- r
              k <- parseKey(k)
              v <- vFormat.reads(v)
            } yield r.updated(k, v)
          }
        case x => JsError(s"Can't parse map: $x")
      },
      tjs = Writes.map[V](vFormat).contramap(_.map { case (k, v) => stringifyKey(k) -> v })
    )
  }
}
