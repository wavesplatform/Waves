package com.wavesplatform.ride

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.AccountScriptInfo
import com.wavesplatform.state.InvokeScriptResult.DataEntry
import play.api.libs.json.*

case class RideRunnerInput(
    scriptAddress: Address,
    trace: Boolean,
    request: JsObject,
    accountScript: Map[Address, AccountScriptInfo],
    height: Int,
    activatedFeatures: Map[Short, Int],
    accountData: Map[Address, Map[String, DataEntry]],
    hasData: Map[Address, Boolean],
    resolveAlias: Map[Alias, Address]
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
