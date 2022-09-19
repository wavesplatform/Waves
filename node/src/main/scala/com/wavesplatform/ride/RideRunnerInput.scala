package com.wavesplatform.ride

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.AccountScriptInfo
import play.api.libs.json.*

case class RideRunnerInput(
    scriptAddress: Address,
    trace: Boolean,
    request: JsObject,
    accountScript: Map[Address, AccountScriptInfo],
    height: Int,
    activatedFeatures: Map[Short, Int]
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

  implicit val scriptFormat: Format[Script] = implicitly[Format[String]]
    .bimap(
      Script.fromBase64String(_).explicitGet(), // TODO JsError instead
      _.bytes().base64Raw
    )
  implicit val accountScriptInfoFormat: OFormat[AccountScriptInfo] = Json.format

  implicit val rideRunnerInputFormat: OFormat[RideRunnerInput] = Json.format

  def mapFormat[K, V: Format](stringifyKey: K => String, parseKey: String => JsResult[K]): Format[Map[K, V]] = {
    val vReads = implicitly[Reads[V]]
    Format(
      fjs = Reads {
        case JsObject(xs) =>
          xs.foldLeft[JsResult[Map[K, V]]](JsSuccess(Map.empty[K, V])) { case (r, (k, v)) =>
            for {
              r <- r
              k <- parseKey(k)
              v <- vReads.reads(v)
            } yield r.updated(k, v)
          }
        case x => JsError(s"Can't parse map: $x")
      },
      tjs = Writes.map[V].contramap(_.map { case (k, v) => stringifyKey(k) -> v })
    )
  }
}
