package com.wavesplatform.ride.runner.storage

import cats.syntax.contravariantSemigroupal.*
import com.wavesplatform.account.Address
import com.wavesplatform.ride.runner.stats.RideRunnerStats.rideRequestTotalNumber
import com.wavesplatform.ride.runner.storage.persistent.{CacheKeys, PersistentStorage}
import com.wavesplatform.ride.runner.storage.persistent.CacheKeys.{Requests, RequestsLastIndex}
import play.api.libs.json.*

import java.util.concurrent.atomic.AtomicInteger

trait RequestsStorage {
  def size: Int
  def all(): List[ScriptRequest]

  /** Doesn't check presence
    */
  def append(x: ScriptRequest): Unit
}

final case class ScriptRequest(address: Address, requestBody: JsObject)
object ScriptRequest {
  implicit val requestsKeyReads: Reads[ScriptRequest] = Reads {
    case JsArray(rawAddress +: rawRequestBody +: xs) if xs.isEmpty =>
      val address = rawAddress match {
        case JsString(rawAddress) => Address.fromString(rawAddress).left.map(e => s"Expected '$rawAddress' to be an address: $e")
        case x                    => Left(s"Expected a string, got: $x")
      }

      val requestBody = rawRequestBody match {
        case r: JsObject => Right(r)
        case x           => Left(s"Expected a JsObject, got: $x")
      }

      (address, requestBody).mapN(ScriptRequest.apply) match {
        case Left(e)  => JsError(s"Can't parse RequestKey: $e")
        case Right(r) => JsSuccess(r)
      }

    case x => JsError(s"Expected an array with two elements, got: $x")
  }
}

class DefaultRequestsStorage(storage: PersistentStorage) extends RequestsStorage {
  private val lastIndexKey = RequestsLastIndex.mkKey(())
  private val lastIndex    = new AtomicInteger(storage.readOnly(_.db.getOpt(lastIndexKey).getOrElse(-1)))
  refreshCounter()

  override def size: Int = lastIndex.get() + 1

  override def all(): List[ScriptRequest] = storage.readOnly { ro =>
    var r = List.empty[ScriptRequest]
    ro.db.iterateOver(Requests.prefixBytes) { entry =>
      r = CacheKeys.Requests.parseValue(entry.getValue) :: r
    }
    r // .reverse the order doesn't matter
  }

  override def append(x: ScriptRequest): Unit = storage.readWrite { rw =>
    val newIndex = lastIndex.incrementAndGet()
    val key      = CacheKeys.Requests.mkKey(newIndex)
    rw.db.put(lastIndexKey, newIndex)
    rw.db.put(key, x)
    refreshCounter()
  }

  private def refreshCounter(): Unit = rideRequestTotalNumber.update(size)
}
