package com.wavesplatform.riderunner.storage

import cats.syntax.contravariantSemigroupal.*
import com.wavesplatform.account.Address
import com.wavesplatform.database.DBExt
import com.wavesplatform.riderunner.app.RideRunnerMetrics.rideScriptTotalNumber
import com.wavesplatform.riderunner.storage.persistent.CacheKeys
import com.wavesplatform.riderunner.storage.persistent.CacheKeys.{Requests, RequestsLastIndex}
import org.iq80.leveldb.DB
import play.api.libs.json.*

import java.util.concurrent.atomic.AtomicInteger

trait RequestsStorage {
  def size: Int
  def all(): List[RequestKey]

  /** Doesn't check presence
    */
  def append(x: RequestKey): Unit
}

// TODO rename
final case class RequestKey(address: Address, requestBody: JsObject)
object RequestKey {
  implicit val requestsKeyReads: Reads[RequestKey] = Reads {
    case JsArray(rawAddress +: rawRequestBody +: xs) if xs.isEmpty =>
      val address = rawAddress match {
        case JsString(rawAddress) => Address.fromString(rawAddress).left.map(e => s"Expected '$rawAddress' to be an address: $e")
        case x                    => Left(s"Expected a string, got: $x")
      }

      val requestBody = rawRequestBody match {
        case r: JsObject => Right(r)
        case x           => Left(s"Expected a JsObject, got: $x")
      }

      (address, requestBody).mapN(RequestKey.apply) match {
        case Left(e)  => JsError(s"Can't parse RequestKey: $e")
        case Right(r) => JsSuccess(r)
      }

    case x => JsError(s"Expected an array with two elements, got: $x")
  }
}

class LevelDbRequestsStorage(db: DB) extends RequestsStorage {
  private val lastIndexKey = RequestsLastIndex.mkKey(())
  private val lastIndex    = new AtomicInteger(db.readOnly(_.getOpt(lastIndexKey).getOrElse(-1)))
  refreshCounter()

  override def size: Int = lastIndex.get() + 1

  override def all(): List[RequestKey] = db.readOnly { ro =>
    var r = List.empty[RequestKey]
    ro.iterateOver(Requests.prefixBytes) { entry =>
      r = CacheKeys.Requests.parseValue(entry.getValue) :: r
    }
    r // .reverse the order doesn't matter
  }

  override def append(x: RequestKey): Unit = db.readWrite { rw =>
    val newIndex = lastIndex.incrementAndGet()
    val key      = CacheKeys.Requests.mkKey(newIndex)
    rw.put(lastIndexKey, newIndex)
    rw.put(key, x)
    refreshCounter()
  }

  private def refreshCounter(): Unit = rideScriptTotalNumber.update(size)
}
