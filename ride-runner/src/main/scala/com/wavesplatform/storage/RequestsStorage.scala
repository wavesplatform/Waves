package com.wavesplatform.storage

import com.wavesplatform.account.Address
import com.wavesplatform.database.DBExt
import com.wavesplatform.ride.app.RideRunnerMetrics.rideScriptTotalNumber
import com.wavesplatform.storage.persistent.CacheKeys
import org.iq80.leveldb.DB
import play.api.libs.json.{JsObject, Json, Reads}

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
  implicit val requestsKeyReads: Reads[RequestKey] = Json.reads[(Address, JsObject)].map(Function.tupled(RequestKey))
}

class LevelDbRequestsStorage(db: DB) extends RequestsStorage {
  private val lastIndexKey = CacheKeys.RequestsLastIndex.mkKey(())
  private val lastIndex    = new AtomicInteger(db.readOnly(_.getOpt(lastIndexKey).getOrElse(-1)))
  refreshCounter()

  override def size: Int = lastIndex.get() + 1

  override def all(): List[RequestKey] = db.readOnly { ro =>
    var r = List.empty[RequestKey]
    ro.iterateOver(CacheKeys.Requests.prefixBytes) { entry =>
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
