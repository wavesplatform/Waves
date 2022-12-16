package com.wavesplatform.storage

import com.wavesplatform.account.Address
import com.wavesplatform.database.DBExt
import com.wavesplatform.ride.app.RideRunnerMetrics.rideScriptTotalNumber
import com.wavesplatform.storage.RequestsStorage.RequestKey
import com.wavesplatform.storage.persistent.CacheKeys
import org.iq80.leveldb.DB
import play.api.libs.json.JsObject

import java.util.concurrent.atomic.AtomicInteger

trait RequestsStorage {
  def all(): List[RequestKey]
  def append(x: RequestKey): Unit
}

object RequestsStorage {
  type RequestKey = (Address, JsObject)
}

class LevelDbRequestsStorage(db: DB) extends RequestsStorage {
  private val lastIndexKey = CacheKeys.RequestsLastIndex.mkKey(())
  private val lastIndex    = new AtomicInteger(db.readOnly(_.getOpt(lastIndexKey).getOrElse(-1)))
  rideScriptTotalNumber.update(lastIndex.get())

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
    rideScriptTotalNumber.update(newIndex + 1) // Because it starts from 0
    rw.put(lastIndexKey, newIndex)
    rw.put(key, x)
  }
}
