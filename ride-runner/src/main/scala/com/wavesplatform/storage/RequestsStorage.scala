package com.wavesplatform.storage

import com.wavesplatform.account.Address
import com.wavesplatform.database.DBExt
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
  private val numberKey = CacheKeys.RequestsNumber.mkKey(())
  private val number    = new AtomicInteger(db.readOnly(_.getOpt(numberKey).getOrElse(-1)))

  override def all(): List[RequestKey] = db.readOnly { ro =>
    var r = List.empty[RequestKey]
    ro.iterateOver(CacheKeys.Requests.prefixBytes) { entry =>
      r = CacheKeys.Requests.parseValue(entry.getValue) :: r
    }
    r // .reverse the order doesn't matter
  }

  override def append(x: RequestKey): Unit = db.readWrite { rw =>
    val key = CacheKeys.Requests.mkKey(number.incrementAndGet())
    rw.put(key, x)
  }
}
