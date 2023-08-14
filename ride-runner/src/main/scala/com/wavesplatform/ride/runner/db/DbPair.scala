package com.wavesplatform.ride.runner.db

import com.wavesplatform.database.DBEntry
import com.wavesplatform.ride.runner.caches.persistent.KvPair

class DbPair[KeyT, ValueT](kvPair: KvPair[KeyT, ValueT], val dbEntry: DBEntry) {
  lazy val key: KeyT     = kvPair.parseKey(dbEntry.getKey)
  lazy val value: ValueT = kvPair.parseValue(dbEntry.getValue)
}
