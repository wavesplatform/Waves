package com.wavesplatform.database

import java.util.Comparator

import com.wavesplatform.common.ByteStrComparator
import com.wavesplatform.common.state.ByteStr
import org.rocksdb.WriteBatch
import com.wavesplatform.database.InMemoryDB.ByteArrayHashingStrategy
import org.eclipse.collections.impl.factory.{HashingStrategyMaps, HashingStrategySets}

class SortedBatch extends WriteBatch {
  val addedEntries   = HashingStrategyMaps.mutable.`with`[Array[Byte], Array[Byte]](ByteArrayHashingStrategy)
  val deletedEntries = HashingStrategySets.mutable.`with`[Array[Byte]](ByteArrayHashingStrategy)

  override def put(bytes: Array[Byte], bytes1: Array[Byte]): Unit = {
    addedEntries.put(bytes, bytes1)
    deletedEntries.remove(bytes)
    this
  }

  override def delete(bytes: Array[Byte]): Unit = {
    addedEntries.remove(bytes)
    deletedEntries.add(bytes)
    this
  }

}

object SortedBatch {
  val byteStrComparator: Comparator[ByteStr] = (o1: ByteStr, o2: ByteStr) => ByteStrComparator.compare(o1, o2)
}
