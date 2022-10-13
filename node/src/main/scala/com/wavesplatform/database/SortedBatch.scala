package com.wavesplatform.database

import java.util
import java.util.Comparator

import com.google.common.primitives.UnsignedBytes
import com.wavesplatform.common.ByteStrComparator
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.InMemoryDB.ByteArrayHashingStrategy
import org.eclipse.collections.api.factory.SortedMaps
import org.eclipse.collections.impl.factory.{HashingStrategyMaps, HashingStrategySets}
import org.iq80.leveldb.WriteBatch

class SortedBatch extends WriteBatch {
  val addedEntries = HashingStrategyMaps.mutable.`with`[Array[Byte], Array[Byte]](ByteArrayHashingStrategy)
  val deletedEntries = HashingStrategySets.mutable.`with`[Array[Byte]](ByteArrayHashingStrategy)

  override def put(bytes: Array[Byte], bytes1: Array[Byte]): WriteBatch = {
    addedEntries.put(bytes, bytes1)
    deletedEntries.remove(bytes)
    this
  }

  override def delete(bytes: Array[Byte]): WriteBatch = {
    addedEntries.remove(bytes)
    deletedEntries.add(bytes)
    this
  }

  override def close(): Unit = {}
}

object SortedBatch {
  val byteStrComparator: Comparator[ByteStr] = (o1: ByteStr, o2: ByteStr) => ByteStrComparator.compare(o1, o2)
}
