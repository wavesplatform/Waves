package com.wavesplatform.database

import java.util.Comparator
import com.wavesplatform.common.ByteStrComparator
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.SortedBatch.ByteArrayHashingStrategy
import org.rocksdb.WriteBatch
import org.eclipse.collections.api.block.HashingStrategy
import org.eclipse.collections.impl.factory.{HashingStrategyMaps, HashingStrategySets}

class SortedBatch extends WriteBatch {
  val addedEntries   = HashingStrategyMaps.mutable.`with`[Array[Byte], Array[Byte]](ByteArrayHashingStrategy)
  val deletedEntries = HashingStrategySets.mutable.`with`[Array[Byte]](ByteArrayHashingStrategy)

  override def put(bytes: Array[Byte], bytes1: Array[Byte]): Unit = {
    addedEntries.put(bytes, bytes1)
    deletedEntries.remove(bytes)
  }

  override def delete(bytes: Array[Byte]): Unit = {
    addedEntries.remove(bytes)
    deletedEntries.add(bytes)
  }

}

object SortedBatch {
  val byteStrComparator: Comparator[ByteStr] = (o1: ByteStr, o2: ByteStr) => ByteStrComparator.compare(o1, o2)

  object ByteArrayHashingStrategy extends HashingStrategy[Array[Byte]] {
    override def computeHashCode(obj: Array[Byte]): Int = java.util.Arrays.hashCode(obj)

    override def equals(object1: Array[Byte], object2: Array[Byte]): Boolean = java.util.Arrays.equals(object1, object2)
  }
}
