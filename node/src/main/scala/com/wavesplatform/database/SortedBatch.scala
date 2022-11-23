package com.wavesplatform.database

import java.util
import java.util.Comparator

import com.wavesplatform.common.ByteStrComparator
import com.wavesplatform.common.state.ByteStr
import org.rocksdb.WriteBatch

class SortedBatch extends WriteBatch {
  val addedEntries   = new util.TreeMap[ByteStr, Array[Byte]](SortedBatch.byteStrComparator)
  val deletedEntries = new util.TreeSet[ByteStr](SortedBatch.byteStrComparator)

  override def put(bytes: Array[Byte], bytes1: Array[Byte]): Unit = {
    val k = ByteStr(bytes)
    addedEntries.put(k, bytes1)
    deletedEntries.remove(k)
  }

  override def delete(bytes: Array[Byte]): Unit = {
    val k = ByteStr(bytes)
    addedEntries.remove(k)
    deletedEntries.add(k)
  }

}

object SortedBatch {
  val byteStrComparator: Comparator[ByteStr] = (o1: ByteStr, o2: ByteStr) => ByteStrComparator.compare(o1, o2)
}
