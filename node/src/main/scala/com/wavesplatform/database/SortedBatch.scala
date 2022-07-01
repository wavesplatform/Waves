package com.wavesplatform.database

import com.wavesplatform.common.state.ByteStr
import org.rocksdb.WriteBatch

import scala.collection.mutable

class SortedBatch extends WriteBatch {
  val addedEntries: mutable.Map[ByteStr, Array[Byte]] = mutable.TreeMap[ByteStr, Array[Byte]]()
  val deletedEntries: mutable.Set[ByteStr]            = mutable.TreeSet[ByteStr]()

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
