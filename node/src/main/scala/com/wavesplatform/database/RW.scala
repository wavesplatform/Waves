package com.wavesplatform.database

import com.wavesplatform.metrics.RocksDBStats
import com.wavesplatform.metrics.RocksDBStats.DbHistogramExt
import com.wavesplatform.state.Height
import org.rocksdb.{ColumnFamilyHandle, ReadOptions, RocksDB, WriteBatch}

import java.util.Arrays
import scala.collection.mutable.ArrayBuffer

class RW(db: RocksDB, readOptions: ReadOptions, batch: WriteBatch) extends ReadOnlyDB(db, readOptions) {
  import RW.*

  private val ops            = ArrayBuffer.empty[Op]
  private var hasRangeDelete = false

  def put[V](key: Key[V], value: V): Int = {
    val bytes = key.encode(value)
    RocksDBStats.write.recordTagged(key, bytes)
    ops += Put(cf(key), key.keyBytes, bytes)
    bytes.length
  }

  def put(key: Array[Byte], value: Array[Byte]): Unit = ops += Put(db.getDefaultColumnFamily, key, value)

  def update[V](key: Key[V])(f: V => V): Unit = put(key, f(get(key)))

  def delete(key: Array[Byte]): Unit = ops += Delete(db.getDefaultColumnFamily, key)

  def delete[V](key: Key[V]): Unit = ops += Delete(cf(key), key.keyBytes)

  def deleteRange[V](fromInclusive: Key[V], toExclusive: Key[V]): Unit = deleteRange(fromInclusive.keyBytes, toExclusive.keyBytes)

  // Deletes in range [from, to)
  // Keep in mind, that bytes in Java are signed.
  // So fromInclusive=[0, ...] removes all keys, but [Byte.MinValue, ...] can skip some keys.
  def deleteRange(fromInclusive: Array[Byte], toExclusive: Array[Byte]): Unit = {
    hasRangeDelete = true
    ops += DeleteRange(db.getDefaultColumnFamily, fromInclusive, toExclusive)
  }

  def filterHistory(key: Key[Seq[Height]], heightToRemove: Height): Unit = {
    val newValue = get(key).filterNot(_ == heightToRemove)
    if (newValue.nonEmpty) put(key, newValue)
    else delete(key)
  }

  /** Moves the buffered operations into the batch. Memtable inserts in key order reuse the previous insert's skiplist position, so the
    * operations are sorted unless a range delete makes their relative order matter. The sort is stable: operations on one key keep their order.
    */
  private[database] def flush(): Unit = {
    val ordered = if (hasRangeDelete) ops else ops.sortInPlaceWith(before)
    ordered.foreach {
      case Put(h, k, v)             => batch.put(h, k, v)
      case Delete(h, k)             => batch.delete(h, k)
      case DeleteRange(h, from, to) => batch.deleteRange(h, from, to)
    }
    ops.clear()
    hasRangeDelete = false
  }

  private def cf(key: Key[?]): ColumnFamilyHandle = key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily)
}

object RW {
  private sealed trait Op { def cf: ColumnFamilyHandle; def key: Array[Byte] }
  private final case class Put(cf: ColumnFamilyHandle, key: Array[Byte], value: Array[Byte])      extends Op
  private final case class Delete(cf: ColumnFamilyHandle, key: Array[Byte])                       extends Op
  private final case class DeleteRange(cf: ColumnFamilyHandle, key: Array[Byte], to: Array[Byte]) extends Op

  private def before(a: Op, b: Op): Boolean = {
    val c = Integer.compare(a.cf.getID, b.cf.getID)
    if (c != 0) c < 0 else Arrays.compareUnsigned(a.key, b.key) < 0
  }
}
