package com.wavesplatform.database.jna

import com.google.common.collect.AbstractIterator
import com.protonail.leveldb.jna._
import com.wavesplatform.database.jna.LevelDBJNADB.{JNADBIterator, JNASnapshot, JNAWriteBatch}
import org.iq80.leveldb._

private[jna] class LevelDBJNADB(levelDB: LevelDB) extends DB {
  override def get(key: Array[Byte]): Array[Byte] =
    get(key, new ReadOptions)

  override def get(key: Array[Byte], options: ReadOptions): Array[Byte] = {
    val o1 = LevelDBJNADB.toJNAReadOptions(options)
    try levelDB.get(key, o1)
    finally o1.close()
  }

  override def iterator(): DBIterator =
    new JNADBIterator(levelDB, new LevelDBReadOptions)

  override def iterator(options: ReadOptions): DBIterator =
    new JNADBIterator(levelDB, LevelDBJNADB.toJNAReadOptions(options))

  override def put(key: Array[Byte], value: Array[Byte]): Unit =
    put(key, value, new WriteOptions)

  override def delete(key: Array[Byte]): Unit =
    delete(key, new WriteOptions)

  override def write(updates: WriteBatch): Unit =
    write(updates, new WriteOptions)

  override def createWriteBatch(): WriteBatch =
    new JNAWriteBatch

  override def put(key: Array[Byte], value: Array[Byte], options: WriteOptions): Snapshot = {
    val o1 = LevelDBJNADB.toJNAWriteOptions(options)
    try levelDB.put(key, value, o1)
    finally o1.close()
    null
  }

  override def delete(key: Array[Byte], options: WriteOptions): Snapshot = {
    val o1 = LevelDBJNADB.toJNAWriteOptions(options)
    try levelDB.delete(key, o1)
    finally o1.close()
    null
  }

  override def write(updates: WriteBatch, options: WriteOptions): Snapshot = updates match {
    case wb: JNAWriteBatch =>
      val o1 = LevelDBJNADB.toJNAWriteOptions(options)
      try levelDB.write(wb.batch, o1)
      finally o1.close()
      null

    case _ =>
      throw new IllegalArgumentException(s"Write batch not supported: $updates")
  }

  override def getSnapshot: Snapshot =
    JNASnapshot(levelDB.createSnapshot())

  override def getApproximateSizes(ranges: org.iq80.leveldb.Range*): Array[Long] = {
    val jnaRanges = ranges.map(r => new com.protonail.leveldb.jna.Range(r.start(), r.limit()))
    levelDB.approximateSizes(jnaRanges: _*)
  }

  override def getProperty(name: String): String =
    levelDB.property(name)

  override def suspendCompactions(): Unit = ()

  override def resumeCompactions(): Unit = ()

  override def compactRange(begin: Array[Byte], end: Array[Byte]): Unit =
    levelDB.compactRange(begin, end)

  override def close(): Unit =
    levelDB.close()
}

private object LevelDBJNADB {
  final class JNAWriteBatch extends WriteBatch {
    val batch = new LevelDBWriteBatch

    override def put(key: Array[Byte], value: Array[Byte]): WriteBatch = {
      batch.put(key, value)
      this
    }

    override def delete(key: Array[Byte]): WriteBatch = {
      batch.delete(key)
      this
    }

    override def close(): Unit =
      batch.close()
  }

  private[this] type DBEntry = java.util.Map.Entry[Array[Byte], Array[Byte]]

  //noinspection ScalaStyle
  final class JNADBIterator(levelDB: LevelDB, options: LevelDBReadOptions) extends AbstractIterator[DBEntry] with DBIterator {
    private[this] val iterator = new LevelDBKeyValueIterator(levelDB, options)

    override def seek(key: Array[Byte]): Unit =
      iterator.seekToKey(key)

    override def seekToFirst(): Unit =
      iterator.seekToFirst()

    override def peekNext(): DBEntry =
      this.peek()

    override def hasPrev: Boolean =
      false

    override def prev(): DBEntry =
      throw new NotImplementedError("prev() is not implemented")

    override def peekPrev(): DBEntry =
      throw new NotImplementedError("peekPrev() is not implemented")

    override def seekToLast(): Unit =
      iterator.seekToLast()

    override def close(): Unit = {
      iterator.close()
      options.close()
    }

    override def computeNext(): DBEntry = {
      if (iterator.hasNext) {
        val pair = iterator.next()
        new DBEntry {
          override def getKey: Array[Byte]                       = pair.getKey
          override def getValue: Array[Byte]                     = pair.getValue
          override def setValue(value: Array[Byte]): Array[Byte] = throw new NotImplementedError("setValue(Array[Byte]) is not implemented")
        }
      } else {
        this.endOfData()
        null
      }
    }
  }

  final case class JNASnapshot(snapshot: LevelDBSnapshot) extends Snapshot {
    override def close(): Unit = snapshot.close()
  }

  def toJNAReadOptions(ro: ReadOptions): LevelDBReadOptions = {
    val jna = new LevelDBReadOptions
    jna.setFillCache(ro.fillCache())
    jna.setVerifyChecksum(ro.verifyChecksums())
    ro.snapshot() match {
      case JNASnapshot(snapshot) => jna.setSnapshot(snapshot)
      case _                     => // Ignore
    }
    jna
  }

  def toJNAWriteOptions(wo: WriteOptions): LevelDBWriteOptions = {
    val jna = new LevelDBWriteOptions
    jna.setSync(wo.sync())
    jna
  }
}
