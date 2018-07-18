package com.wavesplatform.db

import java.nio.charset.{Charset, StandardCharsets}

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.utils.{ScorexLogging, forceStopApplication}
import org.iq80.leveldb.{DB, DBIterator, WriteBatch}

import scala.collection.AbstractIterator
import scala.util.control.NonFatal

abstract class Storage(private val db: DB) extends ScorexLogging {
  protected val Charset: Charset = StandardCharsets.UTF_8

  protected val Separator: Array[Byte] = Array[Byte](':')

  def get(key: Array[Byte]): Option[Array[Byte]] = {
    try {
      Option(db.get(key))
    } catch {
      case NonFatal(t) =>
        log.error("LevelDB get error", t)
        forceStopApplication()
        throw t
    }
  }

  def createBatch(): Option[WriteBatch] = {
    try {
      Some(db.createWriteBatch())
    } catch {
      case NonFatal(t) =>
        log.error("LevelDB create batch error", t)
        forceStopApplication()
        throw t
    }
  }

  def put(key: Array[Byte], value: Array[Byte], batch: Option[WriteBatch]): Unit = {
    try {
      if (batch.isDefined) batch.get.put(key, value) else db.put(key, value)
    } catch {
      case NonFatal(t) =>
        log.error("LevelDB batch put error", t)
        forceStopApplication()
        throw t
    }
  }

  def delete(key: Array[Byte], batch: Option[WriteBatch]): Unit = {
    try {
      if (batch.isDefined) batch.get.delete(key) else db.delete(key)
    } catch {
      case NonFatal(t) =>
        log.error("LevelDB delete error", t)
        forceStopApplication()
        throw t
    }
  }

  def commit(batch: Option[WriteBatch]): Unit = {
    batch.foreach { b =>
      try {
        db.write(b)
      } catch {
        case NonFatal(t) =>
          log.error("LevelDB write batch error", t)
          forceStopApplication()
          throw t
      } finally {
        b.close()
      }
    }
  }

  class KeysIterator(val it: DBIterator) extends AbstractIterator[Array[Byte]] {
    override def hasNext: Boolean = it.hasNext

    override def next(): Array[Byte] = it.next().getKey

    def close(): Unit = it.close()
  }

  protected def allKeys: KeysIterator = {
    val it: DBIterator = db.iterator()
    it.seekToFirst()
    new KeysIterator(it)
  }

  def removeEverything(b: Option[WriteBatch]): Unit

  protected def makePrefix(prefix: Array[Byte]): Array[Byte] = Bytes.concat(prefix, Separator)

  protected def makeKey(prefix: Array[Byte], key: Array[Byte]): Array[Byte] = Bytes.concat(prefix, Separator, key, Separator)

  protected def makeKey(prefix: Array[Byte], key: String): Array[Byte] = makeKey(prefix, key.getBytes(Charset))

  protected def makeKey(prefix: Array[Byte], key: Int): Array[Byte] = makeKey(prefix, Ints.toByteArray(key))
}
