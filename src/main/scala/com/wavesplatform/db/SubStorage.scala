package com.wavesplatform.db

import com.google.common.primitives.{Bytes, Ints}
import org.iq80.leveldb.{DB, WriteBatch}

class SubStorage(db: DB, name: String) extends Storage(db) {

  private val subPrefix: Array[Byte] = name.getBytes(Charset)

  override protected def makeKey(prefix: Array[Byte], key: Array[Byte]): Array[Byte] = Bytes.concat(subPrefix, Separator, prefix, Separator, key, Separator)

  override protected def makeKey(prefix: Array[Byte], key: String): Array[Byte] = makeKey(prefix, key.getBytes(Charset))

  override protected def makeKey(prefix: Array[Byte], key: Int): Array[Byte] = makeKey(prefix, Ints.toByteArray(key))

  override def removeEverything(b: Option[WriteBatch]): Unit = {
    map(makePrefix(subPrefix), stripPrefix = false).foreach(e => delete(e._1, b))
  }

}
