package com.wavesplatform.db

import com.google.common.primitives.{Bytes, Ints}
import org.iq80.leveldb.{DB, WriteBatch}

class SubStorage(db: DB, name: String) extends Storage(db) {

  private val subPrefix: Array[Byte] = name.getBytes(Charset)

  override protected def makePrefix(prefix: Array[Byte]): Array[Byte] = Bytes.concat(subPrefix, Separator, prefix, Separator)

  override protected def makeKey(prefix: Array[Byte], key: Array[Byte]): Array[Byte] = Bytes.concat(subPrefix, Separator, prefix, Separator, key)

  override protected def makeKey(prefix: Array[Byte], key: String): Array[Byte] = makeKey(prefix, key.getBytes(Charset))

  override protected def makeKey(prefix: Array[Byte], key: Int): Array[Byte] = makeKey(prefix, Ints.toByteArray(key))

  override def removeEverything(b: Option[WriteBatch]): Unit = {
    val it = allKeys
    while (it.hasNext) {
      val key = it.next()
      if (key.startsWith(subPrefix)) delete(key, b)
    }
    it.close()
  }

}
