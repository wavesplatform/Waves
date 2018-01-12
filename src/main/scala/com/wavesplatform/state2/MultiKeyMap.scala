package com.wavesplatform.state2

import org.h2.mvstore.MVStore
import org.h2.mvstore.`type`.DataType
import scorex.utils.LogMVMapBuilder

class MultiKeyMap[V](db: MVStore, valueDataType: DataType, prefix: String) {
  private def compositeKey(k1: ByteStr, k2: ByteStr): Array[Byte] = k1.arr ++ k2.arr

  private val k1k2v = db.openMap(prefix + "k1k2v", new LogMVMapBuilder[Array[Byte], V]
    .valueType(valueDataType))

  private val k1k2set = db.openMap(prefix + "k1k2set", new LogMVMapBuilder[ByteStr, List[ByteStr]]
    .keyType(DataTypes.byteStr)
    .valueType(DataTypes.byteStrList))


  def put(k1: ByteStr, k2: ByteStr, v: V): Unit = {
    k1k2v.put(compositeKey(k1, k2), v)
    Option(k1k2set.get(k1)) match {
      case None => k1k2set.put(k1, List(k2))
      case Some(list) if list contains k2 =>
      case Some(list) => k1k2set.put(k1, k2 +: list)
    }
  }

  def get(k1: ByteStr, k2: ByteStr): Option[V] = {
    Option(k1k2v.get(compositeKey(k1, k2)))
  }

  def keys(k1: ByteStr): Set[ByteStr] = Option(k1k2set.get(k1))
    .map(_.toSet)
    .getOrElse(Set.empty[ByteStr])

  def getMap(k1: ByteStr): Map[ByteStr, V] = keys(k1)
    .map(x => x -> get(k1, x).get)
    .toMap

  def clear(): Unit = {
    k1k2v.clear()
    k1k2set.clear()
  }

  def all(): Map[ByteStr, Map[ByteStr, V]] = {
    import scala.collection.JavaConverters._
    k1k2set.entrySet().asScala.map { e =>
      e.getKey -> e.getValue.map(k2 => k2 -> k1k2v.get(compositeKey(e.getKey, k2))).toMap
    }.toMap
  }
}
