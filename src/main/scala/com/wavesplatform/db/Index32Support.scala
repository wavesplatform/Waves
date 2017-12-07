package com.wavesplatform.db

import com.wavesplatform.state2._
import org.iq80.leveldb.WriteBatch

trait Index32Support {
  this: Storage =>

  protected val SubKeyLength = 8
  protected val SubKeysNumber = 4

  def addToIndex32(batch: Option[WriteBatch], indexPrefix: Array[Byte], id: ByteStr): Unit = {
    require(id.arr.length == SubKeysNumber * SubKeyLength, "incorrect id length")

    val (part1, part2, part3, part4) = splitId(id)

    val key0 = makeIndexKey(indexPrefix)
    val key1 = makeIndexKey(indexPrefix, part1)
    val key2 = makeIndexKey(indexPrefix, part1, part2)
    val key3 = makeIndexKey(indexPrefix, part1, part2, part3)

    val level0 = get(key0).map(KeySeqCodec.decode).map(_.explicitGet().value).getOrElse(Seq.empty)
    if (!level0.contains(part1)) put(key0, KeySeqCodec.encode(append(level0, part1)), batch)

    val level1 = get(key1).map(KeySeqCodec.decode).map(_.explicitGet().value).getOrElse(Seq.empty)
    if (!level1.contains(part2)) put(key1, KeySeqCodec.encode(append(level1, part2)), batch)

    val level2 = get(key2).map(KeySeqCodec.decode).map(_.explicitGet().value).getOrElse(Seq.empty)
    if (!level2.contains(part3)) put(key2, KeySeqCodec.encode(append(level2, part3)), batch)

    val level3 = get(key3).map(KeySeqCodec.decode).map(_.explicitGet().value).getOrElse(Seq.empty)
    if (!level3.contains(part4)) put(key3, KeySeqCodec.encode(append(level3, part4)), batch)
  }

  def removeFromIndex32(batch: Option[WriteBatch], indexPrefix: Array[Byte], id: ByteStr): Unit = {
    require(id.arr.length == SubKeysNumber * SubKeyLength, "incorrect id length")

    val (part1, part2, part3, part4) = splitId(id)

    val key0 = makeIndexKey(indexPrefix)
    val key1 = makeIndexKey(indexPrefix, part1)
    val key2 = makeIndexKey(indexPrefix, part1, part2)
    val key3 = makeIndexKey(indexPrefix, part1, part2, part3)

    val level0 = get(key0).map(KeySeqCodec.decode).map(_.explicitGet().value).getOrElse(Seq.empty)
    if (!level0.contains(part1)) updateOrRemove(key0, remove(level0, part1), batch)

    val level1 = get(key1).map(KeySeqCodec.decode).map(_.explicitGet().value).getOrElse(Seq.empty)
    if (!level1.contains(part2)) updateOrRemove(key1, remove(level1, part2), batch)

    val level2 = get(key2).map(KeySeqCodec.decode).map(_.explicitGet().value).getOrElse(Seq.empty)
    if (!level2.contains(part3)) updateOrRemove(key2, remove(level2, part3), batch)

    val level3 = get(key3).map(KeySeqCodec.decode).map(_.explicitGet().value).getOrElse(Seq.empty)
    if (!level3.contains(part4)) updateOrRemove(key3, remove(level3, part4), batch)
  }

  def allIndex32(indexPrefix: Array[Byte]): Seq[ByteStr] = {
    for {
      p1 <- getLevel(indexPrefix)
      p2 <- getLevel(indexPrefix, p1)
      p3 <- getLevel(indexPrefix, p1, p2)
      p4 <- getLevel(indexPrefix, p1, p2, p3)
    } yield combineId(p1, p2, p3, p4)
  }

  def isIndexed(indexPrefix: Array[Byte], id: ByteStr): Boolean = {
    require(id.arr.length == SubKeysNumber * SubKeyLength, "incorrect id length")

    val (part1, part2, part3, _) = splitId(id)

    val key0 = makeIndexKey(indexPrefix)
    val key1 = makeIndexKey(indexPrefix, part1)
    val key2 = makeIndexKey(indexPrefix, part1, part2)
    val key3 = makeIndexKey(indexPrefix, part1, part2, part3)

    val r1 = get(key0).map(KeySeqCodec.decode).exists(_.isRight)
    val r2 = get(key1).map(KeySeqCodec.decode).exists(_.isRight)
    val r3 = get(key2).map(KeySeqCodec.decode).exists(_.isRight)
    val r4 = get(key3).map(KeySeqCodec.decode).exists(_.isRight)

    r1 && r2 && r3 && r4
  }

  private def updateOrRemove(key: Array[Byte], seq: Seq[ByteStr], batch: Option[WriteBatch]): Unit =
    if (seq.nonEmpty) put(key, KeySeqCodec.encode(seq), batch) else delete(key, batch)

  private def getLevel(indexPrefix: Array[Byte], parts: ByteStr*): Seq[ByteStr] =
    get(makeIndexKey(indexPrefix, parts: _*)).map(KeySeqCodec.decode).map(_.explicitGet().value).getOrElse(Seq.empty)

  private def combineId(p1: ByteStr, p2: ByteStr, p3: ByteStr, p4: ByteStr): ByteStr = {
    val result = new Array[Byte](SubKeyLength * SubKeysNumber)
    System.arraycopy(p1.arr, 0, result, 0, SubKeyLength)
    System.arraycopy(p2.arr, 0, result, SubKeyLength, SubKeyLength)
    System.arraycopy(p3.arr, 0, result, 2 * SubKeyLength, SubKeyLength)
    System.arraycopy(p4.arr, 0, result, 3 * SubKeyLength, SubKeyLength)
    new ByteStr(result)
  }

  private def makeIndexKey(indexPrefix: Array[Byte], parts: ByteStr*): Array[Byte] = {
    val level = parts.length
    assert(level < SubKeysNumber, "incorrect index level")

    val indexLength = indexPrefix.length
    val result = new Array[Byte](indexLength + level * (SubKeyLength + 1))
    System.arraycopy(indexPrefix, 0, result, 0, indexLength)
    var i = 0
    while (i < level) {
      val part = parts(i)
      System.arraycopy(Separator, 0, result, indexLength + i * (SubKeyLength + 1), 1)
      System.arraycopy(part.arr, 0, result, indexLength + i * (SubKeyLength + 1) + 1, SubKeyLength)
      i += 1
    }
    result
  }

  private def splitId(id: ByteStr): (ByteStr, ByteStr, ByteStr, ByteStr) = {
    val part1 = new Array[Byte](SubKeyLength)
    val part2 = new Array[Byte](SubKeyLength)
    val part3 = new Array[Byte](SubKeyLength)
    val part4 = new Array[Byte](SubKeyLength)

    System.arraycopy(id.arr, 0, part1, 0, SubKeyLength)
    System.arraycopy(id.arr, SubKeyLength, part2, 0, SubKeyLength)
    System.arraycopy(id.arr, 2 * SubKeyLength, part3, 0, SubKeyLength)
    System.arraycopy(id.arr, 3 * SubKeyLength, part4, 0, SubKeyLength)

    (ByteStr(part1), ByteStr(part2), ByteStr(part3), ByteStr(part4))
  }

  private def append(seq: Seq[ByteStr], value: ByteStr): Seq[ByteStr] = {
    val builder = Seq.newBuilder[ByteStr]
    builder ++= seq
    builder += value
    builder.result()
  }

  private def remove(seq: Seq[ByteStr], value: ByteStr): Seq[ByteStr] = {
    val builder = Seq.newBuilder[ByteStr]
    builder ++= seq.filterNot(_ == value)
    builder.result()
  }
}
