package com.wavesplatform.database

import java.io.*

import com.google.common.hash.{Funnels, HashFunction, Hashing, HashingInputStream, HashingOutputStream, BloomFilter as GBloomFilter}
import com.google.common.primitives.Ints
import com.wavesplatform.utils.ScorexLogging
import org.rocksdb.RocksDB

import scala.util.Try
import scala.util.control.NonFatal

trait BloomFilter {
  def mightContain(key: Array[Byte]): Boolean
  def put(key: Array[Byte]): Unit
}

private[database] class Wrapper(underlying: GBloomFilter[Array[Byte]]) extends BloomFilter {
  override def mightContain(key: Array[Byte]): Boolean = underlying.mightContain(key)
  override def put(key: Array[Byte]): Unit             = underlying.put(key)
}

private[database] class BloomFilterImpl(underlying: GBloomFilter[Array[Byte]], directory: String, filterName: String, db: RocksDB)
    extends Wrapper(underlying)
    with ScorexLogging {
  import com.wavesplatform.database.BloomFilter._
  def save(height: Int): Unit = {
    val file = filterFile(directory, filterName)
    log.info(s"Saving bloom filter to ${file.getAbsolutePath}")
    val out = new HashingOutputStream(
      defaultHash,
      new BufferedOutputStream(new FileOutputStream(file), BufferSize)
    )
    try {
      out.write(Ints.toByteArray(height))
      underlying.writeTo(out)
    } finally out.close()

    val checksum = out.hash()
    db.readWrite(_.put(Keys.bloomFilterChecksum(filterName), checksum.asBytes()))
    log.info(s"Filter hash: $checksum")
  }
}

object BloomFilter extends ScorexLogging {
  val BufferSize                = 2 * 1024 * 1024
  val Suffix                    = ".bf"
  def defaultHash: HashFunction = Hashing.sha256()

  private[database] object AlwaysEmpty extends BloomFilter {
    override def mightContain(key: Array[Byte]): Boolean = true
    override def put(key: Array[Byte]): Unit             = {}
  }

  private[database] def filterFile(directory: String, filterName: String): File = new File(directory, filterName + Suffix)

  def tryLoad(
      db: RocksDB,
      filterName: String,
      directory: String,
      expectedHeight: Int
  ): Try[GBloomFilter[Array[Byte]]] = Try {
    val storedChecksum = db.get(Keys.bloomFilterChecksum(filterName))
    val ff             = filterFile(directory, filterName)
    val in             = new HashingInputStream(defaultHash, new BufferedInputStream(new FileInputStream(ff), BufferSize))
    log.debug(s"Loading bloom filter from ${ff.getAbsolutePath}")
    try {
      val heightBytes = new Array[Byte](java.lang.Integer.BYTES)
      in.read(heightBytes)
      val height = Ints.fromByteArray(heightBytes)
      val filter = GBloomFilter.readFrom(in, Funnels.byteArrayFunnel())
      val code   = in.hash()
      require(code.asBytes().sameElements(storedChecksum), "checksum mismatch")
      require(height == expectedHeight, "filter is stale")
      filter
    } finally in.close()
  }

  private def populate(db: RocksDB, keyTag: KeyTags.KeyTag, filterName: String, expectedInsertions: Long) = {
    log.info(s"Populating bloom filter for $filterName, this can take a while.")
    val filter = GBloomFilter.create(Funnels.byteArrayFunnel(), expectedInsertions)
    db.iterateOver(keyTag)(e => filter.put(e.getKey.drop(2)))
    log.info(s"Populating bloom filter for $filterName finished.")
    filter
  }

  def loadOrPopulate(
      db: RocksDB,
      directory: String,
      filterName: String,
      expectedHeight: Int,
      keyTag: KeyTags.KeyTag,
      expectedInsertions: Long
  ): BloomFilterImpl = {
    val ff = filterFile(directory, filterName)
    val underlying = tryLoad(db, filterName, directory, expectedHeight).recover {
      case _: FileNotFoundException =>
        log.trace(s"Filter file ${ff.getAbsoluteFile} is missing, will re-build the filter from scratch")
        populate(db, keyTag, filterName, expectedInsertions)
      case NonFatal(e) =>
        log.debug(s"Could not load bloom filter from ${ff.getAbsolutePath}", e)
        populate(db, keyTag, filterName, expectedInsertions)
    }.get

    new BloomFilterImpl(underlying, directory, filterName, db)
  }
}
