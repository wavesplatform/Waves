package com.wavesplatform.database

import java.io._

import com.google.common.hash._
import com.google.common.primitives.Ints
import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.DB

object BloomFilters extends ScorexLogging {
  private val BufferSize                = 2 * 1024 * 1024
  private val Suffix                    = ".bf"
  private def defaultHash: HashFunction = Hashing.sha256()

  def loadBloomFilter(
      db: DB,
      directory: String,
      filterName: String,
      expectedHeight: Int,
      keyTag: KeyTags.KeyTag
  ): BloomFilter[Array[Byte]] = {
    val fileName       = filterName + Suffix
    val storedChecksum = db.get(Keys.bloomFilterChecksum(filterName))
    try {
      val in = new HashingInputStream(
        defaultHash,
        new BufferedInputStream(new FileInputStream(new File(directory, fileName)), BufferSize)
      )
      log.info(s"Loading bloom filter from $fileName")
      try {
        val heightBytes = new Array[Byte](java.lang.Integer.BYTES)
        in.read(heightBytes)
        val height = Ints.fromByteArray(heightBytes)
        val filter = BloomFilter.readFrom(in, Funnels.byteArrayFunnel())
        val code   = in.hash()
        require(code.asBytes().sameElements(storedChecksum), "checksum mismatch")
        require(height == expectedHeight, "filter is stale")
        log.info(s"Element count: ${filter.approximateElementCount()}")
        filter
      } finally in.close()
    } catch {
      case e: Exception =>
        log.info(s"Error loading bloom filter from $fileName", e)
        val bf = BloomFilter.create(Funnels.byteArrayFunnel(), 100000000L)
        db.iterateOver(keyTag)(e => bf.put(e.getKey.drop(2)))
        bf
    }
  }

  def saveBloomFilter(filter: BloomFilter[_], db: DB, directory: String, fileName: String, height: Int): Unit = {
    log.info(s"Saving bloom filter to $fileName")

    val out = new HashingOutputStream(
      defaultHash,
      new BufferedOutputStream(new FileOutputStream(new File(directory, fileName + Suffix)), BufferSize)
    )
    try {
      out.write(Ints.toByteArray(height))
      filter.writeTo(out)
    } finally out.close()

    val checksum = out.hash()
    log.info(s"Filter hash: $checksum")
    db.put(Keys.bloomFilterChecksum(fileName).keyBytes, checksum.asBytes())
  }
}
