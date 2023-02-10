package com.wavesplatform.database

import java.io.File
import java.util

import com.typesafe.scalalogging.StrictLogging
import com.wavesplatform.database.RDB.{TxHandle, TxMetaHandle}
import com.wavesplatform.settings.DBSettings
import com.wavesplatform.utils.*
import org.rocksdb.*

import scala.jdk.CollectionConverters.*

final class RDB(
    val db: RocksDB,
    val txMetaHandle: TxMetaHandle,
    val txHandle: TxHandle
) extends AutoCloseable {
  override def close(): Unit = db.close()
}

object RDB extends StrictLogging {
  final class TxMetaHandle private[RDB] (val handle: ColumnFamilyHandle)
  final class TxHandle private[RDB] (val handle: ColumnFamilyHandle)

  private def newColumnFamilyOptions(bitsPerKey: Double, blockSize: Long, cacheCapacity: Long, highPriPoolRatio: Double) = new ColumnFamilyOptions()
    .setTableFormatConfig(
      new BlockBasedTableConfig()
        .setFilterPolicy(new BloomFilter(bitsPerKey))
        .setOptimizeFiltersForMemory(true)
        .setCacheIndexAndFilterBlocks(true)
        .setPinL0FilterAndIndexBlocksInCache(true)
        .setFormatVersion(5)
        .setBlockSize(blockSize)
        .setChecksumType(ChecksumType.kNoChecksum)
        .setBlockCache(new LRUCache(cacheCapacity, -1, false, highPriPoolRatio))
        .setCacheIndexAndFilterBlocksWithHighPriority(true)
        .setDataBlockIndexType(DataBlockIndexType.kDataBlockBinaryAndHash)
        .setDataBlockHashTableUtilRatio(0.5)
    )
    .setWriteBufferSize(128 << 20)
    .setLevelCompactionDynamicLevelBytes(true)
    .useCappedPrefixExtractor(10)
    .setMemtablePrefixBloomSizeRatio(0.25)
    .setCompressionType(CompressionType.LZ4_COMPRESSION)
    .setSstPartitionerFactory(new SstPartitionerFixedPrefixFactory(2))

  def open(settings: DBSettings): RDB = {
    logger.debug(s"Open DB at ${settings.directory}")
    val file = new File(settings.directory)
    val options = new DBOptions()
      .setStatistics(new Statistics())
      .setStatsDumpPeriodSec(300)
      .setCreateIfMissing(true)
      .setParanoidChecks(true)
      .setIncreaseParallelism(4)
      .setBytesPerSync(2 << 20)
      .setMaxBackgroundJobs(4)
      .setCreateMissingColumnFamilies(true)
      .setMaxOpenFiles(100)

    val dbDir = file.getAbsoluteFile
    dbDir.getParentFile.mkdirs()

    val handles = new util.ArrayList[ColumnFamilyHandle]()
    val db = RocksDB.open(
      options,
      settings.directory,
      Seq(
        new ColumnFamilyDescriptor(
          RocksDB.DEFAULT_COLUMN_FAMILY,
          newColumnFamilyOptions(12.0, 16 << 10, 512 << 20, 0.6)
            .setCfPaths(Seq(new DbPath(new File(dbDir, "default").toPath, 0L)).asJava)
        )
      ).asJava,
      handles
    )

    val txMeta = new TxMetaHandle(
      db.createColumnFamily(
        new ColumnFamilyDescriptor(
          "tx-meta".utf8Bytes,
          newColumnFamilyOptions(10.0, 2 << 10, 16 << 20, 0.9)
            .optimizeForPointLookup(16 << 20)
            .setDisableAutoCompactions(true)
            .setCfPaths(Seq(new DbPath(new File(dbDir, "tx-meta").toPath, 0L)).asJava)
        )
      )
    )

    val txHandle = new TxHandle(
      db.createColumnFamily(
        new ColumnFamilyDescriptor(
          "transactions".utf8Bytes,
          newColumnFamilyOptions(10.0, 2 << 10, 16 << 20, 0.9)
            .setCfPaths(Seq(new DbPath(new File(dbDir, "transactions").toPath, 0L)).asJava)
        )
      )
    )

    new RDB(db, txMeta, txHandle)
  }
}
