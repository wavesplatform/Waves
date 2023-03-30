package com.wavesplatform.database

import java.io.File
import java.util
import com.typesafe.scalalogging.StrictLogging
import com.wavesplatform.database.RDB.{TxHandle, TxMetaHandle}
import com.wavesplatform.settings.DBSettings
import com.wavesplatform.utils.*
import org.rocksdb.*

import java.nio.file.{Files, Path}
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
    val file = new File(settings.directory)
    checkDbDir(file.toPath)
    logger.debug(s"Open DB at ${settings.directory}")

    val options = new DBOptions()
      .setCreateIfMissing(true)
      .setParanoidChecks(true)
      .setIncreaseParallelism(4)
      .setBytesPerSync(2 << 20)
      .setMaxBackgroundJobs(4)
      .setCreateMissingColumnFamilies(true)
      .setMaxOpenFiles(100)

    val withStatisticsOptions =
      if (settings.rocksdb.enableStatistics)
        options.setStatistics(new Statistics())
      else options

    val dbDir = file.getAbsoluteFile
    dbDir.getParentFile.mkdirs()

    val handles = new util.ArrayList[ColumnFamilyHandle]()
    val db = RocksDB.open(
      withStatisticsOptions,
      settings.directory,
      Seq(
        new ColumnFamilyDescriptor(
          RocksDB.DEFAULT_COLUMN_FAMILY,
          newColumnFamilyOptions(12.0, 16 << 10, settings.rocksdb.mainCacheSize, 0.6)
            .setCfPaths(Seq(new DbPath(new File(dbDir, "default").toPath, 0L)).asJava)
        ),
        new ColumnFamilyDescriptor(
          "tx-meta".utf8Bytes,
          newColumnFamilyOptions(10.0, 2 << 10, settings.rocksdb.txMetaCacheSize, 0.9)
            .optimizeForPointLookup(16 << 20)
            .setDisableAutoCompactions(true)
            .setCfPaths(Seq(new DbPath(new File(dbDir, "tx-meta").toPath, 0L)).asJava)
        ),
        new ColumnFamilyDescriptor(
          "transactions".utf8Bytes,
          newColumnFamilyOptions(10.0, 2 << 10, settings.rocksdb.txCacheSize, 0.9)
            .setCfPaths(Seq(new DbPath(new File(dbDir, "transactions").toPath, 0L)).asJava)
        )
      ).asJava,
      handles
    )

    new RDB(db, new TxMetaHandle(handles.get(1)), new TxHandle(handles.get(2)))
  }

  private def checkDbDir(dbPath: Path): Unit = {
    val containsLdbFiles = Files.exists(dbPath) && Files.list(dbPath).iterator().asScala.exists(_.getFileName.toString.endsWith(".ldb"))
    if (containsLdbFiles) {
      logger.error(
        s"Database directory ${dbPath.toAbsolutePath.toString} contains LevelDB files (.ldb) which is not compatible with current database. Please delete these files and restart node"
      )
      logger.error("FOR THIS REASON THE NODE STOPPED AUTOMATICALLY")
      forceStopApplication(FatalDBError)
    }
  }
}
