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
    val txHandle: TxHandle,
    acquiredResources: Seq[RocksObject]
) extends AutoCloseable {
  def wrapDb(f: RocksDB => RocksDB): Either[String, RDB] = {
    val wrappedDb = f(db)
    Either.cond(
      wrappedDb.getNativeHandle == db.getNativeHandle,
      new RDB(wrappedDb, txMetaHandle, txHandle, acquiredResources),
      "Failed to wrap DB: passed function results in new database instance"
    )
  }

  override def close(): Unit = {
    acquiredResources.foreach(_.close())
    db.close()
  }
}

object RDB extends StrictLogging {
  final class TxMetaHandle private[RDB] (val handle: ColumnFamilyHandle)
  final class TxHandle private[RDB] (val handle: ColumnFamilyHandle)
  case class OptionsWithResources[A](options: A, resources: Seq[RocksObject])

  def open(settings: DBSettings): RDB = {
    val file = new File(settings.directory)
    checkDbDir(file.toPath)
    logger.debug(s"Open DB at ${settings.directory}")

    val dbOptions = createDbOptions(settings)

    val dbDir = file.getAbsoluteFile
    dbDir.getParentFile.mkdirs()

    val handles          = new util.ArrayList[ColumnFamilyHandle]()
    val defaultCfOptions = newColumnFamilyOptions(12.0, 16 << 10, settings.rocksdb.mainCacheSize, 0.6, settings.rocksdb.writeBufferSize)
    val txMetaCfOptions  = newColumnFamilyOptions(10.0, 2 << 10, settings.rocksdb.txMetaCacheSize, 0.9, settings.rocksdb.writeBufferSize)
    val txCfOptions      = newColumnFamilyOptions(10.0, 2 << 10, settings.rocksdb.txCacheSize, 0.9, settings.rocksdb.writeBufferSize)
    val db = RocksDB.open(
      dbOptions.options,
      settings.directory,
      Seq(
        new ColumnFamilyDescriptor(
          RocksDB.DEFAULT_COLUMN_FAMILY,
          defaultCfOptions.options
            .setCfPaths(Seq(new DbPath(new File(dbDir, "tx-meta").toPath, 0L)).asJava)
        ),
        new ColumnFamilyDescriptor(
          "tx-meta".utf8Bytes,
          txMetaCfOptions.options
            .optimizeForPointLookup(16 << 20)
            .setDisableAutoCompactions(true)
            .setCfPaths(Seq(new DbPath(new File(dbDir, "tx-meta").toPath, 0L)).asJava)
        ),
        new ColumnFamilyDescriptor(
          "transactions".utf8Bytes,
          txCfOptions.options
            .setCfPaths(Seq(new DbPath(new File(dbDir, "transactions").toPath, 0L)).asJava)
        )
      ).asJava,
      handles
    )

    new RDB(
      db,
      new TxMetaHandle(handles.get(1)),
      new TxHandle(handles.get(2)),
      dbOptions.resources ++ defaultCfOptions.resources ++ txMetaCfOptions.resources ++ txCfOptions.resources
    )
  }

  private def newColumnFamilyOptions(
      bitsPerKey: Double,
      blockSize: Long,
      cacheCapacity: Long,
      highPriPoolRatio: Double,
      writeBufferSize: Long
  ): OptionsWithResources[ColumnFamilyOptions] = {
    val bloomFilter           = new BloomFilter(bitsPerKey)
    val blockCache            = new LRUCache(cacheCapacity, -1, false, highPriPoolRatio)
    val sstPartitionerFactory = new SstPartitionerFixedPrefixFactory(2)

    val options = new ColumnFamilyOptions()
      .setTableFormatConfig(
        new BlockBasedTableConfig()
          .setFilterPolicy(bloomFilter)
          .setOptimizeFiltersForMemory(true)
          .setCacheIndexAndFilterBlocks(true)
          .setPinL0FilterAndIndexBlocksInCache(true)
          .setFormatVersion(5)
          .setBlockSize(blockSize)
          .setChecksumType(ChecksumType.kNoChecksum)
          .setBlockCache(blockCache)
          .setCacheIndexAndFilterBlocksWithHighPriority(true)
          .setDataBlockIndexType(DataBlockIndexType.kDataBlockBinaryAndHash)
          .setDataBlockHashTableUtilRatio(0.5)
      )
      .setWriteBufferSize(writeBufferSize)
      .setLevelCompactionDynamicLevelBytes(true)
      .useCappedPrefixExtractor(10)
      .setMemtablePrefixBloomSizeRatio(0.25)
      .setCompressionType(CompressionType.LZ4_COMPRESSION)
      .setSstPartitionerFactory(sstPartitionerFactory)

    OptionsWithResources(options, Seq(options, bloomFilter, blockCache, sstPartitionerFactory))
  }

  private def createDbOptions(settings: DBSettings): OptionsWithResources[DBOptions] = {
    val dbOptions = new DBOptions()
      .setCreateIfMissing(true)
      .setParanoidChecks(true)
      .setIncreaseParallelism(4)
      .setBytesPerSync(2 << 20)
      .setMaxBackgroundJobs(4)
      .setCreateMissingColumnFamilies(true)
      .setMaxOpenFiles(100)

    if (settings.rocksdb.enableStatistics) {
      val statistics = new Statistics()
      OptionsWithResources(
        dbOptions.setStatistics(statistics),
        Seq(dbOptions, statistics)
      )
    } else OptionsWithResources(dbOptions, Seq(dbOptions))
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
