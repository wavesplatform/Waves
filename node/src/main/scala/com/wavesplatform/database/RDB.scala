package com.wavesplatform.database

import com.typesafe.scalalogging.StrictLogging
import com.wavesplatform.database.RDB.{ApiHandle, ByHeightHandle, DataHistoryHandle, InvokeResultsHandle, TxHandle, TxMetaHandle, TxSnapshotHandle}
import com.wavesplatform.settings.{DBSettings, SizeInBytes}
import com.wavesplatform.utils.*
import org.rocksdb.*

import java.io.File
import java.nio.file.{Files, Path}
import java.util
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Using}

final class RDB(
    val db: RocksDB,
    val txMetaHandle: TxMetaHandle,
    val txHandle: TxHandle,
    val txSnapshotHandle: TxSnapshotHandle,
    val apiHandle: ApiHandle,
    val dataHistoryHandle: DataHistoryHandle,
    val invokeResultsHandle: InvokeResultsHandle,
    val byHeightHandle: ByHeightHandle,
    acquiredResources: Seq[RocksObject]
) extends AutoCloseable {
  def statistics: Option[Statistics] = acquiredResources.collectFirst { case s: Statistics => s }

  override def close(): Unit = {
    acquiredResources.foreach(_.close())
    db.close()
  }
}

object RDB extends StrictLogging {
  final class TxMetaHandle private[RDB] (val handle: ColumnFamilyHandle)
  final class TxHandle private[RDB] (val handle: ColumnFamilyHandle)
  final class TxSnapshotHandle private[RDB] (val handle: ColumnFamilyHandle)
  final class ApiHandle private[RDB] (val handle: ColumnFamilyHandle)
  final class DataHistoryHandle private[RDB] (val handle: ColumnFamilyHandle)
  final class InvokeResultsHandle private[RDB] (val handle: ColumnFamilyHandle)
  final class ByHeightHandle private[RDB] (val handle: ColumnFamilyHandle)

  val TxMetaCF        = "tx-meta"
  val TxCF            = "tx"
  val TxSnapshotCF    = "tx-snapshot"
  val ApiCF           = "api"
  val DataHistoryCF   = "data-history"
  val InvokeResultsCF = "invoke-results"
  // Keys written once per height, laid out height first (see Key.heightFirst) so that every flush appends past the existing data
  val ByHeightCF = "by-height"

  /** Column families that only contain data derived from the blockchain and can be truncated without losing the state */
  val NonEssentialColumnFamilies: Seq[String] = Seq(TxSnapshotCF, ApiCF, InvokeResultsCF)

  case class OptionsWithResources[A](options: A, resources: Seq[RocksObject])

  def open(settings: DBSettings): RDB = {
    val file = new File(settings.directory)
    checkDbDir(file.toPath)
    logger.debug(s"Open DB at ${settings.directory}")

    val dbOptions = createDbOptions(settings)
    val dbDir     = file.getAbsoluteFile
    dbDir.getParentFile.mkdirs()

    val handles = new util.ArrayList[ColumnFamilyHandle]()
    // Split out of default and api, so they share those caches and the memory budget stays the same
    val mainCache       = new LRUCache(settings.rocksdb.mainCacheSize.longValue, -1, false, 0.6)
    val apiCache        = newCache(settings.rocksdb.apiCacheSize)
    val txMetaCache     = newCache(settings.rocksdb.txMetaCacheSize)
    val txCache         = newCache(settings.rocksdb.txCacheSize)
    val txSnapshotCache = newCache(settings.rocksdb.txSnapshotCacheSize)
    val defaultCfOptions =
      newColumnFamilyOptions(12.0, 16 << 10, mainCache, settings.rocksdb.writeBufferSize)
    val txMetaCfOptions =
      newColumnFamilyOptions(10.0, 2 << 10, txMetaCache, settings.rocksdb.writeBufferSize)
    val txCfOptions = newColumnFamilyOptions(10.0, 2 << 10, txCache, settings.rocksdb.writeBufferSize)
    val txSnapshotCfOptions =
      newColumnFamilyOptions(10.0, 2 << 10, txSnapshotCache, settings.rocksdb.writeBufferSize)
    val apiCfOptions           = newColumnFamilyOptions(10.0, 2 << 10, apiCache, settings.rocksdb.writeBufferSize)
    val dataHistoryCfOptions   = newColumnFamilyOptions(12.0, 16 << 10, mainCache, settings.rocksdb.writeBufferSize)
    val invokeResultsCfOptions = newColumnFamilyOptions(10.0, 2 << 10, apiCache, settings.rocksdb.writeBufferSize)
    val byHeightCfOptions      = newColumnFamilyOptions(10.0, 4 << 10, mainCache, settings.rocksdb.writeBufferSize, tagFirst = false)
    val db = RocksDB.open(
      dbOptions.options,
      settings.directory,
      Seq(
        new ColumnFamilyDescriptor(
          RocksDB.DEFAULT_COLUMN_FAMILY,
          defaultCfOptions.options
            .setMaxWriteBufferNumber(3)
            .setCfPaths(Seq(new DbPath(new File(dbDir, "default").toPath, 0L)).asJava)
        ),
        new ColumnFamilyDescriptor(
          TxMetaCF.utf8Bytes,
          txMetaCfOptions.options
            .optimizeForPointLookup(16 << 20) // Iterators might not work with this option
            .setDisableAutoCompactions(true)
            .setCfPaths(Seq(new DbPath(new File(dbDir, TxMetaCF).toPath, 0L)).asJava)
        ),
        new ColumnFamilyDescriptor(
          TxCF.utf8Bytes,
          txCfOptions.options
            .setCfPaths(Seq(new DbPath(new File(dbDir, TxCF).toPath, 0L)).asJava)
        ),
        new ColumnFamilyDescriptor(
          TxSnapshotCF.utf8Bytes,
          txSnapshotCfOptions.options
            .setCfPaths(Seq(new DbPath(new File(dbDir, TxSnapshotCF).toPath, 0L)).asJava)
        ),
        new ColumnFamilyDescriptor(
          ApiCF.utf8Bytes,
          apiCfOptions.options
            .setCfPaths(Seq(new DbPath(new File(dbDir, ApiCF).toPath, 0L)).asJava)
        ),
        new ColumnFamilyDescriptor(
          DataHistoryCF.utf8Bytes,
          dataHistoryCfOptions.options
            .setCfPaths(Seq(new DbPath(new File(dbDir, DataHistoryCF).toPath, 0L)).asJava)
        ),
        new ColumnFamilyDescriptor(
          InvokeResultsCF.utf8Bytes,
          invokeResultsCfOptions.options
            .setCfPaths(Seq(new DbPath(new File(dbDir, InvokeResultsCF).toPath, 0L)).asJava)
        ),
        new ColumnFamilyDescriptor(
          ByHeightCF.utf8Bytes,
          byHeightCfOptions.options
            .setCfPaths(Seq(new DbPath(new File(dbDir, ByHeightCF).toPath, 0L)).asJava)
        )
      ).asJava,
      handles
    )

    new RDB(
      db,
      new TxMetaHandle(handles.get(1)),
      new TxHandle(handles.get(2)),
      new TxSnapshotHandle(handles.get(3)),
      new ApiHandle(handles.get(4)),
      new DataHistoryHandle(handles.get(5)),
      new InvokeResultsHandle(handles.get(6)),
      new ByHeightHandle(handles.get(7)),
      dbOptions.resources ++ defaultCfOptions.resources ++ txMetaCfOptions.resources ++ txCfOptions.resources ++
        txSnapshotCfOptions.resources ++ apiCfOptions.resources ++ dataHistoryCfOptions.resources ++ invokeResultsCfOptions.resources ++
        byHeightCfOptions.resources ++ Seq(mainCache, apiCache, txMetaCache, txCache, txSnapshotCache)
    )
  }

  /** Removes all the data from a non-essential column family by dropping it. It is re-created on the next [[open]], because the DB is opened
    * with `createMissingColumnFamilies`.
    */
  def truncateColumnFamily(settings: DBSettings, cfName: String): Unit = {
    val handleOf: RDB => ColumnFamilyHandle = cfName match {
      case TxSnapshotCF    => _.txSnapshotHandle.handle
      case ApiCF           => _.apiHandle.handle
      case InvokeResultsCF => _.invokeResultsHandle.handle
      case _               => throw new IllegalArgumentException(s"$cfName is not one of ${NonEssentialColumnFamilies.mkString(", ")}")
    }

    Using.resource(openExisting(settings)) { rdb =>
      logger.info(s"Truncating $cfName")
      rdb.db.dropColumnFamily(handleOf(rdb))
    }
  }

  /** Auto compactions are disabled for tx-meta, so every flush adds an L0 file and a point lookup may probe all of them. A manual full compaction
    * merges them into the bottom level.
    */
  def compactTxMeta(settings: DBSettings): Unit =
    Using.resources(openExisting(settings), new CompactRangeOptions().setMaxSubcompactions(Runtime.getRuntime.availableProcessors())) {
      (rdb, options) =>
        val handle = rdb.txMetaHandle.handle
        // defs, not vals: re-read after compaction
        def l0Files  = rdb.db.getProperty(handle, "rocksdb.num-files-at-level0")
        def sstBytes = rdb.db.getProperty(handle, "rocksdb.total-sst-files-size")
        logger.info(s"Compacting $TxMetaCF, $l0Files L0 files, $sstBytes bytes")
        rdb.db.compactRange(handle, null, null, options)
        logger.info(s"Compacted $TxMetaCF, $l0Files L0 files, $sstBytes bytes")
    }

  // open creates a missing DB, so a wrong config would make an offline tool silently work on a fresh empty one
  private def openExisting(settings: DBSettings): RDB =
    if (Files.exists(Path.of(settings.directory, "CURRENT"))) open(settings)
    else throw new IllegalStateException(s"No database at ${settings.directory}")

  private def newCache(capacity: SizeInBytes): LRUCache = new LRUCache(capacity.longValue, -1, false, 0.9)

  // The caller closes blockCache: it may be shared between column families
  private def newColumnFamilyOptions(
      bitsPerKey: Double,
      blockSize: Long,
      blockCache: Cache,
      writeBufferSize: SizeInBytes,
      tagFirst: Boolean = true
  ): OptionsWithResources[ColumnFamilyOptions] = {
    val bloomFilter = new BloomFilter(bitsPerKey)
    // Cuts compaction output at the 2-byte key tag; meaningless when the key starts with a height instead
    val sstPartitionerFactory = Option.when(tagFirst)(new SstPartitionerFixedPrefixFactory(2))

    val options = new ColumnFamilyOptions()
      .setTableFormatConfig(
        new BlockBasedTableConfig()
          .setFilterPolicy(bloomFilter)
          .setOptimizeFiltersForMemory(true)
          .setCacheIndexAndFilterBlocks(true)
          .setPinL0FilterAndIndexBlocksInCache(true)
          .setFormatVersion(5)
          .setBlockSize(blockSize)
          .setBlockCache(blockCache)
          .setCacheIndexAndFilterBlocksWithHighPriority(true)
          // A full-file filter is megabytes, more than the cache can hold for every file, so each miss reloaded all of it.
          // Partitions let a lookup load only the few kilobytes it needs.
          .setIndexType(IndexType.kTwoLevelIndexSearch)
          .setPartitionFilters(true)
          .setDataBlockIndexType(DataBlockIndexType.kDataBlockBinaryAndHash)
          .setDataBlockHashTableUtilRatio(0.5)
      )
      .setWriteBufferSize(writeBufferSize.longValue)
      .setCompactionStyle(CompactionStyle.LEVEL)
      .setLevelCompactionDynamicLevelBytes(true)
      // Defines the prefix.
      // Improves an iterator performance for keys with prefixes of 10 or more bytes.
      // If specified key has less than 10 bytes: iterator finds the exact key for seek(key) and becomes invalid after next().
      .useCappedPrefixExtractor(10)
      .setMemtablePrefixBloomSizeRatio(0.25)
      .setCompressionType(CompressionType.LZ4_COMPRESSION)
    sstPartitionerFactory.foreach(options.setSstPartitionerFactory)

    OptionsWithResources(options, Seq(options, bloomFilter) ++ sstPartitionerFactory)
  }

  private def createDbOptions(settings: DBSettings): OptionsWithResources[DBOptions] = {
    val dbOptions = new DBOptions()
      .setCreateIfMissing(true)
      .setIncreaseParallelism(settings.rocksdb.parallelism)
      .setBytesPerSync(2 << 20)
      .setCreateMissingColumnFamilies(true)
      .setMaxOpenFiles(settings.rocksdb.maxOpenFiles)
      .setMaxSubcompactions(2) // Write stalls expected without this option. Can lead to max_background_jobs * max_subcompactions background threads
      .setMaxManifestFileSize(200 << 20)
      .setAllowMmapReads(settings.rocksdb.allowMmapReads)

    if (settings.rocksdb.enableStatistics) {
      val statistics = new Statistics()
      OptionsWithResources(
        dbOptions.setStatistics(statistics),
        Seq(dbOptions, statistics)
      )
    } else OptionsWithResources(dbOptions, Seq(dbOptions))
  }

  private def checkDbDir(dbPath: Path): Unit =
    if (Files.exists(dbPath)) {
      Using(Files.list(dbPath)) { fileList =>
        fileList.iterator().asScala.exists(_.getFileName.toString.endsWith(".ldb"))
      } match {
        case Failure(exception) =>
          logger.error(s"Could not open data directory ${dbPath.toAbsolutePath.toString}", exception)
          forceStopApplication(FatalDBError)
        case Success(true) =>
          logger.error(
            s"Database directory ${dbPath.toAbsolutePath.toString} contains LevelDB files (.ldb) which is not compatible with current database. Please delete these files and restart node"
          )
          logger.error("FOR THIS REASON THE NODE STOPPED AUTOMATICALLY")
          forceStopApplication(FatalDBError)
        case _ =>
      }
    }
}
