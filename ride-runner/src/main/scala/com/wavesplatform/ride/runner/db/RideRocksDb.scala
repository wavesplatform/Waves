package com.wavesplatform.ride.runner.db

import com.wavesplatform.ride.runner.stats.RideRunnerStats
import com.wavesplatform.ride.runner.stats.RideRunnerStats.dbStats
import com.wavesplatform.ride.runner.storage.persistent.KvPair
import com.wavesplatform.utils.*
import org.rocksdb.{ColumnFamilyHandle, RocksDB, Statistics, TickerType, *}

import java.io.File
import java.nio.file.Paths
import java.util
import scala.jdk.CollectionConverters.{CollectionHasAsScala, SeqHasAsJava}
import scala.util.Try

class RideRocksDb(
    db: RocksDB,
    stats: Statistics,
    handles: Seq[ColumnFamilyHandle]
) extends RideDb
    with ScorexLogging {

  override val access: RideDbAccess = RideDbAccess.fromRocksDb(db)

  override def sendStats(): Unit = {
    RocksDbProperties.All.foreach { propName =>
      Try {
        val propValue = db.getAggregatedLongProperty(propName)
        RideRunnerStats.dbStats.withTag("name", propName).update(propValue.toDouble)
      }

      handles.foreach { cfh =>
        val cfName = new String(cfh.getName)
        Try {
          val propValue = db.getLongProperty(cfh, propName)
          RideRunnerStats.columnFamilyProperties
            .withTag("cf", cfName)
            .withTag("name", propName)
            .update(propValue.toDouble)
        }
      }
    }

    TickerType.values().foreach { ticker =>
      val value = stats.getTickerCount(ticker)
      dbStats.withTag("name", ticker.toString).update(value.toDouble)
    }
  }

  override def close(): Unit = db.close()
}

// TODO #101 Settings (those won't break a DB)
object RideRocksDb extends ScorexLogging {
  case class Settings(directory: String)

  def open(settings: Settings): RideDb = {
    log.debug(s"Open DB at ${settings.directory}")
    val file = new File(settings.directory)
    val options = new DBOptions()
      // speeds up opening the DB
      .setSkipStatsUpdateOnDbOpen(true)
      // can be disabled when not profiling
      .setStatsDumpPeriodSec(30)
      .setCreateIfMissing(true)
      .setParanoidChecks(true)
      .setIncreaseParallelism(4)
      // .setBytesPerSync(2 << 20)
      .setMaxBackgroundJobs(4)
      .setCreateMissingColumnFamilies(true)
      .setWalSizeLimitMB(1 << 20) // 1MiB
      //      .setWriteBufferManager(new WriteBufferManager())
      .setMaxOpenFiles(50)

    // https://github.com/facebook/rocksdb/issues/9667#issuecomment-1060614090
    val stats = new Statistics()
    stats.setStatsLevel(StatsLevel.ALL)
    options.setStatistics(stats)

    val dbDir = file.getAbsoluteFile
    dbDir.getParentFile.mkdirs()

    val handles = new util.ArrayList[ColumnFamilyHandle]()
    val db = RocksDB.open(
      options,
      settings.directory,
      Seq(
        new ColumnFamilyDescriptor(
          RocksDB.DEFAULT_COLUMN_FAMILY,
          // TODO Settings
          newColumnFamilyOptions(12.0, 16 << 10, 256 << 20, 8 << 20, 0.6)
            .setCfPaths(Seq(new DbPath(Paths.get(settings.directory, "ride"), 0L)).asJava)
        )
      ).asJava,
      handles
    )

    new RideRocksDb(db, stats, handles.asScala.toList)
  }

  /** @param cacheCapacityInBytes
    *   Total size of the cache.
    * @param highPriPoolRatio
    *   The ratio of capacity reserved for high priority blocks (see Wiki of RocksDB)
    * @return
    */
  def newColumnFamilyOptions(
      bitsPerKey: Double,
      blockSize: Long,
      cacheCapacityInBytes: Long,
      writeBufferSizeInBytes: Long,
      highPriPoolRatio: Double
  ): ColumnFamilyOptions =
    new ColumnFamilyOptions()
      .setTableFormatConfig(
        new BlockBasedTableConfig()
          .setFilterPolicy(new BloomFilter(bitsPerKey))
          .setOptimizeFiltersForMemory(true)
          .setCacheIndexAndFilterBlocks(true)
          .setPinL0FilterAndIndexBlocksInCache(true)
          .setFormatVersion(5)
          .setBlockSize(blockSize)
          .setChecksumType(ChecksumType.kNoChecksum)
          .setBlockCache(new LRUCache(cacheCapacityInBytes, -1, false, highPriPoolRatio)) // TODO
          .setCacheIndexAndFilterBlocksWithHighPriority(true)
          .setDataBlockIndexType(DataBlockIndexType.kDataBlockBinaryAndHash)
          .setDataBlockHashTableUtilRatio(0.5)
      )
      .setWriteBufferSize(writeBufferSizeInBytes)
      .setLevelCompactionDynamicLevelBytes(true)
      .useFixedLengthPrefixExtractor(KvPair.PrefixSize)
      .setMemtablePrefixBloomSizeRatio(0.25)
      .setCompressionType(CompressionType.LZ4_COMPRESSION)
      .setSstPartitionerFactory(new SstPartitionerFixedPrefixFactory(2))
}
