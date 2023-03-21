package com.wavesplatform.ride.runner.db

import com.wavesplatform.ride.runner.stats.RideRunnerStats.{columnFamilyProperties, dbStats}
import com.wavesplatform.utils.*
import org.rocksdb.{ColumnFamilyHandle, RocksDB, Statistics, TickerType, *}

import java.io.File
import java.util
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.{CollectionHasAsScala, SeqHasAsJava}
import scala.util.Try

class RideDb(
    val db: RocksDB,
    private val stats: Statistics,
    private val handles: Seq[ColumnFamilyHandle]
) {
  def sendStats(): Unit = {
    RocksDbProperties.All.foreach { propName =>
      Try {
        val propValue = db.getAggregatedLongProperty(propName)
        dbStats.withTag("name", propName).update(propValue.toDouble)
      }

      handles.foreach { cfh =>
        val cfName = new String(cfh.getName)
        Try {
          val propValue = db.getLongProperty(cfh, propName)
          columnFamilyProperties
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

}

// TODO #101 Settings (those won't break a DB)
object RideDb extends ScorexLogging {
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
      // .setIncreaseParallelism(4)
      // .setBytesPerSync(2 << 20)
      .setMaxBackgroundJobs(4)
      .setCreateMissingColumnFamilies(true)
//      .setWriteBufferManager(new WriteBufferManager())
      // may not be necessary when WAL is disabled, but nevertheless recommended to avoid
      // many small SST files
      .setAvoidFlushDuringRecovery(true)
      // limit the size of the manifest (logs all operations), otherwise it will grow unbounded
      .setMaxManifestFileSize(32 * 1024 * 1024L)
      .setIncreaseParallelism(4)
      .setMaxOpenFiles(50)
      // keep 1 hour of logs - completely arbitrary. we should keep what we think would be
      // a good balance between useful for performance and small for replication
      .setLogFileTimeToRoll(30.minutes.toSeconds)
      .setKeepLogFileNum(2)

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
            .setCfPaths(Seq(new DbPath(new File(dbDir, "ride").toPath, 0L)).asJava)
        )
      ).asJava,
      handles
    )

    new RideDb(db, stats, handles.asScala.toList)
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
      .useCappedPrefixExtractor(10)
      .setMemtablePrefixBloomSizeRatio(0.25)
      .setCompressionType(CompressionType.LZ4_COMPRESSION)
      .setSstPartitionerFactory(new SstPartitionerFixedPrefixFactory(2))
}
