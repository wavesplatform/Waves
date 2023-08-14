package com.wavesplatform.ride.runner.db

import com.typesafe.config.ConfigMemorySize
import com.wavesplatform.ride.runner.caches.disk.KvPair
import com.wavesplatform.ride.runner.db.RideRocksDb.RocksDbStatistics
import com.wavesplatform.ride.runner.stats.RideRunnerStats
import com.wavesplatform.ride.runner.stats.RideRunnerStats.dbStats
import com.wavesplatform.utils.*
import monix.execution.{Cancelable, Scheduler}
import org.rocksdb.{ColumnFamilyHandle, RocksDB, Statistics, TickerType, *}

import java.io.File
import java.nio.file.Paths
import java.util
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.{CollectionHasAsScala, SeqHasAsJava}
import scala.util.Try

class RideRocksDb(db: RocksDB, stats: Option[RocksDbStatistics]) extends RideDb {
  override val access: RideDbAccess = RideDbAccess.fromRocksDb(db)

  override def startCollectingStats(scheduler: Scheduler): Cancelable = stats.fold(Cancelable.empty) { stats =>
    scheduler.scheduleAtFixedRate(30.seconds, 30.seconds) {
      stats.send()
    }
  }

  override def close(): Unit = {
    stats.foreach(_.close())
    db.close()
  }
}

object RideRocksDb extends ScorexLogging {
  case class Settings(directory: String, enableStatistics: Boolean, defaultColumnFamily: ColumnFamilySettings) {
    // Increase when need to delete the database
    val version = 13
  }

  def open(settings: Settings): RideDb = {
    log.debug(s"Open DB at ${settings.directory}")
    val file = new File(settings.directory)

    // https://javadoc.io/doc/org.rocksdb/rocksdbjni/latest/org/rocksdb/DBOptions.html
    val options = new DBOptions()
      .setCreateIfMissing(true)
      .setParanoidChecks(true)
      .setIncreaseParallelism(4)
      .setBytesPerSync(2 << 20) // 2 MiB
      .setMaxBackgroundJobs(4)
      .setCreateMissingColumnFamilies(true)
      .setMaxOpenFiles(50)
      .setWalSizeLimitMB(1 << 20)       // 1 MiB
      .setSkipStatsUpdateOnDbOpen(true) // speeds up opening the DB

    val stats = if (settings.enableStatistics) {
      // https://github.com/facebook/rocksdb/issues/9667#issuecomment-1060614090
      val stats = new Statistics()
      stats.setStatsLevel(StatsLevel.ALL)
      options.setStatistics(stats)
      Some(stats)
    } else None

    val dbDir = file.getAbsoluteFile
    dbDir.getParentFile.mkdirs()

    val handles = new util.ArrayList[ColumnFamilyHandle]()
    val db = RocksDB.open(
      options,
      settings.directory,
      Seq(
        new ColumnFamilyDescriptor(
          RocksDB.DEFAULT_COLUMN_FAMILY,
          settings.defaultColumnFamily.mkColumnFamilyOptions.setCfPaths(Seq(new DbPath(Paths.get(settings.directory, "ride"), 0L)).asJava)
        )
      ).asJava,
      handles
    )

    new RideRocksDb(db, stats.map(new RocksDbStatistics(db, _, handles.asScala.toList)))
  }

  case class ColumnFamilySettings(
      bitsPerKey: Double,
      blockSize: ConfigMemorySize,
      cacheCapacity: ConfigMemorySize,
      writeBufferSize: ConfigMemorySize,
      highPriPoolRatio: Double
  ) {
    def mkColumnFamilyOptions: ColumnFamilyOptions =
      new ColumnFamilyOptions()
        .setTableFormatConfig(
          new BlockBasedTableConfig()
            .setFilterPolicy(new BloomFilter(bitsPerKey))
            .setOptimizeFiltersForMemory(true)
            .setCacheIndexAndFilterBlocks(true)
            .setPinL0FilterAndIndexBlocksInCache(true)
            .setFormatVersion(5)
            .setBlockSize(blockSize.toBytes)
            .setChecksumType(ChecksumType.kNoChecksum)
            .setBlockCache(
              new LRUCache( // https://github.com/facebook/rocksdb/blob/main/java/rocksjni/lru_cache.cc
                cacheCapacity.toBytes,
                -1,    // jnum_shard_bits
                false, // jstrict_capacity_limit
                highPriPoolRatio
              )
            )
            .setCacheIndexAndFilterBlocksWithHighPriority(true)
            .setDataBlockIndexType(DataBlockIndexType.kDataBlockBinaryAndHash)
            .setDataBlockHashTableUtilRatio(0.5)
        )
        .setWriteBufferSize(writeBufferSize.toBytes)
        .setLevelCompactionDynamicLevelBytes(true)
        .useFixedLengthPrefixExtractor(KvPair.PrefixSize)
        .setMemtablePrefixBloomSizeRatio(0.25)
        .setCompressionType(CompressionType.LZ4_COMPRESSION)
        .setSstPartitionerFactory(new SstPartitionerFixedPrefixFactory(2))
  }

  class RocksDbStatistics(db: RocksDB, stats: Statistics, handles: Seq[ColumnFamilyHandle]) extends AutoCloseable {
    def send(): Unit = {
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

    override def close(): Unit = stats.close()
  }
}
