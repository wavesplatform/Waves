package com.wavesplatform.metrics

import com.jprofiler.api.probe.embedded.telemetry.{Telemetry, TelemetryFormat, TelemetryUnit}
import com.jprofiler.api.probe.embedded.{Payload, PayloadProbe}
import com.wavesplatform.database.RDB
import com.google.common.cache.CacheStats
import org.rocksdb.{ColumnFamilyHandle, HistogramType, RocksDB, Statistics, TickerType}

import scala.annotation.static

/** JProfiler probe for the importer: block append time per height range as payloads, import throughput and RocksDB statistics as
  * telemetries. The API is a no-op without the agent, and the telemetries are computed only when the agent polls them.
  */
final class ImportProbe extends PayloadProbe {
  override def getName: String        = "Import"
  override def getDescription: String = "Block append time by height range, import throughput and RocksDB statistics"
  override def isEvents: Boolean      = false
}

object ImportProbe {
  import TelemetryUnit.*

  private val HeightRange = 50_000
  // The agent polls every telemetry once per second, one after another; one sample serves them all.
  private val SampleNanos = 500_000_000L

  private enum Rate(val read: Statistics => Long) {
    case KeysWritten       extends Rate(_.getTickerCount(TickerType.NUMBER_KEYS_WRITTEN))
    case TableOpens        extends Rate(_.getTickerCount(TickerType.NO_FILE_OPENS))
    case DataHit           extends Rate(_.getTickerCount(TickerType.BLOCK_CACHE_DATA_HIT))
    case DataMiss          extends Rate(_.getTickerCount(TickerType.BLOCK_CACHE_DATA_MISS))
    case IndexHit          extends Rate(_.getTickerCount(TickerType.BLOCK_CACHE_INDEX_HIT))
    case IndexMiss         extends Rate(_.getTickerCount(TickerType.BLOCK_CACHE_INDEX_MISS))
    case FilterHit         extends Rate(_.getTickerCount(TickerType.BLOCK_CACHE_FILTER_HIT))
    case FilterMiss        extends Rate(_.getTickerCount(TickerType.BLOCK_CACHE_FILTER_MISS))
    case BytesWritten      extends Rate(_.getTickerCount(TickerType.BYTES_WRITTEN))
    case BytesRead         extends Rate(_.getTickerCount(TickerType.BYTES_READ))
    case WalBytes          extends Rate(_.getTickerCount(TickerType.WAL_FILE_BYTES))
    case FlushBytes        extends Rate(_.getTickerCount(TickerType.FLUSH_WRITE_BYTES))
    case CompactReadBytes  extends Rate(_.getTickerCount(TickerType.COMPACT_READ_BYTES))
    case CompactWriteBytes extends Rate(_.getTickerCount(TickerType.COMPACT_WRITE_BYTES))
    case StallMicros       extends Rate(_.getTickerCount(TickerType.STALL_MICROS))
    case MemtableHit       extends Rate(_.getTickerCount(TickerType.MEMTABLE_HIT))
    case HitL0             extends Rate(_.getTickerCount(TickerType.GET_HIT_L0))
    case HitL1             extends Rate(_.getTickerCount(TickerType.GET_HIT_L1))
    case HitL2AndUp        extends Rate(_.getTickerCount(TickerType.GET_HIT_L2_AND_UP))
    case FilterUseful      extends Rate(_.getTickerCount(TickerType.BLOOM_FILTER_USEFUL))
    case GetMicros         extends Rate(_.getHistogramData(HistogramType.DB_GET).getSum)
    case MultiGetMicros    extends Rate(_.getHistogramData(HistogramType.DB_MULTIGET).getSum)
    case WriteMicros       extends Rate(_.getHistogramData(HistogramType.DB_WRITE).getSum)
    case SeekMicros        extends Rate(_.getHistogramData(HistogramType.DB_SEEK).getSum)
    case SstReadMicros     extends Rate(_.getHistogramData(HistogramType.SST_READ_MICROS).getSum)
    case TableOpenMicros   extends Rate(_.getHistogramData(HistogramType.TABLE_OPEN_IO_MICROS).getSum)
  }

  private enum CfGauge(val read: (RocksDB, ColumnFamilyHandle) => Long) {
    // A string property: getLongProperty only serves the integer ones and throws NotFound for it.
    case L0Files           extends CfGauge(_.getProperty(_, "rocksdb.num-files-at-level0").toLong)
    case PendingCompaction extends CfGauge(_.getLongProperty(_, "rocksdb.estimate-pending-compaction-bytes"))
    case BlockCacheUsage   extends CfGauge(_.getLongProperty(_, "rocksdb.block-cache-usage"))
    case Memtables         extends CfGauge(_.getLongProperty(_, "rocksdb.cur-size-all-mem-tables"))
  }

  // In the order of RDB's column families: default, tx-meta, tx, tx-snapshot, api.
  private final class Source(val db: RocksDB, val statistics: Option[Statistics], val cfs: IndexedSeq[ColumnFamilyHandle])

  // Written by the importer's thread only.
  @volatile private var height = 0
  @volatile private var blocks = 0L
  @volatile private var txs    = 0L
  // Written by the appender thread only.
  @volatile private var complexity = 0L
  private var rangeStart           = -1
  private var rangeLabel           = ""

  private final case class CacheRates(hitRate: Double, misses: Double, evictions: Double)

  // Guarded by this object's lock.
  private var cacheSource: () => Seq[(String, CacheStats)] = () => Nil
  private var lastCacheStats                               = Map.empty[String, CacheStats]
  private var cacheRates                                   = Map.empty[String, CacheRates]
  private var source: Option[Source]                       = None
  private var lastNanos                                    = System.nanoTime()
  private var lastComplexity                               = 0L
  private var complexityRate                               = 0.0
  private var lastBlocks, lastTxs                          = 0L
  private var blockRate, txRate                            = 0.0
  private val lastRates                                    = new Array[Long](Rate.values.length)
  private val rates                                        = new Array[Double](Rate.values.length)
  private val cfGauges                                     = Array.ofDim[Long](CfGauge.values.length, 5)
  private var compactions, flushes                         = 0L

  def appendBlock[A](height: Int, txCount: Int)(append: => A): A = {
    Payload.enter(classOf[ImportProbe])
    try append
    finally {
      Payload.exit(label(height))
      this.height = height
      blocks += 1
      txs += txCount
    }
  }

  /** Script complexity spent by the block just appended. */
  def blockApplied(scriptsComplexity: Long): Unit = complexity += scriptsComplexity

  private def label(height: Int): String = {
    val start = height / HeightRange * HeightRange
    if (start != rangeStart) {
      rangeStart = start
      rangeLabel = s"$start-${start + HeightRange - 1}"
    }
    rangeLabel
  }

  def watch(rdb: RDB, caches: () => Seq[(String, CacheStats)]): Unit = synchronized {
    cacheSource = caches
    lastCacheStats = caches().toMap
    val s = Source(
      rdb.db,
      rdb.statistics,
      IndexedSeq(rdb.db.getDefaultColumnFamily, rdb.txMetaHandle.handle, rdb.txHandle.handle, rdb.txSnapshotHandle.handle, rdb.apiHandle.handle)
    )
    s.statistics.foreach(st => Rate.values.foreach(r => lastRates(r.ordinal) = r.read(st)))
    source = Some(s)
  }

  /** Call before closing the database: a telemetry poll must not reach a closed RocksDB handle. */
  def unwatch(): Unit = synchronized {
    source = None
    cacheSource = () => Nil
  }

  private def sample(): Unit = synchronized {
    val now = System.nanoTime()
    if (now - lastNanos >= SampleNanos) {
      val seconds = (now - lastNanos) / 1e9
      val b       = blocks
      val t       = txs
      blockRate = (b - lastBlocks) / seconds
      txRate = (t - lastTxs) / seconds
      lastBlocks = b
      lastTxs = t
      val c = complexity
      complexityRate = (c - lastComplexity) / seconds
      lastComplexity = c
      val stats = cacheSource().toMap
      cacheRates = stats.map { case (name, st) =>
        val d     = lastCacheStats.get(name).fold(st)(st.minus)
        val total = d.requestCount
        name -> CacheRates(if (total == 0) 0 else d.hitCount * 100.0 / total, d.missCount / seconds, d.evictionCount / seconds)
      }
      lastCacheStats = stats
      source.foreach { s =>
        s.statistics.foreach { st =>
          Rate.values.foreach { r =>
            val v = r.read(st)
            rates(r.ordinal) = (v - lastRates(r.ordinal)) / seconds
            lastRates(r.ordinal) = v
          }
        }
        for (g <- CfGauge.values; (cf, i) <- s.cfs.zipWithIndex) cfGauges(g.ordinal)(i) = g.read(s.db, cf)
        compactions = s.db.getLongProperty("rocksdb.num-running-compactions")
        flushes = s.db.getLongProperty("rocksdb.num-running-flushes")
      }
      lastNanos = now
    }
  }

  // Probe telemetries are 32-bit ints, and the UI ignored TelemetryFormat.scale (BYTES lines read 0 MB), so no line uses it:
  // byte rates are plain bytes (under 2 GB/s) and gauges that reach gigabytes are plain MB.
  private def int(v: Double): Int = math.min(v, Int.MaxValue.toDouble).toInt

  private def rate(r: Rate): Int      = { sample(); int(rates(r.ordinal)) }
  private def bytesRate(r: Rate): Int = { sample(); int(rates(r.ordinal)) }
  // Microseconds spent per second.
  private def timeShare(r: Rate): Int = { sample(); int(rates(r.ordinal)) }
  private def hitRate(hit: Rate, miss: Rate): Int = {
    sample()
    val total = rates(hit.ordinal) + rates(miss.ordinal)
    if (total == 0) 0 else int(rates(hit.ordinal) * 100 / total)
  }
  private def cacheHitRate(name: String): Int                       = { sample(); cacheRates.get(name).fold(0)(r => int(r.hitRate)) }
  private def cacheRate(name: String, f: CacheRates => Double): Int = { sample(); cacheRates.get(name).fold(0)(r => int(f(r))) }
  private def cf(g: CfGauge, i: Int): Int                           = { sample(); int(cfGauges(g.ordinal)(i).toDouble) }
  private def cfMb(g: CfGauge, i: Int): Int                         = { sample(); int(cfGauges(g.ordinal)(i) / 1e6) }
  private def cfTotalMb(g: CfGauge): Int                            = { sample(); int(cfGauges(g.ordinal).sum / 1e6) }

  @static @Telemetry("Import height") def importHeight(): Int                                                         = height
  @static @Telemetry(value = "Import blocks", format = new TelemetryFormat(PER_SECOND)) def blocksPerSecond(): Int    = { sample(); int(blockRate) }
  @static @Telemetry(value = "Import transactions", format = new TelemetryFormat(PER_SECOND)) def txsPerSecond(): Int = { sample(); int(txRate) }

  @static @Telemetry(value = "Script complexity", format = new TelemetryFormat(PER_SECOND)) def complexityPerSecond(): Int = {
    sample(); int(complexityRate)
  }

  @static @Telemetry(value = "RocksDB keys written", format = new TelemetryFormat(PER_SECOND)) def keysWritten(): Int = rate(Rate.KeysWritten)
  @static @Telemetry(value = "RocksDB table opens", format = new TelemetryFormat(PER_SECOND)) def tableOpens(): Int   = rate(Rate.TableOpens)

  private final val Time = "RocksDB time per second (1 s = one busy thread)"
  @static @Telemetry(value = Time, line = "get", format = new TelemetryFormat(MICROSECONDS)) def getTime(): Int = timeShare(
    Rate.GetMicros
  )
  @static @Telemetry(value = Time, line = "multiGet", format = new TelemetryFormat(MICROSECONDS)) def multiGetTime(): Int = timeShare(
    Rate.MultiGetMicros
  )
  @static @Telemetry(value = Time, line = "write", format = new TelemetryFormat(MICROSECONDS)) def writeTime(): Int = timeShare(
    Rate.WriteMicros
  )
  @static @Telemetry(value = Time, line = "seek", format = new TelemetryFormat(MICROSECONDS)) def seekTime(): Int = timeShare(
    Rate.SeekMicros
  )
  @static @Telemetry(value = Time, line = "stall", format = new TelemetryFormat(MICROSECONDS)) def stallTime(): Int = timeShare(
    Rate.StallMicros
  )
  @static @Telemetry(value = Time, line = "SST read, all threads", format = new TelemetryFormat(MICROSECONDS)) def sstReadTime(): Int =
    timeShare(Rate.SstReadMicros)
  @static @Telemetry(value = Time, line = "table open IO, all threads", format = new TelemetryFormat(MICROSECONDS)) def tableOpenTime(): Int =
    timeShare(Rate.TableOpenMicros)

  private final val Hits = "RocksDB block cache hit rate"
  @static @Telemetry(value = Hits, line = "data", format = new TelemetryFormat(PERCENT)) def dataHits(): Int =
    hitRate(Rate.DataHit, Rate.DataMiss)
  @static @Telemetry(value = Hits, line = "index", format = new TelemetryFormat(PERCENT)) def indexHits(): Int =
    hitRate(Rate.IndexHit, Rate.IndexMiss)
  @static @Telemetry(value = Hits, line = "filter", format = new TelemetryFormat(PERCENT)) def filterHits(): Int =
    hitRate(Rate.FilterHit, Rate.FilterMiss)

  private final val Gets                                                                                                = "RocksDB gets served by"
  @static @Telemetry(value = Gets, line = "memtable", format = new TelemetryFormat(PER_SECOND)) def getsMemtable(): Int = rate(Rate.MemtableHit)
  @static @Telemetry(value = Gets, line = "L0", format = new TelemetryFormat(PER_SECOND)) def getsL0(): Int             = rate(Rate.HitL0)
  @static @Telemetry(value = Gets, line = "L1", format = new TelemetryFormat(PER_SECOND)) def getsL1(): Int             = rate(Rate.HitL1)
  @static @Telemetry(value = Gets, line = "L2 and up", format = new TelemetryFormat(PER_SECOND)) def getsL2(): Int      = rate(Rate.HitL2AndUp)
  @static @Telemetry(value = Gets, line = "skipped by bloom filter", format = new TelemetryFormat(PER_SECOND)) def filterUseful(): Int = rate(
    Rate.FilterUseful
  )

  private final val Io                                                                                            = "RocksDB IO per second"
  @static @Telemetry(value = Io, line = "written", format = new TelemetryFormat(BYTES)) def bytesWritten(): Int   = bytesRate(Rate.BytesWritten)
  @static @Telemetry(value = Io, line = "read by gets", format = new TelemetryFormat(BYTES)) def bytesRead(): Int = bytesRate(Rate.BytesRead)
  @static @Telemetry(value = Io, line = "WAL", format = new TelemetryFormat(BYTES)) def walBytes(): Int           = bytesRate(Rate.WalBytes)
  @static @Telemetry(value = Io, line = "flush", format = new TelemetryFormat(BYTES)) def flushBytes(): Int       = bytesRate(Rate.FlushBytes)
  @static @Telemetry(value = Io, line = "compaction read", format = new TelemetryFormat(BYTES)) def compactReadBytes(): Int =
    bytesRate(Rate.CompactReadBytes)
  @static @Telemetry(value = Io, line = "compaction write", format = new TelemetryFormat(BYTES)) def compactWriteBytes(): Int =
    bytesRate(Rate.CompactWriteBytes)

  private final val L0                                                         = "RocksDB L0 files"
  @static @Telemetry(value = L0, line = "default") def l0Default(): Int        = cf(CfGauge.L0Files, 0)
  @static @Telemetry(value = L0, line = "tx-meta") def l0TxMeta(): Int         = cf(CfGauge.L0Files, 1)
  @static @Telemetry(value = L0, line = "tx") def l0Tx(): Int                  = cf(CfGauge.L0Files, 2)
  @static @Telemetry(value = L0, line = "tx-snapshot") def l0TxSnapshot(): Int = cf(CfGauge.L0Files, 3)
  @static @Telemetry(value = L0, line = "api") def l0Api(): Int                = cf(CfGauge.L0Files, 4)

  private final val Pending = "RocksDB pending compaction, MB"
  @static @Telemetry(value = Pending, line = "default") def pendingDefault(): Int =
    cfMb(CfGauge.PendingCompaction, 0)
  @static @Telemetry(value = Pending, line = "tx-meta") def pendingTxMeta(): Int =
    cfMb(CfGauge.PendingCompaction, 1)
  @static @Telemetry(value = Pending, line = "tx") def pendingTx(): Int =
    cfMb(CfGauge.PendingCompaction, 2)
  @static @Telemetry(value = Pending, line = "tx-snapshot") def pendingTxSnapshot(): Int =
    cfMb(CfGauge.PendingCompaction, 3)
  @static @Telemetry(value = Pending, line = "api") def pendingApi(): Int =
    cfMb(CfGauge.PendingCompaction, 4)

  private final val Jobs                                                               = "RocksDB background jobs"
  @static @Telemetry(value = Jobs, line = "compactions") def runningCompactions(): Int = { sample(); compactions.toInt }
  @static @Telemetry(value = Jobs, line = "flushes") def runningFlushes(): Int         = { sample(); flushes.toInt }

  private final val Memory = "RocksDB memory, MB"
  @static @Telemetry(value = Memory, line = "block caches") def blockCacheUsage(): Int =
    cfTotalMb(CfGauge.BlockCacheUsage)
  @static @Telemetry(value = Memory, line = "memtables") def memtables(): Int = cfTotalMb(CfGauge.Memtables)

  private final val CacheHits = "State cache hit rate"
  @static @Telemetry(value = CacheHits, line = "balance", format = new TelemetryFormat(PERCENT)) def cacheHitsBalance(): Int =
    cacheHitRate("balance")
  @static @Telemetry(value = CacheHits, line = "leaseBalance", format = new TelemetryFormat(PERCENT)) def cacheHitsLeaseBalance(): Int = cacheHitRate(
    "leaseBalance"
  )
  @static @Telemetry(
    value = CacheHits,
    line = "assetDescription",
    format = new TelemetryFormat(PERCENT)
  ) def cacheHitsAssetDescription(): Int = cacheHitRate("assetDescription")
  @static @Telemetry(value = CacheHits, line = "volumeAndFee", format = new TelemetryFormat(PERCENT)) def cacheHitsVolumeAndFee(): Int = cacheHitRate(
    "volumeAndFee"
  )
  @static @Telemetry(value = CacheHits, line = "accountScript", format = new TelemetryFormat(PERCENT)) def cacheHitsAccountScript(): Int =
    cacheHitRate("accountScript")
  @static @Telemetry(value = CacheHits, line = "assetScript", format = new TelemetryFormat(PERCENT)) def cacheHitsAssetScript(): Int = cacheHitRate(
    "assetScript"
  )
  @static @Telemetry(value = CacheHits, line = "addressId", format = new TelemetryFormat(PERCENT)) def cacheHitsAddressId(): Int =
    cacheHitRate("addressId")
  @static @Telemetry(value = CacheHits, line = "accountData", format = new TelemetryFormat(PERCENT)) def cacheHitsAccountData(): Int = cacheHitRate(
    "accountData"
  )
  @static @Telemetry(value = CacheHits, line = "alias", format = new TelemetryFormat(PERCENT)) def cacheHitsAlias(): Int =
    cacheHitRate("alias")
  @static @Telemetry(value = CacheHits, line = "blockHeight", format = new TelemetryFormat(PERCENT)) def cacheHitsBlockHeight(): Int = cacheHitRate(
    "blockHeight"
  )
  @static @Telemetry(
    value = CacheHits,
    line = "balanceAtHeight",
    format = new TelemetryFormat(PERCENT)
  ) def cacheHitsBalanceAtHeight(): Int = cacheHitRate("balanceAtHeight")
  @static @Telemetry(
    value = CacheHits,
    line = "leaseBalanceAtHeight",
    format = new TelemetryFormat(PERCENT)
  ) def cacheHitsLeaseBalanceAtHeight(): Int = cacheHitRate("leaseBalanceAtHeight")

  private final val CacheMisses = "State cache misses"
  @static @Telemetry(value = CacheMisses, line = "balance", format = new TelemetryFormat(PER_SECOND)) def cacheMissesBalance(): Int =
    cacheRate("balance", _.misses)
  @static @Telemetry(value = CacheMisses, line = "leaseBalance", format = new TelemetryFormat(PER_SECOND)) def cacheMissesLeaseBalance(): Int =
    cacheRate("leaseBalance", _.misses)
  @static @Telemetry(value = CacheMisses, line = "assetDescription", format = new TelemetryFormat(PER_SECOND)) def cacheMissesAssetDescription()
      : Int = cacheRate("assetDescription", _.misses)
  @static @Telemetry(value = CacheMisses, line = "volumeAndFee", format = new TelemetryFormat(PER_SECOND)) def cacheMissesVolumeAndFee(): Int =
    cacheRate("volumeAndFee", _.misses)
  @static @Telemetry(value = CacheMisses, line = "accountScript", format = new TelemetryFormat(PER_SECOND)) def cacheMissesAccountScript(): Int =
    cacheRate("accountScript", _.misses)
  @static @Telemetry(value = CacheMisses, line = "assetScript", format = new TelemetryFormat(PER_SECOND)) def cacheMissesAssetScript(): Int =
    cacheRate("assetScript", _.misses)
  @static @Telemetry(value = CacheMisses, line = "addressId", format = new TelemetryFormat(PER_SECOND)) def cacheMissesAddressId(): Int =
    cacheRate("addressId", _.misses)
  @static @Telemetry(value = CacheMisses, line = "accountData", format = new TelemetryFormat(PER_SECOND)) def cacheMissesAccountData(): Int =
    cacheRate("accountData", _.misses)
  @static @Telemetry(value = CacheMisses, line = "alias", format = new TelemetryFormat(PER_SECOND)) def cacheMissesAlias(): Int =
    cacheRate("alias", _.misses)
  @static @Telemetry(value = CacheMisses, line = "blockHeight", format = new TelemetryFormat(PER_SECOND)) def cacheMissesBlockHeight(): Int =
    cacheRate("blockHeight", _.misses)
  @static @Telemetry(value = CacheMisses, line = "balanceAtHeight", format = new TelemetryFormat(PER_SECOND)) def cacheMissesBalanceAtHeight(): Int =
    cacheRate("balanceAtHeight", _.misses)
  @static @Telemetry(
    value = CacheMisses,
    line = "leaseBalanceAtHeight",
    format = new TelemetryFormat(PER_SECOND)
  ) def cacheMissesLeaseBalanceAtHeight(): Int = cacheRate("leaseBalanceAtHeight", _.misses)

  private final val CacheEvictions = "State cache evictions"
  @static @Telemetry(value = CacheEvictions, line = "balance", format = new TelemetryFormat(PER_SECOND)) def cacheEvictionsBalance(): Int =
    cacheRate("balance", _.evictions)
  @static @Telemetry(value = CacheEvictions, line = "leaseBalance", format = new TelemetryFormat(PER_SECOND)) def cacheEvictionsLeaseBalance(): Int =
    cacheRate("leaseBalance", _.evictions)
  @static @Telemetry(value = CacheEvictions, line = "assetDescription", format = new TelemetryFormat(PER_SECOND)) def cacheEvictionsAssetDescription()
      : Int = cacheRate("assetDescription", _.evictions)
  @static @Telemetry(value = CacheEvictions, line = "volumeAndFee", format = new TelemetryFormat(PER_SECOND)) def cacheEvictionsVolumeAndFee(): Int =
    cacheRate("volumeAndFee", _.evictions)
  @static @Telemetry(value = CacheEvictions, line = "accountScript", format = new TelemetryFormat(PER_SECOND)) def cacheEvictionsAccountScript()
      : Int = cacheRate("accountScript", _.evictions)
  @static @Telemetry(value = CacheEvictions, line = "assetScript", format = new TelemetryFormat(PER_SECOND)) def cacheEvictionsAssetScript(): Int =
    cacheRate("assetScript", _.evictions)
  @static @Telemetry(value = CacheEvictions, line = "addressId", format = new TelemetryFormat(PER_SECOND)) def cacheEvictionsAddressId(): Int =
    cacheRate("addressId", _.evictions)
  @static @Telemetry(value = CacheEvictions, line = "accountData", format = new TelemetryFormat(PER_SECOND)) def cacheEvictionsAccountData(): Int =
    cacheRate("accountData", _.evictions)
  @static @Telemetry(value = CacheEvictions, line = "alias", format = new TelemetryFormat(PER_SECOND)) def cacheEvictionsAlias(): Int =
    cacheRate("alias", _.evictions)
  @static @Telemetry(value = CacheEvictions, line = "blockHeight", format = new TelemetryFormat(PER_SECOND)) def cacheEvictionsBlockHeight(): Int =
    cacheRate("blockHeight", _.evictions)
  @static @Telemetry(value = CacheEvictions, line = "balanceAtHeight", format = new TelemetryFormat(PER_SECOND)) def cacheEvictionsBalanceAtHeight()
      : Int = cacheRate("balanceAtHeight", _.evictions)
  @static @Telemetry(
    value = CacheEvictions,
    line = "leaseBalanceAtHeight",
    format = new TelemetryFormat(PER_SECOND)
  ) def cacheEvictionsLeaseBalanceAtHeight(): Int = cacheRate("leaseBalanceAtHeight", _.evictions)
}
