package com.wavesplatform.metrics

import com.jprofiler.api.probe.embedded.telemetry.{Telemetry, TelemetryFormat, TelemetryUnit}
import com.jprofiler.api.probe.embedded.{Payload, PayloadProbe}
import com.wavesplatform.database.RDB
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

  private enum CfGauge(val property: String) {
    case L0Files           extends CfGauge("rocksdb.num-files-at-level0")
    case PendingCompaction extends CfGauge("rocksdb.estimate-pending-compaction-bytes")
    case BlockCacheUsage   extends CfGauge("rocksdb.block-cache-usage")
    case Memtables         extends CfGauge("rocksdb.cur-size-all-mem-tables")
  }

  // In the order of RDB's column families: default, tx-meta, tx, tx-snapshot, api.
  private final class Source(val db: RocksDB, val statistics: Option[Statistics], val cfs: IndexedSeq[ColumnFamilyHandle])

  // Written by the importer's thread only.
  @volatile private var height = 0
  @volatile private var blocks = 0L
  @volatile private var txs    = 0L
  private var rangeStart       = -1
  private var rangeLabel       = ""

  // Guarded by this object's lock.
  private var source: Option[Source] = None
  private var lastNanos              = System.nanoTime()
  private var lastBlocks, lastTxs    = 0L
  private var blockRate, txRate      = 0.0
  private val lastRates              = new Array[Long](Rate.values.length)
  private val rates                  = new Array[Double](Rate.values.length)
  private val cfGauges               = Array.ofDim[Long](CfGauge.values.length, 5)
  private var compactions, flushes   = 0L

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

  private def label(height: Int): String = {
    val start = height / HeightRange * HeightRange
    if (start != rangeStart) {
      rangeStart = start
      rangeLabel = s"$start-${start + HeightRange - 1}"
    }
    rangeLabel
  }

  def watch(rdb: RDB): Unit = synchronized {
    val s = Source(
      rdb.db,
      rdb.statistics,
      IndexedSeq(rdb.db.getDefaultColumnFamily, rdb.txMetaHandle.handle, rdb.txHandle.handle, rdb.txSnapshotHandle.handle, rdb.apiHandle.handle)
    )
    s.statistics.foreach(st => Rate.values.foreach(r => lastRates(r.ordinal) = r.read(st)))
    source = Some(s)
  }

  /** Call before closing the database: a telemetry poll must not reach a closed RocksDB handle. */
  def unwatch(): Unit = synchronized { source = None }

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
      source.foreach { s =>
        s.statistics.foreach { st =>
          Rate.values.foreach { r =>
            val v = r.read(st)
            rates(r.ordinal) = (v - lastRates(r.ordinal)) / seconds
            lastRates(r.ordinal) = v
          }
        }
        for (g <- CfGauge.values; (cf, i) <- s.cfs.zipWithIndex) cfGauges(g.ordinal)(i) = s.db.getLongProperty(cf, g.property)
        compactions = s.db.getLongProperty("rocksdb.num-running-compactions")
        flushes = s.db.getLongProperty("rocksdb.num-running-flushes")
      }
      lastNanos = now
    }
  }

  // Probe telemetries are stored as 32-bit ints.
  private def int(v: Double): Int = math.min(v, Int.MaxValue.toDouble).toInt

  private def rate(r: Rate): Int   = { sample(); int(rates(r.ordinal)) }
  private def kbRate(r: Rate): Int = { sample(); int(rates(r.ordinal) / 1000) }
  // Share of one thread's time, in tenths of a percent.
  private def timeShare(r: Rate): Int = { sample(); int(rates(r.ordinal) / 1000) }
  // In tenths of a percent.
  private def hitRate(hit: Rate, miss: Rate): Int = {
    sample()
    val total = rates(hit.ordinal) + rates(miss.ordinal)
    if (total == 0) 0 else int(rates(hit.ordinal) * 1000 / total)
  }
  private def cf(g: CfGauge, i: Int): Int   = { sample(); int(cfGauges(g.ordinal)(i).toDouble) }
  private def cfKb(g: CfGauge, i: Int): Int = { sample(); int(cfGauges(g.ordinal)(i) / 1000.0) }
  private def cfTotalKb(g: CfGauge): Int    = { sample(); int(cfGauges(g.ordinal).sum / 1000.0) }

  @static @Telemetry("Import height") def importHeight(): Int                                                         = height
  @static @Telemetry(value = "Import blocks", format = new TelemetryFormat(PER_SECOND)) def blocksPerSecond(): Int    = { sample(); int(blockRate) }
  @static @Telemetry(value = "Import transactions", format = new TelemetryFormat(PER_SECOND)) def txsPerSecond(): Int = { sample(); int(txRate) }

  @static @Telemetry(value = "RocksDB keys written", format = new TelemetryFormat(PER_SECOND)) def keysWritten(): Int = rate(Rate.KeysWritten)
  @static @Telemetry(value = "RocksDB table opens", format = new TelemetryFormat(PER_SECOND)) def tableOpens(): Int   = rate(Rate.TableOpens)

  private final val Time = "RocksDB time (100% = one thread)"
  @static @Telemetry(value = Time, line = "get", format = new TelemetryFormat(value = PERCENT, scale = 1)) def getTime(): Int = timeShare(
    Rate.GetMicros
  )
  @static @Telemetry(value = Time, line = "multiGet", format = new TelemetryFormat(value = PERCENT, scale = 1)) def multiGetTime(): Int = timeShare(
    Rate.MultiGetMicros
  )
  @static @Telemetry(value = Time, line = "write", format = new TelemetryFormat(value = PERCENT, scale = 1)) def writeTime(): Int = timeShare(
    Rate.WriteMicros
  )
  @static @Telemetry(value = Time, line = "seek", format = new TelemetryFormat(value = PERCENT, scale = 1)) def seekTime(): Int = timeShare(
    Rate.SeekMicros
  )
  @static @Telemetry(value = Time, line = "stall", format = new TelemetryFormat(value = PERCENT, scale = 1)) def stallTime(): Int = timeShare(
    Rate.StallMicros
  )
  @static @Telemetry(value = Time, line = "SST read, all threads", format = new TelemetryFormat(value = PERCENT, scale = 1)) def sstReadTime(): Int =
    timeShare(Rate.SstReadMicros)
  @static @Telemetry(value = Time, line = "table open IO, all threads", format = new TelemetryFormat(value = PERCENT, scale = 1)) def tableOpenTime()
      : Int = timeShare(Rate.TableOpenMicros)

  private final val Hits = "RocksDB block cache hit rate"
  @static @Telemetry(value = Hits, line = "data", format = new TelemetryFormat(value = PERCENT, scale = 1)) def dataHits(): Int =
    hitRate(Rate.DataHit, Rate.DataMiss)
  @static @Telemetry(value = Hits, line = "index", format = new TelemetryFormat(value = PERCENT, scale = 1)) def indexHits(): Int =
    hitRate(Rate.IndexHit, Rate.IndexMiss)
  @static @Telemetry(value = Hits, line = "filter", format = new TelemetryFormat(value = PERCENT, scale = 1)) def filterHits(): Int =
    hitRate(Rate.FilterHit, Rate.FilterMiss)

  private final val Gets                                                                                                = "RocksDB gets served by"
  @static @Telemetry(value = Gets, line = "memtable", format = new TelemetryFormat(PER_SECOND)) def getsMemtable(): Int = rate(Rate.MemtableHit)
  @static @Telemetry(value = Gets, line = "L0", format = new TelemetryFormat(PER_SECOND)) def getsL0(): Int             = rate(Rate.HitL0)
  @static @Telemetry(value = Gets, line = "L1", format = new TelemetryFormat(PER_SECOND)) def getsL1(): Int             = rate(Rate.HitL1)
  @static @Telemetry(value = Gets, line = "L2 and up", format = new TelemetryFormat(PER_SECOND)) def getsL2(): Int      = rate(Rate.HitL2AndUp)
  @static @Telemetry(value = Gets, line = "skipped by bloom filter", format = new TelemetryFormat(PER_SECOND)) def filterUseful(): Int = rate(
    Rate.FilterUseful
  )

  private final val Io = "RocksDB IO per second"
  @static @Telemetry(value = Io, line = "written", format = new TelemetryFormat(value = BYTES, scale = -3)) def bytesWritten(): Int = kbRate(
    Rate.BytesWritten
  )
  @static @Telemetry(value = Io, line = "read by gets", format = new TelemetryFormat(value = BYTES, scale = -3)) def bytesRead(): Int = kbRate(
    Rate.BytesRead
  )
  @static @Telemetry(value = Io, line = "WAL", format = new TelemetryFormat(value = BYTES, scale = -3)) def walBytes(): Int = kbRate(Rate.WalBytes)
  @static @Telemetry(value = Io, line = "flush", format = new TelemetryFormat(value = BYTES, scale = -3)) def flushBytes(): Int = kbRate(
    Rate.FlushBytes
  )
  @static @Telemetry(value = Io, line = "compaction read", format = new TelemetryFormat(value = BYTES, scale = -3)) def compactReadBytes(): Int =
    kbRate(Rate.CompactReadBytes)
  @static @Telemetry(value = Io, line = "compaction write", format = new TelemetryFormat(value = BYTES, scale = -3)) def compactWriteBytes(): Int =
    kbRate(Rate.CompactWriteBytes)

  private final val L0                                                         = "RocksDB L0 files"
  @static @Telemetry(value = L0, line = "default") def l0Default(): Int        = cf(CfGauge.L0Files, 0)
  @static @Telemetry(value = L0, line = "tx-meta") def l0TxMeta(): Int         = cf(CfGauge.L0Files, 1)
  @static @Telemetry(value = L0, line = "tx") def l0Tx(): Int                  = cf(CfGauge.L0Files, 2)
  @static @Telemetry(value = L0, line = "tx-snapshot") def l0TxSnapshot(): Int = cf(CfGauge.L0Files, 3)
  @static @Telemetry(value = L0, line = "api") def l0Api(): Int                = cf(CfGauge.L0Files, 4)

  private final val Pending = "RocksDB pending compaction"
  @static @Telemetry(value = Pending, line = "default", format = new TelemetryFormat(value = BYTES, scale = -3)) def pendingDefault(): Int =
    cfKb(CfGauge.PendingCompaction, 0)
  @static @Telemetry(value = Pending, line = "tx-meta", format = new TelemetryFormat(value = BYTES, scale = -3)) def pendingTxMeta(): Int =
    cfKb(CfGauge.PendingCompaction, 1)
  @static @Telemetry(value = Pending, line = "tx", format = new TelemetryFormat(value = BYTES, scale = -3)) def pendingTx(): Int =
    cfKb(CfGauge.PendingCompaction, 2)
  @static @Telemetry(value = Pending, line = "tx-snapshot", format = new TelemetryFormat(value = BYTES, scale = -3)) def pendingTxSnapshot(): Int =
    cfKb(CfGauge.PendingCompaction, 3)
  @static @Telemetry(value = Pending, line = "api", format = new TelemetryFormat(value = BYTES, scale = -3)) def pendingApi(): Int =
    cfKb(CfGauge.PendingCompaction, 4)

  private final val Jobs                                                               = "RocksDB background jobs"
  @static @Telemetry(value = Jobs, line = "compactions") def runningCompactions(): Int = { sample(); compactions.toInt }
  @static @Telemetry(value = Jobs, line = "flushes") def runningFlushes(): Int         = { sample(); flushes.toInt }

  private final val Memory = "RocksDB memory"
  @static @Telemetry(value = Memory, line = "block caches", format = new TelemetryFormat(value = BYTES, scale = -3)) def blockCacheUsage(): Int =
    cfTotalKb(CfGauge.BlockCacheUsage)
  @static @Telemetry(value = Memory, line = "memtables", format = new TelemetryFormat(value = BYTES, scale = -3)) def memtables(): Int = cfTotalKb(
    CfGauge.Memtables
  )
}
