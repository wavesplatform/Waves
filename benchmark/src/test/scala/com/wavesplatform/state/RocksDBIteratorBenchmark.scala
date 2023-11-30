package com.wavesplatform.state

import java.nio.file.Files
import java.util.concurrent.TimeUnit

import com.google.common.primitives.Ints
import com.typesafe.config.ConfigFactory
import com.wavesplatform.database.RDB
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.RocksDBIteratorBenchmark.*
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import org.rocksdb.{ReadOptions, WriteBatch, WriteOptions}

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 100)
class RocksDBIteratorBenchmark {
  @Benchmark
  def directOrderIterator(st: BaseSt, bh: Blackhole): Unit = {
    bh.consume {
      val iter = st.rdb.db.newIterator(st.readOptions)
      iter.seek(st.firstKey)
      while (iter.isValid) {
        iter.key()
        iter.value()
        iter.next()
      }
      iter.close()
    }
  }

  @Benchmark
  def reverseOrderIterator(st: BaseSt, bh: Blackhole): Unit = {
    bh.consume {
      val iter = st.rdb.db.newIterator(st.readOptions)
      iter.seekForPrev(st.lastKey)
      while (iter.isValid) {
        iter.key()
        iter.value()
        iter.prev()
      }
      iter.close()
    }
  }
}

object RocksDBIteratorBenchmark {

  @State(Scope.Benchmark)
  class BaseSt {
    private val wavesSettings: WavesSettings =
      WavesSettings.fromRootConfig(loadConfig(ConfigFactory.load()))

    val rdb: RDB = {
      val dir = Files.createTempDirectory("state-synthetic").toAbsolutePath.toString
      RDB.open(wavesSettings.dbSettings.copy(directory = dir))
    }

    val keysPrefix            = "keysPrefix"
    val firstKey: Array[Byte] = keysPrefix.getBytes ++ Ints.toByteArray(1)
    val lastKey: Array[Byte]  = keysPrefix.getBytes ++ Ints.toByteArray(10000)

    val kvs: Map[Array[Byte], Array[Byte]] = (1 to 10000).map { idx =>
      (keysPrefix.getBytes ++ Ints.toByteArray(idx)) -> s"value$idx".getBytes
    }.toMap

    val readOptions: ReadOptions = new ReadOptions().setTotalOrderSeek(false).setPrefixSameAsStart(true)

    private val wb: WriteBatch = new WriteBatch()
    kvs.foreach { case (key, value) =>
      wb.put(key, value)
    }
    rdb.db.write(new WriteOptions(), wb)

    @TearDown
    def close(): Unit = {
      rdb.close()
    }
  }
}
