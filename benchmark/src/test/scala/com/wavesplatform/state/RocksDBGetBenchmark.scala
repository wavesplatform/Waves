package com.wavesplatform.state

import java.util.concurrent.TimeUnit
import com.typesafe.config.ConfigFactory
import com.wavesplatform.database.openDB
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.RocksDBGetBenchmark.*
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import org.rocksdb.{ReadOptions, RocksDB, WriteBatch, WriteOptions}
import sun.nio.ch.Util

import java.nio.file.Files

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 100)
class RocksDBGetBenchmark {
  @Benchmark
  def simpleGet(st: BaseSt, bh: Blackhole): Unit = {
    bh.consume(st.kvs.foreach { case (key, _) =>
      st.db.get(st.readOptions, key)
    })
  }

  @Benchmark
  def byteBufferGet(st: BaseSt, bh: Blackhole): Unit = {
    bh.consume {
      st.kvs.foreach { case (key, value) =>
        val keyBuffer = Util.getTemporaryDirectBuffer(key.length)
        keyBuffer.put(key).flip()
        val valBuffer = Util.getTemporaryDirectBuffer(value.length)

        st.db.get(st.readOptions, keyBuffer, valBuffer)

        Util.releaseTemporaryDirectBuffer(keyBuffer)
        Util.releaseTemporaryDirectBuffer(valBuffer)
      }
    }
  }
}

object RocksDBGetBenchmark {

  @State(Scope.Benchmark)
  class BaseSt {
    private val wavesSettings: WavesSettings =
      WavesSettings.fromRootConfig(loadConfig(ConfigFactory.load()))

    val db: RocksDB = {
      val dir = Files.createTempDirectory("state-synthetic").toAbsolutePath.toString
      openDB(wavesSettings.dbSettings.copy(directory = dir))
    }

    val kvs: Map[Array[Byte], Array[Byte]] = (1 to 10000).map { idx =>
      s"key$idx".getBytes -> s"value$idx".getBytes
    }.toMap

    val readOptions: ReadOptions = new ReadOptions()

    private val wb: WriteBatch = new WriteBatch()
    kvs.foreach { case (key, value) =>
      wb.put(key, value)
    }
    db.write(new WriteOptions(), wb)

    @TearDown
    def close(): Unit = {
      db.close()
    }
  }
}
