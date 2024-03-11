package com.wavesplatform.state

import com.google.common.primitives.{Bytes, Shorts}
import com.typesafe.config.ConfigFactory
import com.wavesplatform.database.{
  AddressId,
  CurrentData,
  DataNode,
  KeyTags,
  Keys,
  RDB,
  readCurrentData,
  readDataNode,
  writeCurrentData,
  writeDataNode
}
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.RocksDBSeekForPrevBenchmark.*
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import org.rocksdb.{ReadOptions, WriteBatch, WriteOptions}

import java.nio.file.Files
import java.util.concurrent.TimeUnit
import scala.util.Using

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 100)
class RocksDBSeekForPrevBenchmark {
  @Benchmark
  def seekForPrev(st: BaseSt, bh: Blackhole): Unit = {
    bh.consume {
      val iter = st.rdb.db.newIterator(st.readOptions)
      iter.seekForPrev(st.dataNodeKey(Height(Int.MaxValue)))
      if (iter.isValid && iter.key().startsWith(st.dataNodeKeyPrefix)) {
        readDataNode(st.keyString)(iter.value()).prevHeight
      }
      iter.close()
    }
  }

  @Benchmark
  def get(st: BaseSt, bh: Blackhole): Unit = {
    bh.consume {
      readCurrentData(st.keyString)(st.rdb.db.get(st.currentDataKey)).prevHeight
    }
  }
}

object RocksDBSeekForPrevBenchmark {

  @State(Scope.Benchmark)
  class BaseSt {
    private val wavesSettings: WavesSettings =
      WavesSettings.fromRootConfig(loadConfig(ConfigFactory.load()))

    val rdb: RDB = {
      val dir = Files.createTempDirectory("state-synthetic").toAbsolutePath.toString
      RDB.open(wavesSettings.dbSettings.copy(directory = dir))
    }

    val addressId: AddressId = AddressId(1L)

    val keyString                          = "key"
    val currentDataKey: Array[Byte]        = Keys.data(addressId, keyString).keyBytes
    val dataNodeKey: Height => Array[Byte] = Keys.dataAt(addressId, "key")(_).keyBytes
    val dataNodeKeyPrefix: Array[Byte] = Bytes.concat(Shorts.toByteArray(KeyTags.DataHistory.id.toShort), addressId.toByteArray, keyString.getBytes)

    private val dataEntry: StringDataEntry = StringDataEntry(keyString, "value")

    val readOptions: ReadOptions = new ReadOptions()

    Using.Manager { use =>
      val wb = use(new WriteBatch())
      wb.put(currentDataKey, writeCurrentData(CurrentData(dataEntry, Height(10000), Height(9999))))
      (1 to 1000).foreach { h =>
        wb.put(dataNodeKey(Height(h)), writeDataNode(DataNode(dataEntry, Height(h - 1))))
      }
      rdb.db.write(use(new WriteOptions()), wb)
    }

    @TearDown
    def close(): Unit = {
      readOptions.close()
      rdb.close()
    }
  }
}
