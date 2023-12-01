package com.wavesplatform.state

import java.nio.file.Files
import java.util.Comparator
import java.util.concurrent.TimeUnit
import java.util.function.Consumer

import com.google.common.primitives.{Ints, UnsignedBytes}
import com.typesafe.config.ConfigFactory
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.RDB
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.RocksDBWriteBatchBenchmark.*
import org.eclipse.collections.api.block.HashingStrategy
import org.eclipse.collections.api.tuple.Pair
import org.eclipse.collections.impl.factory.{HashingStrategyMaps, HashingStrategySets}
import org.eclipse.collections.impl.utility.MapIterate
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import org.rocksdb.{WriteBatch, WriteOptions}
import com.wavesplatform.utils.byteStrOrdering

import scala.util.Random

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 100)
class RocksDBWriteBatchBenchmark {

  @Benchmark
  def sortedBatch(st: BaseSt, bh: Blackhole): Unit = {
    bh.consume {
      val sortedBatch = new SortedBatch
      val nativeBatch = new WriteBatch()
      st.kvsShuffled.foreach { case (k, v) =>
        sortedBatch.put(k, v)
      }
      MapIterate
        .toListOfPairs(sortedBatch.addedEntries)
        .sortThis((o1: Pair[Array[Byte], Array[Byte]], o2: Pair[Array[Byte], Array[Byte]]) =>
          UnsignedBytes.lexicographicalComparator().compare(o1.getOne, o2.getOne)
        )
        .forEach(new Consumer[Pair[Array[Byte], Array[Byte]]] {
          override def accept(t: Pair[Array[Byte], Array[Byte]]): Unit = nativeBatch.put(t.getOne, t.getTwo)
        })
      st.rdb.db.write(st.writeOptions, nativeBatch)
    }
  }

  @Benchmark
  def notSortedBatch(st: BaseSt, bh: Blackhole): Unit = {
    bh.consume {
      val nativeBatch = new WriteBatch()
      st.kvsShuffled.foreach { case (k, v) =>
        nativeBatch.put(k, v)
      }
      st.rdb.db.write(st.writeOptions, nativeBatch)
    }
  }
}

object RocksDBWriteBatchBenchmark {

  @State(Scope.Benchmark)
  class BaseSt {
    private val wavesSettings: WavesSettings =
      WavesSettings.fromRootConfig(loadConfig(ConfigFactory.load()))

    val rdb: RDB = {
      val dir = Files.createTempDirectory("state-synthetic").toAbsolutePath.toString
      RDB.open(wavesSettings.dbSettings.copy(directory = dir))
    }

    private val minIdx      = 1
    private val maxIdx      = 10000
    private val firstPrefix = 'A'
    private val lastPrefix  = 'Z'

    private val firstKey = s"${firstPrefix}key".getBytes ++ Ints.toByteArray(minIdx)
    private val lastKey  = s"${lastPrefix}key".getBytes ++ Ints.toByteArray(maxIdx)

    private val kvs: Seq[(Array[Byte], Array[Byte])] =
      for {
        prefixChar <- firstPrefix to lastPrefix
        idx        <- minIdx to maxIdx
      } yield {
        (s"${prefixChar}key".getBytes ++ Ints.toByteArray(idx)) -> s"value$idx".getBytes
      }

    Random.setSeed(42)
    val kvsShuffled: Seq[(Array[Byte], Array[Byte])] = Random.shuffle(kvs)

    val writeOptions = new WriteOptions()

    @Setup(Level.Invocation)
    def setup(): Unit =
      rdb.db.deleteRange(firstKey, lastKey)

    @TearDown
    def close(): Unit = {
      writeOptions.close()
      rdb.close()
    }
  }

  class SortedBatch extends WriteBatch {
    val addedEntries   = HashingStrategyMaps.mutable.`with`[Array[Byte], Array[Byte]](ByteArrayHashingStrategy)
    val deletedEntries = HashingStrategySets.mutable.`with`[Array[Byte]](ByteArrayHashingStrategy)

    override def put(bytes: Array[Byte], bytes1: Array[Byte]): Unit = {
      addedEntries.put(bytes, bytes1)
      deletedEntries.remove(bytes)
    }

    override def delete(bytes: Array[Byte]): Unit = {
      addedEntries.remove(bytes)
      deletedEntries.add(bytes)
    }

  }

  object SortedBatch {
    val byteStrComparator: Comparator[ByteStr] = (o1: ByteStr, o2: ByteStr) => byteStrOrdering.compare(o1, o2)
  }

  object ByteArrayHashingStrategy extends HashingStrategy[Array[Byte]] {
    override def computeHashCode(obj: Array[Byte]): Int = java.util.Arrays.hashCode(obj)

    override def equals(object1: Array[Byte], object2: Array[Byte]): Boolean = java.util.Arrays.equals(object1, object2)
  }
}
