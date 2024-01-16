package com.wavesplatform.state

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.*
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.database
import com.wavesplatform.database.{DBExt, Keys, RDB, RocksDBWriter}
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.RocksDBWriterBenchmark.*
import com.wavesplatform.transaction.Transaction
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.io.File
import java.util.concurrent.{ThreadLocalRandom, TimeUnit}
import scala.io.Codec
import scala.util.Using

/** Tests over real database. How to test:
  *   1. Download a database 2. Import it:
  *      https://github.com/wavesplatform/Waves/wiki/Export-and-import-of-the-blockchain#import-blocks-from-the-binary-file 3. Run ExtractInfo to
  *      collect queries for tests 4. Make Caches.MaxSize = 1 5. Run this test
  */
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 100)
class RocksDBWriterBenchmark {
  @Benchmark
  def readFullBlock_test(st: BlocksByIdSt, bh: Blackhole): Unit = {
    bh.consume(st.blockById(st.allBlocks.random).get)
  }

  @Benchmark
  def readBlockHeader_test(st: BlocksByHeightSt, bh: Blackhole): Unit = {
    bh.consume(st.db.blockHeader(st.allBlocks.random).get)
  }

  @Benchmark
  def transactionById_test(st: TransactionByIdSt, bh: Blackhole): Unit = {
    bh.consume(st.db.transactionInfo(st.allTxs.random).get)
  }
}

object RocksDBWriterBenchmark {

  @State(Scope.Benchmark)
  class TransactionByIdSt extends BaseSt {
    val allTxs: Vector[ByteStr] = load("transactionById", benchSettings.restTxsFile)(x => ByteStr(Base58.tryDecodeWithLimit(x).get))
  }

  @State(Scope.Benchmark)
  class TransactionByAddressSt extends BaseSt {
    val txsAddresses: Vector[Address] = load("transactionByAddress", ???)(x => Address.fromString(x).explicitGet())
  }

  @State(Scope.Benchmark)
  class BlocksByIdSt extends BaseSt {
    val allBlocks: Vector[ByteStr] = load("blocksById", benchSettings.blocksFile)(x => ByteStr(Base58.tryDecodeWithLimit(x).get))
  }

  @State(Scope.Benchmark)
  class BlocksByHeightSt extends BaseSt {
    val allBlocks: Vector[Int] = load("blocksByHeight", benchSettings.blocksFile)(_.toInt)
  }

  @State(Scope.Benchmark)
  class BaseSt {
    protected val benchSettings: Settings = Settings.fromConfig(ConfigFactory.load())
    private val wavesSettings: WavesSettings = {
      val config = loadConfig(ConfigFactory.parseFile(new File(benchSettings.networkConfigFile)))
      WavesSettings.fromRootConfig(config)
    }

    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = wavesSettings.blockchainSettings.addressSchemeCharacter.toByte
    }

    private val rawDB: RDB = {
      val dir = new File(wavesSettings.dbSettings.directory)
      if (!dir.isDirectory) throw new IllegalArgumentException(s"Can't find directory at '${wavesSettings.dbSettings.directory}'")
      RDB.open(wavesSettings.dbSettings)
    }

    val db = RocksDBWriter(rawDB, wavesSettings.blockchainSettings, wavesSettings.dbSettings, wavesSettings.enableLightMode)

    def loadBlockInfoAt(height: Int): Option[(BlockMeta, Seq[(TxMeta, Transaction)])] =
      loadBlockMetaAt(height).map { meta =>
        meta -> database.loadTransactions(Height(height), rawDB)
      }

    def loadBlockMetaAt(height: Int): Option[BlockMeta] = rawDB.db.get(Keys.blockMetaAt(Height(height))).flatMap(BlockMeta.fromPb)

    val cba = CommonBlocksApi(db, loadBlockMetaAt, loadBlockInfoAt)

    def blockById(id: ByteStr): Option[(BlockMeta, Seq[(TxMeta, Transaction)])] = cba.block(id)

    @TearDown
    def close(): Unit = {
      db.close()
      rawDB.close()
    }

    protected def load[T](label: String, absolutePath: String)(f: String => T): Vector[T] = {
      Using.resource(scala.io.Source.fromFile(absolutePath)(Codec.UTF8))(_.getLines().map(f).toVector)
    }
  }

  implicit class VectorOps[T](self: Vector[T]) {
    def random: T = self(ThreadLocalRandom.current().nextInt(self.size))
  }
}
