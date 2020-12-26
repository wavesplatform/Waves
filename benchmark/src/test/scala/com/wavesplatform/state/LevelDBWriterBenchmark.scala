package com.wavesplatform.state

import java.io.File
import java.util.concurrent.{ThreadLocalRandom, TimeUnit}

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account._
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.database
import com.wavesplatform.database.{DBExt, Keys, LevelDBFactory, LevelDBWriter}
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.LevelDBWriterBenchmark._
import com.wavesplatform.transaction.{ApplicationStatus, Transaction}
import org.iq80.leveldb.{DB, Options}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.io.Codec

/**
  * Tests over real database. How to test:
  * 1. Download a database
  * 2. Import it: https://github.com/wavesplatform/Waves/wiki/Export-and-import-of-the-blockchain#import-blocks-from-the-binary-file
  * 3. Run ExtractInfo to collect queries for tests
  * 4. Make Caches.MaxSize = 1
  * 5. Run this test
  */
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 100)
class LevelDBWriterBenchmark {
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

object LevelDBWriterBenchmark {

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

    private val rawDB: DB = {
      val dir = new File(wavesSettings.dbSettings.directory)
      if (!dir.isDirectory) throw new IllegalArgumentException(s"Can't find directory at '${wavesSettings.dbSettings.directory}'")
      LevelDBFactory.factory.open(dir, new Options)
    }

    val db = LevelDBWriter.readOnly(rawDB, wavesSettings)

    def loadBlockInfoAt(height: Int): Option[(BlockMeta, Seq[(Transaction, ApplicationStatus)])] =
      loadBlockMetaAt(height).map { meta =>
        meta -> rawDB.readOnly(ro => database.loadTransactions(Height(height), ro)).fold(Seq.empty[(Transaction, ApplicationStatus)])(identity)
      }

    def loadBlockMetaAt(height: Int): Option[BlockMeta] = rawDB.get(Keys.blockMetaAt(Height(height)))

    val cba = CommonBlocksApi(db, loadBlockMetaAt, loadBlockInfoAt)

    def blockById(id: ByteStr): Option[(BlockMeta, Seq[(Transaction, ApplicationStatus)])] = cba.block(id)

    @TearDown
    def close(): Unit = {
      rawDB.close()
    }

    protected def load[T](label: String, absolutePath: String)(f: String => T): Vector[T] = {
      scala.io.Source
        .fromFile(absolutePath)(Codec.UTF8)
        .getLines()
        .map(f)
        .toVector
    }
  }

  implicit class VectorOps[T](self: Vector[T]) {
    def random: T = self(ThreadLocalRandom.current().nextInt(self.size))
  }
}
