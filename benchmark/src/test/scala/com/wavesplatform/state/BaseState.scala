package com.wavesplatform.state

import com.typesafe.config.ConfigFactory

import java.io.File
import java.nio.file.Files
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.{RocksDBWriter, openDB}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.{FunctionalitySettings, WavesSettings, loadConfig}
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.state.utils.TestRocksDB
import com.wavesplatform.transaction.{GenesisTransaction, Transaction}
import monix.execution.UncaughtExceptionReporter
import monix.reactive.Observer
import org.openjdk.jmh.annotations.{Setup, TearDown}
import org.rocksdb.RocksDB
import org.scalacheck.{Arbitrary, Gen}

trait BaseState {
  import BaseState.*

  val benchSettings: Settings = Settings.fromConfig(ConfigFactory.load())
  val wavesSettings: WavesSettings = {
    val config = loadConfig(ConfigFactory.parseFile(new File(benchSettings.networkConfigFile)))
    WavesSettings.fromRootConfig(config)
  }
  private val fsSettings: FunctionalitySettings = updateFunctionalitySettings(FunctionalitySettings.TESTNET)
  private val db: RocksDB = {
    val dir = Files.createTempDirectory("state-synthetic").toAbsolutePath.toString
    openDB(wavesSettings.dbSettings.copy(directory = dir))
  }

  private val portfolioChanges = Observer.empty(UncaughtExceptionReporter.default)
  val state: RocksDBWriter     = TestRocksDB.withFunctionalitySettings(db, portfolioChanges, fsSettings)

  private var _richAccount: KeyPair = _
  def richAccount: KeyPair          = _richAccount

  private var _lastBlock: Block = _
  def lastBlock: Block          = _lastBlock

  protected def waves(n: Float): Long = (n * 100000000L).toLong
  protected val accountGen: Gen[KeyPair] =
    Gen.containerOfN[Array, Byte](32, Arbitrary.arbitrary[Byte]).map(seed => KeyPair(seed))

  protected def updateFunctionalitySettings(base: FunctionalitySettings): FunctionalitySettings = base

  protected def txGenP(sender: KeyPair, ts: Long): Gen[Transaction]

  private def genBlock(base: Block, sender: KeyPair): Gen[Block] =
    for {
      transferTxs <- Gen.sequence[Vector[Transaction], Transaction]((1 to TxsInBlock).map { i =>
        txGenP(sender, base.header.timestamp + i)
      })
    } yield TestBlock.create(
      time = transferTxs.last.timestamp,
      ref = base.id(),
      txs = transferTxs
    )

  private val initGen: Gen[(KeyPair, Block)] = for {
    rich <- accountGen
  } yield {
    val genesisTx = GenesisTransaction.create(rich.toAddress, waves(100000000L), System.currentTimeMillis() - 10000).explicitGet()
    (rich, TestBlock.create(time = genesisTx.timestamp, Seq(genesisTx)))
  }

  protected def nextBlock(txs: Seq[Transaction]): Block = TestBlock.create(
    time = txs.last.timestamp,
    ref = lastBlock.id(),
    txs = txs
  )

  private def append(prev: Option[Block], next: Block): Unit = {
    val preconditionDiff = BlockDiffer.fromBlock(state, prev, next, MiningConstraint.Unlimited, next.header.generationSignature).explicitGet().diff
    state.append(preconditionDiff, 0, 0, None, next.header.generationSignature, next)
  }

  def applyBlock(b: Block): Unit = {
    append(Some(lastBlock), b)
    _lastBlock = b
  }

  def genAndApplyNextBlock(): Unit = {
    val b = genBlock(lastBlock, richAccount).sample.get
    applyBlock(b)
  }

  @Setup
  def init(): Unit = {
    val (richAccount, genesisBlock) = initGen.sample.get
    _richAccount = richAccount

    append(None, genesisBlock)
    _lastBlock = genesisBlock
  }

  @TearDown
  def close(): Unit = {
    db.close()
  }
}

object BaseState {
  private val TxsInBlock = 5000
}
