package com.wavesplatform.state

import java.io.File
import java.nio.file.Files

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.block.Block
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.db.LevelDBFactory
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.transaction.{GenesisTransaction, Transaction}
import org.iq80.leveldb.{DB, Options}
import org.openjdk.jmh.annotations.{Setup, TearDown}
import org.scalacheck.{Arbitrary, Gen}

trait BaseState {
  import BaseState._

  private val fsSettings: FunctionalitySettings = updateFunctionalitySettings(FunctionalitySettings.TESTNET)
  private val db: DB = {
    val dir     = Files.createTempDirectory("state-synthetic").toAbsolutePath.toString
    val options = new Options()
    options.createIfMissing(true)
    LevelDBFactory.factory.open(new File(dir), options)
  }

  val state: LevelDBWriter = new LevelDBWriter(db, fsSettings, 100000, 2000, 120 * 60 * 1000)

  private var _richAccount: PrivateKeyAccount = _
  def richAccount: PrivateKeyAccount          = _richAccount

  private var _lastBlock: Block = _
  def lastBlock: Block          = _lastBlock

  protected def waves(n: Float): Long              = (n * 100000000L).toLong
  protected val accountGen: Gen[PrivateKeyAccount] = Gen.containerOfN[Array, Byte](32, Arbitrary.arbitrary[Byte]).map(seed => PrivateKeyAccount(seed))

  protected def updateFunctionalitySettings(base: FunctionalitySettings): FunctionalitySettings = base

  protected def txGenP(sender: PrivateKeyAccount, ts: Long): Gen[Transaction]

  private def genBlock(base: Block, sender: PrivateKeyAccount): Gen[Block] =
    for {
      transferTxs <- Gen.sequence[Vector[Transaction], Transaction]((1 to TxsInBlock).map { i =>
        txGenP(sender, base.timestamp + i)
      })
    } yield
      TestBlock.create(
        time = transferTxs.last.timestamp,
        ref = base.uniqueId,
        txs = transferTxs
      )

  private val initGen: Gen[(PrivateKeyAccount, Block)] = for {
    rich <- accountGen
  } yield {
    val genesisTx = GenesisTransaction.create(rich, waves(100000000L), System.currentTimeMillis() - 10000).explicitGet()
    (rich, TestBlock.create(time = genesisTx.timestamp, Seq(genesisTx)))
  }

  protected def nextBlock(txs: Seq[Transaction]): Block = TestBlock.create(
    time = txs.last.timestamp,
    ref = lastBlock.uniqueId,
    txs = txs
  )

  private def append(prev: Option[Block], next: Block): Unit = {
    val preconditionDiff = BlockDiffer.fromBlock(fsSettings, state, prev, next, MiningConstraint.Unlimited).explicitGet()._1
    state.append(preconditionDiff, 0, next)
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
