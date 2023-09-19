package com.wavesplatform.db

import cats.syntax.traverse.*
import com.google.common.primitives.Shorts
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.{GenesisBlockVersion, GenesisGenerationSignature, GenesisGenerator, GenesisReference}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.{KeyTags, RDB, RocksDBWriter, TestStorageFactory, loadActiveLeases}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.TransactionStateSnapshot
import com.wavesplatform.history.SnapshotOps.TransactionStateSnapshotExt
import com.wavesplatform.history.{Domain, SnapshotOps}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lagonaki.mocks.TestBlock.BlockWithSigner
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.{TestFunctionalitySettings as TFS, *}
import com.wavesplatform.state.diffs.{BlockDiffer, ENOUGH_AMT}
import com.wavesplatform.state.reader.SnapshotBlockchain
import com.wavesplatform.state.utils.TestRocksDB
import com.wavesplatform.state.{Blockchain, BlockchainUpdaterImpl, Diff, NgState, Portfolio, StateSnapshot, TxStateSnapshotHashBuilder}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{BlockchainUpdater, GenesisTransaction, Transaction, TxHelpers}
import com.wavesplatform.{NTPTime, TestHelpers}
import org.rocksdb.RocksDB
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, Suite}

import java.nio.file.Files
import scala.concurrent.duration.*

trait WithState extends BeforeAndAfterAll with DBCacheSettings with Matchers with NTPTime { _: Suite =>
  protected val ignoreBlockchainUpdateTriggers: BlockchainUpdateTriggers = BlockchainUpdateTriggers.noop

  private val path  = Files.createTempDirectory("rocks-temp").toAbsolutePath
  protected val rdb = RDB.open(dbSettings.copy(directory = path.toAbsolutePath.toString))

  private val MaxKey = Shorts.toByteArray(KeyTags.maxId.toShort)
  private val MinKey = new Array[Byte](2)

  protected def tempDb[A](f: RDB => A): A = {
    val path = Files.createTempDirectory("rocks-temp").toAbsolutePath
    val rdb  = RDB.open(dbSettings.copy(directory = path.toAbsolutePath.toString))
    try {
      f(rdb)
    } finally {
      rdb.close()
      TestHelpers.deleteRecursively(path)
    }
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    rdb.close()
    TestHelpers.deleteRecursively(path)
  }

  protected def withRocksDBWriter[A](ws: WavesSettings)(test: RocksDBWriter => A): A = {
    try {
      val (_, rdw) = TestStorageFactory(
        ws,
        rdb,
        ntpTime,
        ignoreBlockchainUpdateTriggers
      )
      test(rdw)
    } finally {
      Seq(rdb.db.getDefaultColumnFamily, rdb.txHandle.handle, rdb.txMetaHandle.handle).foreach { cfh =>
        rdb.db.deleteRange(cfh, MinKey, MaxKey)
      }
    }
  }

  protected def withRocksDBWriter[A](bs: BlockchainSettings)(test: RocksDBWriter => A): A =
    withRocksDBWriter(TestSettings.Default.copy(blockchainSettings = bs))(test)

  def withRocksDBWriter[A](fs: FunctionalitySettings)(test: RocksDBWriter => A): A =
    withRocksDBWriter(TestRocksDB.createTestBlockchainSettings(fs))(test)

  protected def withTestState[A](ws: WavesSettings)(test: (BlockchainUpdaterImpl, RocksDBWriter) => A): A = {
    try {
      val (bcu, rdw) = TestStorageFactory(
        ws,
        rdb,
        ntpTime,
        ignoreBlockchainUpdateTriggers
      )
      test(bcu, rdw)
    } finally {
      Seq(rdb.db.getDefaultColumnFamily, rdb.txHandle.handle, rdb.txMetaHandle.handle).foreach { cfh =>
        rdb.db.deleteRange(cfh, MinKey, MaxKey)
      }
    }
  }

  protected def withTestState[A](bs: BlockchainSettings)(test: (BlockchainUpdaterImpl, RocksDBWriter) => A): A =
    withTestState(TestSettings.Default.copy(blockchainSettings = bs))(test(_, _))

  def withTestState[A](fs: FunctionalitySettings)(test: (BlockchainUpdaterImpl, RocksDBWriter) => A): A =
    withTestState(TestRocksDB.createTestBlockchainSettings(fs))(test)

  def assertDiffEi(
      preconditions: Seq[BlockWithSigner],
      block: BlockWithSigner,
      fs: FunctionalitySettings = TFS.Enabled,
      enableExecutionLog: Boolean = false
  )(
      assertion: Either[ValidationError, Diff] => Unit
  ): Unit = withTestState(fs) { (bcu, state) =>
    assertDiffEi(preconditions, block, bcu, state, enableExecutionLog)(assertion)
  }

  def assertDiffEi(
      preconditions: Seq[BlockWithSigner],
      block: BlockWithSigner,
      bcu: BlockchainUpdaterImpl,
      state: RocksDBWriter,
      enableExecutionLog: Boolean
  )(
      assertion: Either[ValidationError, Diff] => Unit
  ): Unit = {
    def differ(blockchain: Blockchain, b: Block) =
      BlockDiffer.fromBlock(
        blockchain,
        None,
        b,
        None,
        MiningConstraint.Unlimited,
        b.header.generationSignature,
        enableExecutionLog = enableExecutionLog
      )

    preconditions.foreach { precondition =>
      val preconditionBlock = blockWithComputedStateHash(precondition.block, precondition.signer, bcu).resultE.explicitGet()
      val BlockDiffer.Result(preconditionDiff, preconditionFees, totalFee, _, _, computedStateHash) = differ(state, preconditionBlock).explicitGet()
      state.append(
        preconditionDiff,
        preconditionFees,
        totalFee,
        None,
        preconditionBlock.header.generationSignature,
        computedStateHash,
        preconditionBlock
      )
    }
    val totalDiff1 = blockWithComputedStateHash(block.block, block.signer, bcu).resultE.flatMap(differ(state, _))
    assertion(totalDiff1.map(_.snapshot.toDiff(state)))
  }

  def assertDiffEiTraced(
      preconditions: Seq[BlockWithSigner],
      block: BlockWithSigner,
      fs: FunctionalitySettings = TFS.Enabled,
      enableExecutionLog: Boolean = false
  )(
      assertion: TracedResult[ValidationError, Diff] => Unit
  ): Unit = withTestState(fs) { (bcu, state) =>
    def getCompBlockchain(blockchain: Blockchain) = {
      val reward = if (blockchain.height > 0) bcu.computeNextReward else None
      SnapshotBlockchain(blockchain, reward)
    }

    def differ(blockchain: Blockchain, prevBlock: Option[Block], b: Block) =
      BlockDiffer.fromBlockTraced(
        getCompBlockchain(blockchain),
        prevBlock,
        b,
        None,
        MiningConstraint.Unlimited,
        b.header.generationSignature,
        (_, _) => (),
        verify = true,
        enableExecutionLog = enableExecutionLog,
        txSignParCheck = true
      )

    preconditions.foreach { precondition =>
      val preconditionBlock = blockWithComputedStateHash(precondition.block, precondition.signer, bcu).resultE.explicitGet()
      val BlockDiffer.Result(preconditionDiff, preconditionFees, totalFee, _, _, computedStateHash) =
        differ(state, state.lastBlock, preconditionBlock).resultE.explicitGet()
      state.append(
        preconditionDiff,
        preconditionFees,
        totalFee,
        None,
        preconditionBlock.header.generationSignature,
        computedStateHash,
        preconditionBlock
      )
    }

    val totalDiff1 =
      (blockWithComputedStateHash(block.block, block.signer, bcu) match {
        case right @ TracedResult(Right(_), _, _) => right.copy(trace = Nil)
        case err                                  => err
      }).flatMap(differ(state, state.lastBlock, _))

    assertion(totalDiff1.map(_.snapshot.toDiff(state)))
  }

  private def assertDiffAndState(preconditions: Seq[BlockWithSigner], block: BlockWithSigner, fs: FunctionalitySettings, withNg: Boolean)(
      assertion: (Diff, Blockchain) => Unit
  ): Unit = withTestState(fs) { (bcu, state) =>
    def getCompBlockchain(blockchain: Blockchain) =
      if (withNg && fs.preActivatedFeatures.get(BlockchainFeatures.BlockReward.id).exists(_ <= blockchain.height)) {
        val reward = if (blockchain.height > 0) bcu.computeNextReward else None
        SnapshotBlockchain(blockchain, reward)
      } else blockchain

    def differ(blockchain: Blockchain, prevBlock: Option[Block], b: Block): Either[ValidationError, BlockDiffer.Result] =
      BlockDiffer.fromBlock(
        getCompBlockchain(blockchain),
        if (withNg) prevBlock else None,
        b,
        None,
        MiningConstraint.Unlimited,
        b.header.generationSignature
      )

    preconditions.foldLeft[Option[Block]](None) { (prevBlock, curBlock) =>
      val preconditionBlock = blockWithComputedStateHash(curBlock.block, curBlock.signer, bcu).resultE.explicitGet()
      val BlockDiffer.Result(diff, fees, totalFee, _, _, computedStateHash) = differ(state, prevBlock, preconditionBlock).explicitGet()
      state.append(diff, fees, totalFee, None, preconditionBlock.header.generationSignature, computedStateHash, preconditionBlock)
      Some(preconditionBlock)
    }

    val checkedBlock = blockWithComputedStateHash(block.block, block.signer, bcu).resultE.explicitGet()
    val BlockDiffer.Result(snapshot, fees, totalFee, _, _, computedStateHash) = differ(state, state.lastBlock, checkedBlock).explicitGet()
    val ngState =
      NgState(
        checkedBlock,
        snapshot,
        fees,
        totalFee,
        computedStateHash,
        fs.preActivatedFeatures.keySet,
        None,
        checkedBlock.header.generationSignature,
        Map()
      )
    val cb   = SnapshotBlockchain(state, ngState)
    val diff = snapshot.toDiff(state)
    assertion(diff, cb)

    state.append(snapshot, fees, totalFee, None, checkedBlock.header.generationSignature, computedStateHash, checkedBlock)

    assertion(diff, state)
  }

  def assertNgDiffState(preconditions: Seq[BlockWithSigner], block: BlockWithSigner, fs: FunctionalitySettings = TFS.Enabled)(
      assertion: (Diff, Blockchain) => Unit
  ): Unit =
    assertDiffAndState(preconditions, block, fs, withNg = true)(assertion)

  def assertDiffAndState(preconditions: Seq[BlockWithSigner], block: BlockWithSigner, fs: FunctionalitySettings = TFS.Enabled)(
      assertion: (Diff, Blockchain) => Unit
  ): Unit =
    assertDiffAndState(preconditions, block, fs, withNg = false)(assertion)

  def assertDiffAndState(fs: FunctionalitySettings)(test: (Seq[Transaction] => Either[ValidationError, Unit]) => Unit): Unit =
    withTestState(fs) { (bcu, state) =>
      def getCompBlockchain(blockchain: Blockchain) = {
        val reward = if (blockchain.height > 0) bcu.computeNextReward else None
        SnapshotBlockchain(blockchain, reward)
      }

      def differ(blockchain: Blockchain, b: Block) =
        BlockDiffer.fromBlock(
          getCompBlockchain(blockchain),
          state.lastBlock,
          b,
          None,
          MiningConstraint.Unlimited,
          b.header.generationSignature
        )

      test(txs => {
        val nextHeight   = state.height + 1
        val isProto      = state.activatedFeatures.get(BlockchainFeatures.BlockV5.id).exists(nextHeight > 1 && nextHeight >= _)
        val block        = TestBlock.create(txs, if (isProto) Block.ProtoBlockVersion else Block.PlainBlockVersion)
        val checkedBlock = blockWithComputedStateHash(block.block, block.signer, bcu).resultE.explicitGet()

        differ(state, checkedBlock).map { result =>
          val snapshot = SnapshotOps.fromDiff(result.snapshot.toDiff(state), state).explicitGet()
          state.append(
            snapshot,
            result.carry,
            result.totalFee,
            None,
            checkedBlock.header.generationSignature.take(Block.HitSourceLength),
            result.computedStateHash,
            checkedBlock
          )
        }
      })
    }

  def assertBalanceInvariant(diff: Diff): Unit = {
    val portfolioDiff = diff.portfolios.values.fold(Portfolio())(_.combine(_).explicitGet())
    portfolioDiff.balance shouldBe 0
    portfolioDiff.effectiveBalance(false).explicitGet() shouldBe 0
    all(portfolioDiff.assets.values) shouldBe 0
  }

  def assertLeft(preconditions: Seq[BlockWithSigner], block: BlockWithSigner, fs: FunctionalitySettings = TFS.Enabled)(errorMessage: String): Unit =
    assertDiffEi(preconditions, block, fs)(_ should produce(errorMessage))

  def blockWithComputedStateHash(
      blockWithoutStateHash: Block,
      signer: KeyPair,
      blockchain: BlockchainUpdater & Blockchain
  ): TracedResult[ValidationError, Block] = {
    (if (blockchain.isFeatureActivated(TransactionStateSnapshot, blockchain.height + 1)) {
       val compBlockchain =
         SnapshotBlockchain(blockchain, StateSnapshot.empty, blockWithoutStateHash, ByteStr.empty, 0, blockchain.computeNextReward, None)
       val prevStateHash = blockchain.prevStateHash(Some(blockWithoutStateHash.header.reference))
       TracedResult(
         BlockDiffer
           .createInitialBlockSnapshot(
             blockchain,
             blockWithoutStateHash.header.reference,
             blockWithoutStateHash.header.generator.toAddress
           )
       )
         .flatMap { initSnapshot =>
           val initStateHash = BlockDiffer.computeInitialStateHash(compBlockchain, initSnapshot, prevStateHash)

           TxStateSnapshotHashBuilder
             .computeStateHash(
               blockWithoutStateHash.transactionData,
               initStateHash,
               initSnapshot,
               signer,
               blockchain.lastBlockTimestamp,
               blockWithoutStateHash.header.timestamp,
               blockWithoutStateHash.header.challengedHeader.isDefined,
               compBlockchain
             )
             .map(Some(_))
         }
     } else TracedResult(Right(None))).flatMap { stateHash =>
      TracedResult(
        Block
          .buildAndSign(
            version = blockWithoutStateHash.header.version,
            timestamp = blockWithoutStateHash.header.timestamp,
            reference = blockWithoutStateHash.header.reference,
            baseTarget = blockWithoutStateHash.header.baseTarget,
            generationSignature = blockWithoutStateHash.header.generationSignature,
            txs = blockWithoutStateHash.transactionData,
            featureVotes = blockWithoutStateHash.header.featureVotes,
            rewardVote = blockWithoutStateHash.header.rewardVote,
            signer = signer,
            stateHash = stateHash,
            challengedHeader = None
          )
      )
    }
  }
}

trait WithDomain extends WithState { _: Suite =>
  val DomainPresets = com.wavesplatform.test.DomainPresets
  import DomainPresets.*

  def domainSettingsWithFS(fs: FunctionalitySettings): WavesSettings =
    DomainPresets.domainSettingsWithFS(fs)

  def withDomain[A](
      settings: WavesSettings =
        DomainPresets.SettingsFromDefaultConfig.addFeatures(BlockchainFeatures.SmartAccounts), // SmartAccounts to allow V2 transfers by default
      balances: Seq[AddrWithBalance] = Seq.empty,
      wrapDB: RocksDB => RocksDB = identity
  )(test: Domain => A): A =
    withRocksDBWriter(settings) { blockchain =>
      var domain: Domain = null
      val bcu = new BlockchainUpdaterImpl(
        blockchain,
        settings,
        ntpTime,
        BlockchainUpdateTriggers.combined(domain.triggers),
        loadActiveLeases(rdb, _, _)
      )

      try {
        val wrappedDb = wrapDB(rdb.db)
        assert(wrappedDb.getNativeHandle == rdb.db.getNativeHandle, "wrap function should not create new database instance")
        domain = Domain(new RDB(wrappedDb, rdb.txMetaHandle, rdb.txHandle, rdb.txSnapshotHandle, Seq.empty), bcu, blockchain, settings)
        val genesis = balances.map { case AddrWithBalance(address, amount) =>
          TxHelpers.genesis(address, amount)
        }
        if (genesis.nonEmpty) {
          domain.appendBlock(
            createGenesisWithStateHash(
              genesis,
              bcu.isFeatureActivated(BlockchainFeatures.TransactionStateSnapshot, 1),
              Some(settings.blockchainSettings.genesisSettings.initialBaseTarget)
            )
          )
        }
        test(domain)
      } finally {
        domain.utxPool.close()
        bcu.shutdown()
      }
    }

  private val allVersions = DirectiveDictionary[StdLibVersion].all
  private val lastVersion = allVersions.last

  def testDomain(
      balances: Seq[AddrWithBalance] = Nil,
      from: StdLibVersion = V3,
      to: StdLibVersion = lastVersion
  )(assertion: (StdLibVersion, Domain) => Unit): Unit =
    allVersions
      .filter(v => v >= from && v <= to)
      .foreach(v => withDomain(DomainPresets.settingsForRide(v), balances)(assertion(v, _)))

  def createGenesisWithStateHash(txs: Seq[GenesisTransaction], txStateSnapshotActivated: Boolean, baseTarget: Option[Long] = None): Block = {
    val timestamp = txs.map(_.timestamp).max
    val genesisSettings = GenesisSettings(
      timestamp,
      timestamp,
      txs.map(_.amount.value).sum,
      None,
      txs.map { tx =>
        GenesisTransactionSettings(tx.recipient.toString, tx.amount.value)
      },
      baseTarget.getOrElse(2L),
      60.seconds
    )

    (for {
      txs <- genesisSettings.transactions.toList.map { gts =>
        for {
          address <- Address.fromString(gts.recipient)
          tx      <- GenesisTransaction.create(address, gts.amount, genesisSettings.timestamp)
        } yield tx
      }.sequence
      baseTarget = genesisSettings.initialBaseTarget
      timestamp  = genesisSettings.blockTimestamp
      block <- Block.buildAndSign(
        GenesisBlockVersion,
        timestamp,
        GenesisReference,
        baseTarget,
        GenesisGenerationSignature,
        txs,
        GenesisGenerator,
        Seq.empty,
        -1,
        Option.when(txStateSnapshotActivated)(TxStateSnapshotHashBuilder.createGenesisStateHash(txs)),
        None
      )
    } yield block).explicitGet()
  }
}

object WithState {
  case class AddrWithBalance(address: Address, balance: Long = ENOUGH_AMT)

  object AddrWithBalance {
    def enoughBalances(accs: KeyPair*): Seq[AddrWithBalance] =
      accs.map(acc => AddrWithBalance(acc.toAddress))

    implicit def toAddrWithBalance(v: (KeyPair, Long)): AddrWithBalance = AddrWithBalance(v._1.toAddress, v._2)
  }
}
