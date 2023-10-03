package com.wavesplatform.history

import cats.implicits.catsSyntaxOption
import cats.syntax.traverse.*
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.*
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, BlockSnapshot, ChallengedHeader, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.consensus.{PoSCalculator, PoSSelector}
import com.wavesplatform.database.{DBExt, Keys, RDB, RocksDBWriter}
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.{BlockV5, RideV6, TransactionStateSnapshot}
import com.wavesplatform.history.SnapshotOps.TransactionStateSnapshotExt
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.mining.{BlockChallenger, BlockChallengerImpl}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.*
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult.{Applied, Ignored}
import com.wavesplatform.state.diffs.{BlockDiffer, TransactionDiffer}
import com.wavesplatform.state.reader.SnapshotBlockchain
import com.wavesplatform.test.TestTime
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{BlockchainUpdater, *}
import com.wavesplatform.utils.{EthEncoding, SystemTime}
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{Application, TestValues, crypto}
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.rocksdb.RocksDB
import org.scalatest.matchers.should.Matchers.*
import play.api.libs.json.{JsNull, JsValue, Json}

import scala.collection.immutable.SortedMap
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.util.Try
import scala.util.control.NonFatal

case class Domain(rdb: RDB, blockchainUpdater: BlockchainUpdaterImpl, rocksDBWriter: RocksDBWriter, settings: WavesSettings) {
  import Domain.*

  val blockchain: BlockchainUpdaterImpl = blockchainUpdater

  @volatile
  var triggers: Seq[BlockchainUpdateTriggers] = Nil

  val posSelector: PoSSelector = PoSSelector(blockchainUpdater, None)

  val transactionDiffer: Transaction => TracedResult[ValidationError, Diff] =
    TransactionDiffer(blockchain.lastBlockTimestamp, System.currentTimeMillis())(blockchain, _).map(_.toDiff(blockchain))

  val transactionDifferWithLog: Transaction => TracedResult[ValidationError, Diff] =
    TransactionDiffer(blockchain.lastBlockTimestamp, System.currentTimeMillis(), enableExecutionLog = true)(blockchain, _)
      .map(_.toDiff(blockchain))

  def createDiffE(tx: Transaction): Either[ValidationError, Diff] = transactionDiffer(tx).resultE
  def createDiff(tx: Transaction): Diff                           = createDiffE(tx).explicitGet()

  lazy val utxPool: UtxPoolImpl =
    new UtxPoolImpl(SystemTime, blockchain, settings.utxSettings, settings.maxTxErrorLogSize, settings.minerSettings.enable)
  lazy val wallet: Wallet = Wallet(settings.walletSettings.copy(file = None))

  lazy val testTime: TestTime = TestTime()
  lazy val blockAppender: Block => Task[Either[ValidationError, BlockApplyResult]] =
    BlockAppender(blockchain, testTime, utxPool, posSelector, Scheduler.singleThread("appender"))(_, None)
  lazy val blockChallenger: Option[BlockChallenger] =
    if (!settings.enableLightMode)
      Some(
        new BlockChallengerImpl(
          blockchain,
          new DefaultChannelGroup(GlobalEventExecutor.INSTANCE),
          wallet,
          settings,
          testTime,
          posSelector,
          blockAppender
        )
      )
    else None

  object commonApi {

    /** @return
      *   Tuple of (asset, feeInAsset, feeInWaves)
      * @see
      *   [[com.wavesplatform.state.diffs.FeeValidation#getMinFee(com.wavesplatform.state.Blockchain, com.wavesplatform.transaction.Transaction)]]
      */
    def calculateFee(tx: Transaction): (Asset, Long, Long) =
      transactions.calculateFee(tx).explicitGet()

    def calculateWavesFee(tx: Transaction): Long = {
      val (Waves, _, feeInWaves) = calculateFee(tx): @unchecked
      feeInWaves
    }

    def transactionMeta(transactionId: ByteStr): TransactionMeta =
      transactions
        .transactionById(transactionId)
        .getOrElse(throw new NoSuchElementException(s"No meta for $transactionId"))

    def invokeScriptResult(transactionId: ByteStr): InvokeScriptResult =
      transactionMeta(transactionId) match {
        case hsc: TransactionMeta.HasStateChanges => hsc.invokeScriptResult.get
        case _                                    => ???
      }

    def addressTransactions(address: Address): Seq[TransactionMeta] =
      transactions.transactionsByAddress(address, None, Set.empty, None).toListL.runSyncUnsafe()

    def commonTransactionsApi(challenger: Option[BlockChallenger]): CommonTransactionsApi =
      CommonTransactionsApi(
        blockchainUpdater.bestLiquidSnapshot.map(diff => Height(blockchainUpdater.height) -> diff),
        rdb,
        blockchain,
        utxPool,
        challenger,
        tx => Future.successful(utxPool.putIfNew(tx)),
        Application.loadBlockAt(rdb, blockchain)
      )

    lazy val transactions: CommonTransactionsApi = commonTransactionsApi(blockChallenger)
  }

  def liquidState: Option[NgState] = {
    val cls   = classOf[BlockchainUpdaterImpl]
    val field = cls.getDeclaredFields.find(_.getName.endsWith("ngState")).get
    field.setAccessible(true)
    field.get(blockchain).asInstanceOf[Option[NgState]]
  }

  def liquidAndSolidAssert(doCheck: () => Unit): Unit = {
    require(liquidState.isDefined, "No liquid state is present")
    try doCheck()
    catch { case NonFatal(err) => throw new RuntimeException("Liquid check failed", err) }
    makeStateSolid()
    try doCheck()
    catch { case NonFatal(err) => throw new RuntimeException("Solid check failed", err) }
  }

  def makeStateSolid(): (Int, SortedMap[String, String]) = {
    if (liquidState.isDefined) appendBlock() // Just append empty block
    (solidStateHeight, solidStateSnapshot())
  }

  def solidStateHeight: Int = {
    rdb.db.get(Keys.height)
  }

  def solidStateSnapshot(): SortedMap[String, String] = {
    val builder = SortedMap.newBuilder[String, String]
    rdb.db.iterateOver(Array.emptyByteArray, None)(e =>
      builder.addOne(EthEncoding.toHexString(e.getKey).drop(2) -> EthEncoding.toHexString(e.getValue).drop(2))
    )
    builder.result()
  }

  def lastBlock: Block = {
    blockchainUpdater.lastBlockId
      .flatMap(blockchainUpdater.liquidBlock)
      .orElse(rocksDBWriter.lastBlock)
      .getOrElse(TestBlock.create(Nil).block)
  }

  def liquidDiff: Diff =
    blockchainUpdater.bestLiquidSnapshot.orEmpty.toDiff(rocksDBWriter)

  def microBlocks: Vector[MicroBlock] = blockchain.microblockIds.reverseIterator.flatMap(blockchain.microBlock).to(Vector)

  def effBalance(a: Address): Long = blockchainUpdater.effectiveBalance(a, 1000)

  def appendBlock(b: Block): BlockApplyResult = blockchainUpdater.processBlock(b).explicitGet()

  def appendBlockE(b: Block, snapshot: Option[BlockSnapshot] = None): Either[ValidationError, BlockApplyResult] =
    blockchainUpdater.processBlock(b, snapshot)

  def rollbackTo(blockId: ByteStr): DiscardedBlocks = blockchainUpdater.removeAfter(blockId).explicitGet()

  def appendMicroBlock(b: MicroBlock): BlockId = blockchainUpdater.processMicroBlock(b, None).explicitGet()

  def appendMicroBlockE(b: MicroBlock): Either[ValidationError, BlockId] = blockchainUpdater.processMicroBlock(b, None)

  def lastBlockId: ByteStr = blockchainUpdater.lastBlockId.getOrElse(randomSig)

  def carryFee(refId: Option[ByteStr]): Long = blockchainUpdater.carryFee(refId)

  def balance(address: Address): Long               = blockchainUpdater.balance(address)
  def balance(address: Address, asset: Asset): Long = blockchainUpdater.balance(address, asset)

  def nftList(address: Address): Seq[(IssuedAsset, AssetDescription)] = rdb.db.withResource { resource =>
    AddressPortfolio
      .nftIterator(resource, address, blockchainUpdater.bestLiquidSnapshot.orEmpty, None, blockchainUpdater.assetDescription)
      .toSeq
      .flatten
  }

  def addressTransactions(address: Address, from: Option[ByteStr] = None): Seq[(Height, Transaction)] =
    AddressTransactions
      .allAddressTransactions(
        rdb,
        blockchainUpdater.bestLiquidSnapshot.map(diff => Height(blockchainUpdater.height) -> diff),
        address,
        None,
        Set.empty,
        from
      )
      .map { case (m, tx, _) => m.height -> tx }
      .toListL
      .runSyncUnsafe()

  def portfolio(address: Address): Seq[(IssuedAsset, Long)] = Domain.portfolio(address, rdb.db, blockchainUpdater)

  def appendAndAssertSucceed(txs: Transaction*): Block = {
    val block = createBlock(Block.PlainBlockVersion, txs)
    appendBlock(block)
    txs.foreach { tx =>
      if (!blockchain.transactionSucceeded(tx.id())) {
        val stateChanges = Try(commonApi.invokeScriptResult(tx.id())).toOption.flatMap(_.error).fold(JsNull: JsValue)(Json.toJson(_))
        throw new AssertionError(s"Should succeed: ${tx.id()}, script error: ${Json.prettyPrint(stateChanges)}")
      }
    }
    lastBlock
  }

  def appendAndCatchError(txs: Transaction*): ValidationError = {
    val block  = createBlock(Block.PlainBlockVersion, txs)
    val result = appendBlockE(block)
    txs.foreach { tx =>
      assert(blockchain.transactionInfo(tx.id()).isEmpty, s"should not pass: $tx")
    }
    result.left.getOrElse(throw new RuntimeException(s"Block appended successfully: $txs"))
  }

  def appendAndAssertFailed(txs: Transaction*): Block = {
    val block = createBlock(Block.PlainBlockVersion, txs)
    appendBlockE(block) match {
      case Left(err) =>
        throw new RuntimeException(s"Should be success: $err")

      case Right(_) =>
        txs.foreach(tx => assert(!blockchain.transactionSucceeded(tx.id()), s"should fail: $tx"))
        lastBlock
    }
  }

  def appendAndAssertFailed(tx: Transaction, message: String): Block = {
    appendBlock(tx)
    assert(!blockchain.transactionSucceeded(tx.id()), s"should fail: $tx")
    liquidDiff.errorMessage(tx.id()).get.text should include(message)
    lastBlock
  }

  def appendBlockE(txs: Transaction*): Either[ValidationError, BlockApplyResult] =
    createBlockE(Block.PlainBlockVersion, txs).flatMap(appendBlockE(_))

  def appendBlock(version: Byte, txs: Transaction*): Block = {
    val block = createBlock(version, txs)
    appendBlock(block)
    lastBlock
  }

  def appendBlock(txs: Transaction*): Block =
    appendBlock(Block.PlainBlockVersion, txs*)

  def appendKeyBlock(signer: KeyPair = defaultSigner, ref: Option[ByteStr] = None): Block = {
    val block = createBlock(
      Block.NgBlockVersion,
      Nil,
      ref.orElse(Some(lastBlockId)),
      generator = signer
    )
    appendBlock(block) match {
      case Applied(discardedSnapshots, _) =>
        utxPool.setPrioritySnapshots(discardedSnapshots)
        utxPool.cleanUnconfirmed()
      case Ignored => ()
    }

    lastBlock
  }

  def appendMicroBlockE(txs: Transaction*): Either[Throwable, BlockId] =
    Try(appendMicroBlock(txs*)).toEither

  def createMicroBlockE(stateHash: Option[ByteStr] = None, signer: Option[KeyPair] = None, ref: Option[ByteStr] = None)(
      txs: Transaction*
  ): Either[ValidationError, MicroBlock] = {
    val lastBlock   = this.lastBlock
    val blockSigner = signer.getOrElse(defaultSigner)
    val stateHashE = if (blockchain.isFeatureActivated(BlockchainFeatures.TransactionStateSnapshot)) {
      stateHash
        .map(Right(_))
        .getOrElse(
          TxStateSnapshotHashBuilder
            .computeStateHash(
              txs,
              lastBlock.header.stateHash.get,
              StateSnapshot.empty,
              blockSigner,
              rocksDBWriter.lastBlockTimestamp,
              blockchain.lastBlockTimestamp.get,
              isChallenging = false,
              blockchain
            )
            .resultE
        )
        .map(Some(_))
    } else Right(None)

    for {
      sh <- stateHashE
      block <- Block
        .buildAndSign(
          lastBlock.header.version,
          lastBlock.header.timestamp,
          lastBlock.header.reference,
          lastBlock.header.baseTarget,
          lastBlock.header.generationSignature,
          lastBlock.transactionData ++ txs,
          blockSigner,
          lastBlock.header.featureVotes,
          lastBlock.header.rewardVote,
          sh,
          None
        )
      microblock <- MicroBlock
        .buildAndSign(
          lastBlock.header.version,
          blockSigner,
          txs,
          ref.getOrElse(blockchainUpdater.lastBlockId.get),
          block.signature,
          block.header.stateHash
        )
    } yield microblock
  }

  def createMicroBlock(stateHash: Option[ByteStr] = None, signer: Option[KeyPair] = None, ref: Option[ByteStr] = None)(
      txs: Transaction*
  ): MicroBlock =
    createMicroBlockE(stateHash, signer, ref)(txs*).explicitGet()

  def appendMicroBlock(txs: Transaction*): BlockId = {
    val mb = createMicroBlock()(txs*)
    blockchainUpdater.processMicroBlock(mb, None).explicitGet()
  }

  def rollbackTo(height: Int): Unit = {
    val blockId = blockchain.blockId(height).get
    blockchainUpdater.removeAfter(blockId).explicitGet()
  }

  def rollbackMicros(offset: Int = 1): Unit = {
    val blockId =
      blockchainUpdater.microblockIds
        .drop(offset)
        .headOption
        .getOrElse(throw new IllegalStateException("Insufficient count of microblocks"))

    blockchainUpdater.removeAfter(blockId).explicitGet()
  }

  def createBlock(
      version: Byte,
      txs: Seq[Transaction],
      ref: Option[ByteStr] = blockchainUpdater.lastBlockId,
      strictTime: Boolean = false,
      generator: KeyPair = defaultSigner,
      stateHash: Option[Option[ByteStr]] = None,
      challengedHeader: Option[ChallengedHeader] = None,
      rewardVote: Long = -1L
  ): Block = createBlockE(version, txs, ref, strictTime, generator, stateHash, challengedHeader, rewardVote).explicitGet()

  def createBlockE(
      version: Byte,
      txs: Seq[Transaction],
      ref: Option[ByteStr] = blockchainUpdater.lastBlockId,
      strictTime: Boolean = false,
      generator: KeyPair = defaultSigner,
      stateHash: Option[Option[ByteStr]] = None,
      challengedHeader: Option[ChallengedHeader] = None,
      rewardVote: Long = -1L
  ): Either[ValidationError, Block] = {
    val reference = ref.getOrElse(randomSig)

    val parentHeight     = ref.flatMap(blockchain.heightOf).getOrElse(blockchain.height)
    val parent           = blockchain.blockHeader(parentHeight).map(_.header).getOrElse(lastBlock.header)
    val greatGrandParent = blockchain.blockHeader(parentHeight - 2).map(_.header)

    for {
      timestamp <-
        if (blockchain.height > 0)
          posSelector
            .getValidBlockDelay(blockchain.height, generator, parent.baseTarget, blockchain.balance(generator.toAddress) max 1e12.toLong)
            .map(_ + parent.timestamp)
        else
          Right(System.currentTimeMillis() - (1 hour).toMillis)
      consensus <-
        if (blockchain.height > 0)
          posSelector
            .consensusData(
              generator,
              parentHeight,
              settings.blockchainSettings.genesisSettings.averageBlockDelay,
              parent.baseTarget,
              parent.timestamp,
              greatGrandParent.map(_.timestamp),
              timestamp
            )
        else Right(NxtLikeConsensusBlockData(60, generationSignature))
      resultBt =
        if (blockchain.isFeatureActivated(BlockchainFeatures.FairPoS, parentHeight)) {
          consensus.baseTarget
        } else if (parentHeight % 2 != 0) parent.baseTarget
        else consensus.baseTarget.max(PoSCalculator.MinBaseTarget)
      blockWithoutStateHash <- Block
        .buildAndSign(
          version = if (consensus.generationSignature.size == 96) Block.ProtoBlockVersion else version,
          timestamp = if (strictTime) timestamp else SystemTime.getTimestamp(),
          reference = reference,
          baseTarget = resultBt,
          generationSignature = consensus.generationSignature,
          txs = txs,
          featureVotes = Nil,
          rewardVote = rewardVote,
          signer = generator,
          stateHash = None,
          challengedHeader = challengedHeader
        )
      resultStateHash <- stateHash.map(Right(_)).getOrElse {
        if (blockchain.isFeatureActivated(TransactionStateSnapshot, blockchain.height + 1)) {
          val blockchainWithNewBlock =
            SnapshotBlockchain(blockchain, StateSnapshot.empty, blockWithoutStateHash, ByteStr.empty, 0, blockchain.computeNextReward, None)
          val prevStateHash = blockchain.lastStateHash(Some(blockWithoutStateHash.header.reference))

          BlockDiffer
            .createInitialBlockSnapshot(blockchain, blockWithoutStateHash.header.reference, generator.toAddress)
            .flatMap { initSnapshot =>
              val initStateHash = BlockDiffer.computeInitialStateHash(blockchainWithNewBlock, initSnapshot, prevStateHash)

              TxStateSnapshotHashBuilder
                .computeStateHash(
                  txs,
                  initStateHash,
                  initSnapshot,
                  generator,
                  blockchain.lastBlockTimestamp,
                  blockWithoutStateHash.header.timestamp,
                  challengedHeader.nonEmpty,
                  blockchainWithNewBlock
                )
                .resultE
                .map(Some(_))
            }
        } else Right(None)
      }
      resultBlock <- Block
        .buildAndSign(
          version = if (consensus.generationSignature.size == 96) Block.ProtoBlockVersion else version,
          timestamp = if (strictTime) timestamp else SystemTime.getTimestamp(),
          reference = reference,
          baseTarget = resultBt,
          generationSignature = consensus.generationSignature,
          txs = txs,
          featureVotes = Nil,
          rewardVote = rewardVote,
          signer = generator,
          stateHash = resultStateHash,
          challengedHeader = challengedHeader
        )
    } yield resultBlock
  }

  def createChallengingBlock(
      challengingMiner: KeyPair,
      challengedBlock: Block,
      strictTime: Boolean = false,
      stateHash: Option[Option[ByteStr]] = None,
      ref: Option[ByteStr] = None,
      txs: Option[Seq[Transaction]] = None,
      challengedHeader: Option[ChallengedHeader] = None,
      timestamp: Option[Long] = None
  ): Block = {
    createBlock(
      Block.ProtoBlockVersion,
      txs.getOrElse(challengedBlock.transactionData),
      ref.orElse(blockchain.lastBlockId),
      strictTime = strictTime,
      generator = challengingMiner,
      stateHash = stateHash,
      challengedHeader = Some(
        challengedHeader.getOrElse(
          ChallengedHeader(
            challengedBlock.header.timestamp,
            challengedBlock.header.baseTarget,
            challengedBlock.header.generationSignature,
            Seq.empty,
            challengedBlock.sender,
            -1,
            challengedBlock.header.stateHash,
            challengedBlock.signature
          )
        )
      ),
      timestamp = timestamp
    )
  }

  val blocksApi: CommonBlocksApi = {
    def loadBlockMetaAt(db: RocksDB, blockchainUpdater: BlockchainUpdaterImpl)(height: Int): Option[BlockMeta] =
      Application.loadBlockMetaAt(db, blockchainUpdater)(height)

    def loadBlockInfoAt(db: RDB, blockchainUpdater: BlockchainUpdaterImpl)(
        height: Int
    ): Option[(BlockMeta, Seq[(TxMeta, Transaction)])] =
      Application.loadBlockInfoAt(db, blockchainUpdater)(height)

    CommonBlocksApi(blockchainUpdater, loadBlockMetaAt(rdb.db, blockchainUpdater), loadBlockInfoAt(rdb, blockchainUpdater))
  }

  // noinspection ScalaStyle
  object helpers {
    def creditWavesToDefaultSigner(amount: Long = 10_0000_0000): Unit = {
      import com.wavesplatform.transaction.utils.EthConverters.*
      appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress, amount), TxHelpers.genesis(TxHelpers.defaultSigner.toEthWavesAddress, amount))
    }

    def creditWavesFromDefaultSigner(to: Address, amount: Long = 1_0000_0000): Unit = {
      appendBlock(TxHelpers.transfer(to = to, amount = amount))
    }

    def issueAsset(issuer: KeyPair = defaultSigner, script: Script = null, amount: Long = 1000): IssuedAsset = {
      val transaction = TxHelpers.issue(issuer, script = Option(script), amount = amount)
      appendBlock(transaction)
      IssuedAsset(transaction.id())
    }

    def setScript(account: KeyPair, script: Script): Unit = {
      appendBlock(TxHelpers.setScript(account, script))
    }

    def setData(account: KeyPair, entries: DataEntry[?]*): Unit = {
      appendBlock(entries.map(TxHelpers.dataEntry(account, _))*)
    }

    def transfer(account: KeyPair, to: Address, amount: Long, asset: Asset): Unit = {
      appendBlock(TxHelpers.transfer(account, to, amount, asset))
    }

    def transferAll(account: KeyPair, to: Address, asset: Asset): Unit = {
      val balanceMinusFee = {
        val balance = blockchain.balance(account.toAddress, asset)
        if (asset == Waves) balance - TestValues.fee else balance
      }
      transfer(account, to, balanceMinusFee, asset)
    }
  }

  val transactionsApi: CommonTransactionsApi = CommonTransactionsApi(
    blockchainUpdater.bestLiquidSnapshot.map(Height(blockchainUpdater.height) -> _),
    rdb,
    blockchain,
    utxPool,
    blockChallenger,
    _ => Future.successful(TracedResult(Right(true))),
    h => blocksApi.blockAtHeight(h)
  )

  val accountsApi: CommonAccountsApi = CommonAccountsApi(
    () => blockchainUpdater.snapshotBlockchain,
    rdb,
    blockchain
  )

  val assetsApi: CommonAssetsApi = CommonAssetsApi(
    () => blockchainUpdater.bestLiquidSnapshot.orEmpty,
    rdb.db,
    blockchain
  )
}

object Domain {
  implicit class BlockchainUpdaterExt[A <: BlockchainUpdater & Blockchain](bcu: A) {
    def processBlock(block: Block, snapshot: Option[BlockSnapshot] = None): Either[ValidationError, BlockApplyResult] = {
      val hitSourcesE =
        if (bcu.height == 0 || !bcu.activatedFeaturesAt(bcu.height + 1).contains(BlockV5.id))
          Right(block.header.generationSignature -> block.header.challengedHeader.map(_.generationSignature))
        else {
          val parentHeight = bcu.heightOf(block.header.reference).getOrElse(bcu.height)

          val prevHs =
            if (bcu.isFeatureActivated(BlockchainFeatures.FairPoS, parentHeight) && parentHeight > 100)
              bcu.hitSource(parentHeight - 100).get
            else bcu.hitSource(parentHeight).get

          for {
            hs <- crypto
              .verifyVRF(block.header.generationSignature, prevHs.arr, block.header.generator, bcu.isFeatureActivated(RideV6, parentHeight))
            challengedHs <- block.header.challengedHeader.traverse(ch =>
              crypto.verifyVRF(ch.generationSignature, prevHs.arr, ch.generator, bcu.isFeatureActivated(RideV6, parentHeight))
            )
          } yield hs -> challengedHs
        }

      hitSourcesE.flatMap { case (hitSource, challengedHitSource) =>
        bcu.processBlock(block, hitSource, snapshot, challengedHitSource)
      }
    }
  }

  def portfolio(address: Address, db: RocksDB, blockchainUpdater: BlockchainUpdaterImpl): Seq[(IssuedAsset, Long)] = db.withResource { resource =>
    AddressPortfolio
      .assetBalanceIterator(
        resource,
        address,
        blockchainUpdater.bestLiquidSnapshot.orEmpty,
        id => blockchainUpdater.assetDescription(id).exists(!_.nft)
      )
      .toSeq
      .flatten
  }
}
