package com.wavesplatform.history

import cats.implicits.toFoldableOps
import cats.syntax.either.*
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.*
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.consensus.{PoSCalculator, PoSSelector}
import com.wavesplatform.database.{DBExt, Keys, RDB, RocksDBWriter}
import com.wavesplatform.db.WithState
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.{BlockV5, RideV6, TransactionStateSnapshot}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.*
import com.wavesplatform.state.TxStateSnapshotHashBuilder.InitStateHash
import com.wavesplatform.state.diffs.BlockDiffer.CurrentBlockFeePart
import com.wavesplatform.state.diffs.{BlockDiffer, TransactionDiffer}
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{BlockchainUpdater, *}
import com.wavesplatform.utils.{EthEncoding, SystemTime}
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{Application, TestValues, crypto}
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
    TransactionDiffer(blockchain.lastBlockTimestamp, System.currentTimeMillis())(blockchain, _)

  val transactionDifferWithLog: Transaction => TracedResult[ValidationError, Diff] =
    TransactionDiffer(blockchain.lastBlockTimestamp, System.currentTimeMillis(), enableExecutionLog = true)(blockchain, _)

  def createDiffE(tx: Transaction): Either[ValidationError, Diff] = transactionDiffer(tx).resultE
  def createDiff(tx: Transaction): Diff                           = createDiffE(tx).explicitGet()

  lazy val utxPool: UtxPoolImpl =
    new UtxPoolImpl(SystemTime, blockchain, settings.utxSettings, settings.maxTxErrorLogSize, settings.minerSettings.enable)
  lazy val wallet: Wallet = Wallet(settings.walletSettings.copy(file = None))

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

    lazy val transactions: CommonTransactionsApi = CommonTransactionsApi(
      blockchainUpdater.bestLiquidDiff.map(diff => Height(blockchainUpdater.height) -> diff),
      rdb,
      blockchain,
      utxPool,
      tx => Future.successful(utxPool.putIfNew(tx)),
      Application.loadBlockAt(rdb, blockchain)
    )
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
    blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty)

  def microBlocks: Vector[MicroBlock] = blockchain.microblockIds.reverseIterator.flatMap(blockchain.microBlock).to(Vector)

  def effBalance(a: Address): Long = blockchainUpdater.effectiveBalance(a, 1000)

  def appendBlock(b: Block): Seq[Diff] = blockchainUpdater.processBlock(b).explicitGet()

  def appendBlockE(b: Block): Either[ValidationError, Seq[Diff]] = blockchainUpdater.processBlock(b)

  def rollbackTo(blockId: ByteStr): DiscardedBlocks = blockchainUpdater.removeAfter(blockId).explicitGet()

  def appendMicroBlock(b: MicroBlock): BlockId = blockchainUpdater.processMicroBlock(b).explicitGet()

  def lastBlockId: ByteStr = blockchainUpdater.lastBlockId.getOrElse(randomSig)

  def carryFee: Long = blockchainUpdater.carryFee

  def balance(address: Address): Long               = blockchainUpdater.balance(address)
  def balance(address: Address, asset: Asset): Long = blockchainUpdater.balance(address, asset)

  def nftList(address: Address): Seq[(IssuedAsset, AssetDescription)] = rdb.db.withResource { resource =>
    AddressPortfolio
      .nftIterator(resource, address, blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty), None, blockchainUpdater.assetDescription)
      .toSeq
      .flatten
  }

  def addressTransactions(address: Address, from: Option[ByteStr] = None): Seq[(Height, Transaction)] =
    AddressTransactions
      .allAddressTransactions(
        rdb,
        blockchainUpdater.bestLiquidDiff.map(diff => Height(blockchainUpdater.height) -> diff),
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

  def appendBlockE(txs: Transaction*): Either[ValidationError, Seq[Diff]] =
    createBlockE(Block.PlainBlockVersion, txs).flatMap(appendBlockE)

  def appendBlock(version: Byte, txs: Transaction*): Block = {
    val block = createBlock(version, txs)
    appendBlock(block)
    lastBlock
  }

  def appendBlock(txs: Transaction*): Block =
    appendBlock(Block.PlainBlockVersion, txs*)

  def appendKeyBlock(signer: Option[KeyPair] = None, ref: Option[ByteStr] = None): Block = {
    val block = createBlock(
      Block.NgBlockVersion,
      Nil,
      ref.orElse(Some(lastBlockId)),
      generator = signer.getOrElse(defaultSigner)
    )
    val discardedDiffs = appendBlock(block)
    utxPool.setPriorityDiffs(discardedDiffs)
    utxPool.cleanUnconfirmed()
    lastBlock
  }

  def appendMicroBlockE(txs: Transaction*): Either[Throwable, BlockId] =
    Try(appendMicroBlock(txs*)).toEither

  def createMicroBlockE(stateHash: Option[ByteStr] = None)(txs: Transaction*): Either[ValidationError, MicroBlock] = {
    val lastBlock = this.lastBlock
    val stateHashE = if (blockchain.isFeatureActivated(BlockchainFeatures.TransactionStateSnapshot)) {
      stateHash
        .map(Right(_))
        .getOrElse(
          WithState
            .computeStateHash(
              txs,
              lastBlock.header.stateHash.get,
              Diff.empty,
              defaultSigner.toAddress,
              lastBlock.header.timestamp,
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
          defaultSigner,
          lastBlock.header.featureVotes,
          lastBlock.header.rewardVote,
          sh
        )
      microblock <- MicroBlock
        .buildAndSign(
          lastBlock.header.version,
          defaultSigner,
          txs,
          blockchainUpdater.lastBlockId.get,
          block.signature,
          block.header.stateHash
        )
    } yield microblock
  }

  def createMicroBlock(stateHash: Option[ByteStr] = None)(txs: Transaction*): MicroBlock =
    createMicroBlockE(stateHash)(txs*).explicitGet()

  def appendMicroBlock(txs: Transaction*): BlockId = {
    val mb = createMicroBlock()(txs*)
    blockchainUpdater.processMicroBlock(mb).explicitGet()
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
      rewardVote: Long = -1L
  ): Block = createBlockE(version, txs, ref, strictTime, generator, stateHash, rewardVote).explicitGet()

  def createBlockE(
      version: Byte,
      txs: Seq[Transaction],
      ref: Option[ByteStr] = blockchainUpdater.lastBlockId,
      strictTime: Boolean = false,
      generator: KeyPair = defaultSigner,
      stateHash: Option[Option[ByteStr]] = None,
      rewardVote: Long = -1L
  ): Either[ValidationError, Block] = {
    val reference = ref.getOrElse(randomSig)
    val parent = ref
      .flatMap { bs =>
        val height = blockchain.heightOf(bs)
        height.flatMap(blockchain.blockHeader).map(_.header)
      }
      .getOrElse(lastBlock.header)

    val grandParent = ref.flatMap { bs =>
      val height = blockchain.heightOf(bs)
      height.flatMap(h => blockchain.blockHeader(h - 2)).map(_.header)
    }

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
              blockchain.height,
              settings.blockchainSettings.genesisSettings.averageBlockDelay,
              parent.baseTarget,
              parent.timestamp,
              grandParent.map(_.timestamp),
              timestamp
            )
        else Right(NxtLikeConsensusBlockData(60, generationSignature))
      resultBt =
        if (blockchain.isFeatureActivated(BlockchainFeatures.FairPoS, blockchain.height)) {
          consensus.baseTarget
        } else if (blockchain.height % 2 != 0) parent.baseTarget
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
          rewardVote = -1L,
          signer = generator,
          stateHash = None
        )
      resultStateHash <- stateHash.map(Right(_)).getOrElse {
        if (blockchain.isFeatureActivated(TransactionStateSnapshot, blockchain.height + 1)) {
          val blockchain    = CompositeBlockchain(this.blockchain, Diff.empty, blockWithoutStateHash, ByteStr.empty, 0, None)
          val prevStateHash = this.blockchain.lastBlockHeader.flatMap(_.header.stateHash).getOrElse(InitStateHash)

          val blockReward = if (this.blockchain.height == 0) 0 else this.settings.blockchainSettings.rewardsSettings.initial
          val carry       = if (this.blockchain.height == 0) 0 else this.carryFee

          BlockDiffer
            .createInitialBlockDiff(blockchain, generator.toAddress, blockchain.height, Some(blockReward), Some(carry))
            .leftMap(GenericError(_))
            .flatMap { initDiff =>
              val initStateHash =
                if (initDiff == Diff.empty) prevStateHash
                else TxStateSnapshotHashBuilder.createHashFromDiff(blockchain, initDiff).createHash(prevStateHash)

              WithState
                .computeStateHash(txs, initStateHash, initDiff, generator.toAddress, blockWithoutStateHash.header.timestamp, blockchain)
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
          stateHash = resultStateHash
        )
    } yield resultBlock
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
    blockchainUpdater.bestLiquidDiff.map(Height(blockchainUpdater.height) -> _),
    rdb,
    blockchain,
    utxPool,
    _ => Future.successful(TracedResult(Right(true))),
    h => blocksApi.blockAtHeight(h)
  )

  val accountsApi: CommonAccountsApi = CommonAccountsApi(
    () => blockchainUpdater.getCompositeBlockchain,
    rdb,
    blockchain
  )

  val assetsApi: CommonAssetsApi = CommonAssetsApi(
    () => blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty),
    rdb.db,
    blockchain
  )

  def calculateStateHash(
      prevStateHash: Option[ByteStr],
      signer: KeyPair,
      blockTs: TxTimestamp,
      txs: Transaction*
  ): Either[ValidationError, ByteStr] = {
    val feeFromPreviousBlockE = blocksApi.blockAtHeight(blockchain.height).fold(Portfolio.empty.asRight[String]) { case (_, txs) =>
      txs
        .map(_._2)
        .map { t =>
          val pf = Portfolio.build(t.assetFee)
          pf.minus(pf.multiply(CurrentBlockFeePart))
        }
        .foldM(Portfolio.empty)(_.combine(_))
    }
    val minerReward = blockchain.lastBlockReward.fold(Portfolio.empty)(Portfolio.waves)
    val totalReward = feeFromPreviousBlockE.flatMap(fee => minerReward.combine(fee)).explicitGet()

    val initStateHash = prevStateHash.getOrElse(TxStateSnapshotHashBuilder.InitStateHash)
    val txDiffer      = TransactionDiffer(blockchain.lastBlockTimestamp, blockTs) _
    txs
      .foldLeft[Either[ValidationError, (ByteStr, Diff)]](Right(initStateHash -> Diff(portfolios = Map(signer.toAddress -> totalReward)))) {
        case (Right((prevStateHash, accDiff)), tx) =>
          val compositeBlockchain = CompositeBlockchain(blockchain, accDiff)
          for {
            txDiff <- txDiffer(compositeBlockchain, tx).resultE
            stateHash = TxStateSnapshotHashBuilder.createHashFromDiff(compositeBlockchain, txDiff).createHash(prevStateHash)
            resDiff <- accDiff.combineF(txDiff).leftMap(GenericError(_))
          } yield (stateHash, resDiff)
        case (err @ Left(_), _) => err
      }
      .map(_._1)
  }
}

object Domain {
  implicit class BlockchainUpdaterExt[A <: BlockchainUpdater & Blockchain](bcu: A) {
    def processBlock(block: Block): Either[ValidationError, Seq[Diff]] = {
      val hitSource =
        if (bcu.height == 0 || !bcu.activatedFeaturesAt(bcu.height + 1).contains(BlockV5.id))
          block.header.generationSignature
        else {
          val hs = bcu.hitSource(bcu.height).get
          crypto.verifyVRF(block.header.generationSignature, hs.arr, block.header.generator, bcu.isFeatureActivated(RideV6)).explicitGet()
        }
      bcu.processBlock(block, hitSource, checkStateHash = false)
    }
  }

  def portfolio(address: Address, db: RocksDB, blockchainUpdater: BlockchainUpdaterImpl): Seq[(IssuedAsset, Long)] = db.withResource { resource =>
    AddressPortfolio
      .assetBalanceIterator(
        resource,
        address,
        blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty),
        id => blockchainUpdater.assetDescription(id).exists(!_.nft)
      )
      .toSeq
      .flatten
  }
}
