package com.wavesplatform.history

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.*
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, ChallengedHeader, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.consensus.{PoSCalculator, PoSSelector}
import com.wavesplatform.database.{DBExt, Keys, RDB, RocksDBWriter}
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
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{BlockchainUpdater, *}
import com.wavesplatform.utils.{EthEncoding, SystemTime}
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{Application, TestValues, crypto, database}
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
      .getOrElse(TestBlock.create(Nil))
  }

  def liquidDiff: Diff =
    blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty)

  def microBlocks: Vector[MicroBlock] = blockchain.microblockIds.reverseIterator.flatMap(blockchain.microBlock).to(Vector)

  def effBalance(a: Address): Long = blockchainUpdater.effectiveBalance(a, 1000)

  def appendBlock(b: Block): Seq[Diff] = blockchainUpdater.processBlock(b).explicitGet()

  def appendBlockE(b: Block): Either[ValidationError, Seq[Diff]] = blockchainUpdater.processBlock(b)

  def rollbackTo(blockId: ByteStr): DiscardedBlocks = blockchainUpdater.removeAfter(blockId).explicitGet()

  def appendMicroBlock(b: MicroBlock): BlockId = blockchainUpdater.processMicroBlock(b).explicitGet()

  def appendMicroBlockE(b: MicroBlock): Either[ValidationError, BlockId] = blockchainUpdater.processMicroBlock(b)

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
    appendBlockE(createBlock(Block.PlainBlockVersion, txs))

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
      generator = signer,
      stateHash = Some(
        this.lastBlock.header.stateHash.map(prev =>
          TxStateSnapshotHashBuilder
            .createHashFromDiff(
              this.blockchain,
              Diff(portfolios = Map(signer.toAddress -> Portfolio.waves(this.settings.blockchainSettings.rewardsSettings.initial)))
            )
            .createHash(prev)
        )
      )
    )
    val discardedDiffs = appendBlock(block)
    utxPool.setPriorityDiffs(discardedDiffs)
    utxPool.cleanUnconfirmed()
    lastBlock
  }

  def appendMicroBlockE(txs: Transaction*): Either[Throwable, BlockId] =
    Try(appendMicroBlock(txs*)).toEither

  def createMicroBlock(stateHash: Option[ByteStr], signer: Option[KeyPair], txs: Transaction*): MicroBlock = {
    val lastBlock = this.lastBlock
    val block = Block
      .buildAndSign(
        lastBlock.header.version,
        lastBlock.header.timestamp,
        lastBlock.header.reference,
        lastBlock.header.baseTarget,
        lastBlock.header.generationSignature,
        lastBlock.transactionData ++ txs,
        signer.getOrElse(defaultSigner),
        lastBlock.header.featureVotes,
        lastBlock.header.rewardVote,
        stateHash.orElse(lastBlock.header.stateHash),
        None
      )
      .explicitGet()
    MicroBlock
      .buildAndSign(
        lastBlock.header.version,
        signer.getOrElse(defaultSigner),
        txs,
        blockchainUpdater.lastBlockId.get,
        block.signature,
        block.header.stateHash
      )
      .explicitGet()
  }

  def appendMicroBlock(txs: Transaction*): BlockId = {
    val mb = createMicroBlock(None, None, txs*)
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
      challengedHeader: Option[ChallengedHeader] = None
  ): Block = {
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

    val timestamp =
      if (blockchain.height > 0)
        parent.timestamp + posSelector
          .getValidBlockDelay(blockchain.height, generator, parent.baseTarget, blockchain.balance(generator.toAddress) max 1e11.toLong)
          .explicitGet()
      else
        System.currentTimeMillis() - (1 hour).toMillis

    val consensus =
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
          .explicitGet()
      else NxtLikeConsensusBlockData(60, generationSignature)

    val resultStateHash = stateHash.getOrElse {
      if (blockchain.isFeatureActivated(TransactionStateSnapshot, blockchain.height + 1)) {
        val blockchain =
          if (this.blockchain.height == 0) this.blockchain
          else CompositeBlockchain(this.blockchain, Some(this.settings.blockchainSettings.rewardsSettings.initial))
        val prevStateHash = blockchain.lastBlockHeader.flatMap(_.header.stateHash).getOrElse(InitStateHash)

        val initDiff = BlockDiffer
          .createInitialBlockDiff(blockchain, generator.toAddress)
          .explicitGet()
        val initStateHash =
          if (initDiff == Diff.empty) prevStateHash
          else TxStateSnapshotHashBuilder.createHashFromDiff(blockchain, initDiff).createHash(prevStateHash)

        Some(computeStateHash(txs, initStateHash, initDiff, generator, timestamp, challengedHeader.nonEmpty, blockchain))
      } else None
    }

    val resultBt =
      if (blockchain.isFeatureActivated(BlockchainFeatures.FairPoS, blockchain.height + 1)) {
        consensus.baseTarget
      } else if (blockchain.height % 2 != 0) parent.baseTarget
      else consensus.baseTarget.max(PoSCalculator.MinBaseTarget)

    Block
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
        stateHash = resultStateHash,
        challengedHeader = challengedHeader
      )
      .explicitGet()
  }

  val blocksApi: CommonBlocksApi = {
    def loadBlockMetaAt(db: RocksDB, blockchainUpdater: BlockchainUpdaterImpl)(height: Int): Option[BlockMeta] =
      blockchainUpdater.liquidBlockMeta
        .filter(_ => blockchainUpdater.height == height)
        .orElse(db.get(Keys.blockMetaAt(Height(height))).flatMap(BlockMeta.fromPb))

    def loadBlockInfoAt(db: RocksDB, blockchainUpdater: BlockchainUpdaterImpl)(
        height: Int
    ): Option[(BlockMeta, Seq[(TxMeta, Transaction)])] =
      loadBlockMetaAt(db, blockchainUpdater)(height).map { meta =>
        meta -> blockchainUpdater
          .liquidTransactions(meta.id)
          .getOrElse(database.loadTransactions(Height(height), rdb))
      }

    CommonBlocksApi(blockchainUpdater, loadBlockMetaAt(rdb.db, blockchainUpdater), loadBlockInfoAt(rdb.db, blockchainUpdater))
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

  def computeStateHash(
      txs: Seq[Transaction],
      initStateHash: ByteStr,
      initDiff: Diff,
      signer: KeyPair,
      timestamp: Long,
      isChallenging: Boolean,
      blockchain: Blockchain
  ): ByteStr = {
    val txDiffer = TransactionDiffer(blockchain.lastBlockTimestamp, timestamp) _

    txs
      .foldLeft(initStateHash -> initDiff) { case ((prevStateHash, accDiff), tx) =>
        val compBlockchain = CompositeBlockchain(blockchain, accDiff)
        val minerDiff      = Diff(portfolios = Map(signer.toAddress -> Portfolio.waves(tx.fee).multiply(CurrentBlockFeePart)))
        txDiffer(compBlockchain, tx).resultE match {
          case Right(txDiff) =>
            val stateHash =
              TxStateSnapshotHashBuilder.createHashFromDiff(compBlockchain, txDiff.combineF(minerDiff).explicitGet()).createHash(prevStateHash)
            (stateHash, accDiff.combineF(txDiff).flatMap(_.combineF(minerDiff)).explicitGet())
          case Left(_) if isChallenging =>
            (prevStateHash, accDiff)
          case Left(err) => throw new RuntimeException(err.toString)
        }

      }
      ._1
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
      bcu.processBlock(block, hitSource)
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
