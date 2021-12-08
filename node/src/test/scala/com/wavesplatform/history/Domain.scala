package com.wavesplatform.history

import scala.concurrent.Future
import scala.concurrent.duration._

import cats.syntax.option._
import com.wavesplatform.{database, Application}
import com.wavesplatform.account.Address
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.{AddressPortfolio, AddressTransactions, CommonBlocksApi, CommonTransactionsApi}
import com.wavesplatform.api.common.CommonTransactionsApi.TransactionMeta
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.{PoSCalculator, PoSSelector}
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.database.{DBExt, Keys, LevelDBWriter}
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.transaction.{BlockchainUpdater, _}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.utils.SystemTime
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observer
import org.iq80.leveldb.DB

case class Domain(db: DB, blockchainUpdater: BlockchainUpdaterImpl, levelDBWriter: LevelDBWriter, settings: WavesSettings) {
  import Domain._

  val blockchain: BlockchainUpdaterImpl = blockchainUpdater

  @volatile
  var triggers: Seq[BlockchainUpdateTriggers] = Nil

  val posSelector: PoSSelector = PoSSelector(blockchainUpdater, None)

  val transactionDiffer: Transaction => TracedResult[ValidationError, Diff] =
    TransactionDiffer(blockchain.lastBlockTimestamp, System.currentTimeMillis())(blockchain, _)

  lazy val utxPool = new UtxPoolImpl(SystemTime, blockchain, Observer.empty, settings.utxSettings)
  lazy val wallet  = Wallet(settings.walletSettings.copy(file = None))

  object commonApi {
    /**
      * @return Tuple of (asset, feeInAsset, feeInWaves)
      * @see [[com.wavesplatform.state.diffs.FeeValidation#getMinFee(com.wavesplatform.state.Blockchain, com.wavesplatform.transaction.Transaction)]]
      */
    def calculateFee(tx: Transaction): (Asset, TxAmount, TxAmount) =
      transactions.calculateFee(tx).explicitGet()

    def calculateWavesFee(tx: Transaction): TxAmount = {
      val (Waves, _, feeInWaves) = (calculateFee(tx): @unchecked)
      feeInWaves
    }

    def invokeScriptResult(transactionId: ByteStr): InvokeScriptResult =
      transactions.transactionById(transactionId).get.asInstanceOf[TransactionMeta.Invoke].invokeScriptResult.get

    def addressTransactions(address: Address): Seq[Transaction] =
      transactions.transactionsByAddress(address, None, Set.empty, None).map(_.transaction).toListL.runSyncUnsafe()

    lazy val transactions: CommonTransactionsApi = CommonTransactionsApi(
      blockchainUpdater.bestLiquidDiff.map(diff => Height(blockchainUpdater.height) -> diff),
      db,
      blockchain,
      utxPool,
      wallet,
      tx => Future.successful(utxPool.putIfNew(tx)),
      Application.loadBlockAt(db, blockchain)
    )
  }

  def lastBlock: Block = {
    blockchainUpdater.lastBlockId
      .flatMap(blockchainUpdater.liquidBlock)
      .orElse(levelDBWriter.lastBlock)
      .getOrElse(TestBlock.create(Nil))
  }

  def liquidDiff: Diff =
    blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty)

  def effBalance(a: Address): Long = blockchainUpdater.effectiveBalance(a, 1000)

  def appendBlock(b: Block): Seq[Diff] = blockchainUpdater.processBlock(b).explicitGet()

  def appendBlockE(b: Block): Either[ValidationError, Seq[Diff]] = blockchainUpdater.processBlock(b)

  def rollbackTo(blockId: ByteStr): DiscardedBlocks = blockchainUpdater.removeAfter(blockId).explicitGet()

  def appendMicroBlock(b: MicroBlock): BlockId = blockchainUpdater.processMicroBlock(b).explicitGet()

  def lastBlockId: ByteStr = blockchainUpdater.lastBlockId.getOrElse(randomSig)

  def carryFee: Long = blockchainUpdater.carryFee

  def balance(address: Address): Long               = blockchainUpdater.balance(address)
  def balance(address: Address, asset: Asset): Long = blockchainUpdater.balance(address, asset)

  def nftList(address: Address): Seq[(IssuedAsset, AssetDescription)] = db.withResource { resource =>
    AddressPortfolio
      .nftIterator(resource, address, blockchainUpdater.bestLiquidDiff.orEmpty, None, blockchainUpdater.assetDescription)
      .toSeq
  }

  def addressTransactions(address: Address, from: Option[ByteStr] = None): Seq[(Height, Transaction)] =
    AddressTransactions
      .allAddressTransactions(
        db,
        blockchainUpdater.bestLiquidDiff.map(diff => Height(blockchainUpdater.height) -> diff),
        address,
        None,
        Set.empty,
        from
      )
      .map { case (m, tx) => m.height -> tx }
      .toSeq

  def portfolio(address: Address): Seq[(IssuedAsset, Long)] = Domain.portfolio(address, db, blockchainUpdater)

  def appendAndAssertSucceed(txs: Transaction*): Block = {
    val block = createBlock(Block.PlainBlockVersion, txs)
    appendBlock(block)
    txs.foreach(tx => require(blockchain.transactionSucceeded(tx.id()), s"should succeed: $tx"))
    lastBlock
  }

  def appendAndAssertFailed(txs: Transaction*): Block = {
    val block = createBlock(Block.PlainBlockVersion, txs)
    appendBlock(block)
    txs.foreach(tx => require(!blockchain.transactionSucceeded(tx.id()), s"should fail: $tx"))
    lastBlock
  }

  def appendBlockE(txs: Transaction*): Either[ValidationError, Seq[Diff]] =
    appendBlockE(createBlock(blockchainUpdater.nextBlockVersion, txs))

  def appendBlock(txs: Transaction*): Block = {
    val block = createBlock(Block.PlainBlockVersion, txs)
    appendBlock(block)
    lastBlock
  }

  def appendKeyBlock(ref: Option[ByteStr] = None): Block = {
    val block = createBlock(Block.NgBlockVersion, Nil, ref.orElse(Some(lastBlockId)))
    appendBlock(block)
    lastBlock
  }

  def appendMicroBlock(txs: Transaction*): BlockId = {
    val lastBlock = this.lastBlock
    val block = Block
      .buildAndSign(
        lastBlock.header.version,
        lastBlock.header.timestamp,
        lastBlock.header.reference,
        lastBlock.header.baseTarget,
        lastBlock.header.generationSignature,
        lastBlock.transactionData ++ txs,
        defaultSigner,
        lastBlock.header.featureVotes,
        lastBlock.header.rewardVote
      )
      .explicitGet()
    val mb = MicroBlock.buildAndSign(lastBlock.header.version, defaultSigner, txs, blockchainUpdater.lastBlockId.get, block.signature).explicitGet()
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

  def createBlock(version: Byte, txs: Seq[Transaction], ref: Option[ByteStr] = blockchainUpdater.lastBlockId, strictTime: Boolean = false): Block = {
    val reference = ref.getOrElse(randomSig)
    val parent = ref.flatMap { bs =>
      val height = blockchain.heightOf(bs)
      height.flatMap(blockchain.blockHeader).map(_.header)
    } getOrElse (lastBlock.header)

    val grandParent = ref.flatMap { bs =>
      val height = blockchain.heightOf(bs)
      height.flatMap(h => blockchain.blockHeader(h - 2)).map(_.header)
    }

    val timestamp =
      if (blockchain.height > 0)
        parent.timestamp + posSelector
          .getValidBlockDelay(blockchain.height, defaultSigner, parent.baseTarget, blockchain.balance(defaultSigner.toAddress) max 1e12.toLong)
          .explicitGet()
      else
        System.currentTimeMillis() - (1 hour).toMillis

    val consensus =
      if (blockchain.height > 0)
        posSelector
          .consensusData(
            defaultSigner,
            blockchain.height,
            settings.blockchainSettings.genesisSettings.averageBlockDelay,
            parent.baseTarget,
            parent.timestamp,
            grandParent.map(_.timestamp),
            timestamp
          )
          .explicitGet()
      else NxtLikeConsensusBlockData(60, generationSignature)

    Block
      .buildAndSign(
        version = if (consensus.generationSignature.size == 96) Block.ProtoBlockVersion else version,
        timestamp = if (strictTime) timestamp else SystemTime.getTimestamp(),
        reference = reference,
        baseTarget = consensus.baseTarget.max(PoSCalculator.MinBaseTarget),
        generationSignature = consensus.generationSignature,
        txs = txs,
        featureVotes = Nil,
        rewardVote = -1L,
        signer = defaultSigner
      )
      .explicitGet()
  }

  val blocksApi: CommonBlocksApi = {
    def loadBlockMetaAt(db: DB, blockchainUpdater: BlockchainUpdaterImpl)(height: Int): Option[BlockMeta] =
      blockchainUpdater.liquidBlockMeta.filter(_ => blockchainUpdater.height == height).orElse(db.get(Keys.blockMetaAt(Height(height))))

    def loadBlockInfoAt(db: DB, blockchainUpdater: BlockchainUpdaterImpl)(
        height: Int
    ): Option[(BlockMeta, Seq[(TxMeta, Transaction)])] =
      loadBlockMetaAt(db, blockchainUpdater)(height).map { meta =>
        meta -> blockchainUpdater
          .liquidTransactions(meta.id)
          .getOrElse(db.readOnly(ro => database.loadTransactions(Height(height), ro)))
      }

    CommonBlocksApi(blockchainUpdater, loadBlockMetaAt(db, blockchainUpdater), loadBlockInfoAt(db, blockchainUpdater))
  }

  val transactionsApi: CommonTransactionsApi = CommonTransactionsApi(
    blockchainUpdater.bestLiquidDiff.map(Height(blockchainUpdater.height) -> _),
    db,
    blockchain,
    utxPool,
    wallet,
    _ => Future.successful(TracedResult(Right(true))),
    h => blocksApi.blockAtHeight(h)
  )
}

object Domain {
  implicit class BlockchainUpdaterExt[A <: BlockchainUpdater](bcu: A) {
    def processBlock(block: Block): Either[ValidationError, Seq[Diff]] =
      bcu.processBlock(block, block.header.generationSignature)
  }

  def portfolio(address: Address, db: DB, blockchainUpdater: BlockchainUpdaterImpl): Seq[(IssuedAsset, Long)] = db.withResource { resource =>
    AddressPortfolio
      .assetBalanceIterator(resource, address, blockchainUpdater.bestLiquidDiff.orEmpty, id => blockchainUpdater.assetDescription(id).exists(!_.nft))
      .toSeq
  }
}
