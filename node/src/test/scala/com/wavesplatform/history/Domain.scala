package com.wavesplatform.history

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.*
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.consensus.{PoSCalculator, PoSSelector}
import com.wavesplatform.database.{DBExt, Keys, LevelDBWriter}
import com.wavesplatform.db.TestUtxPool
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{BlockchainUpdater, *}
import com.wavesplatform.utils.{EthEncoding, SystemTime}
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{TestValues, database}
import monix.execution.Scheduler.Implicits.global
import org.iq80.leveldb.DB
import org.scalatest.matchers.should.Matchers.*
import play.api.libs.json.{JsNull, JsValue, Json}

import scala.collection.immutable.SortedMap
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.util.Try
import scala.util.control.NonFatal

case class Domain(
    db: DB,
    blockchainUpdater: BlockchainUpdaterImpl,
    levelDBWriter: LevelDBWriter,
    settings: WavesSettings,
    beforeSetPriorityDiffs: () => Unit = () => ()
) {
  import Domain.*

  val blockchain: BlockchainUpdaterImpl = blockchainUpdater

  @volatile
  var triggers: Seq[BlockchainUpdateTriggers] = Nil

  val posSelector: PoSSelector = PoSSelector(blockchainUpdater, None)

  val transactionDiffer: Transaction => TracedResult[ValidationError, Diff] =
    TransactionDiffer(blockchain.lastBlockTimestamp, System.currentTimeMillis())(blockchain, _)

  def createDiffE(tx: Transaction): Either[ValidationError, Diff] = transactionDiffer(tx).resultE
  def createDiff(tx: Transaction): Diff                           = createDiffE(tx).explicitGet()

  lazy val utxPool        = new TestUtxPool(SystemTime, blockchain, settings.utxSettings, settings.minerSettings.enable, beforeSetPriorityDiffs)
  lazy val wallet: Wallet = Wallet(settings.walletSettings.copy(file = None))

  def blockchainWithDiscardedDiffs(): CompositeBlockchain = {
    def bc = CompositeBlockchain(blockchain, utxPool.discardedMicrosDiff())
    utxPool.priorityPool.optimisticRead(bc)(_ => true)
  }

  object commonApi {

    /** @return
      *   Tuple of (asset, feeInAsset, feeInWaves)
      * @see
      *   [[com.wavesplatform.state.diffs.FeeValidation#getMinFee(com.wavesplatform.state.Blockchain, com.wavesplatform.transaction.Transaction)]]
      */
    def calculateFee(tx: Transaction): (Asset, Long, Long) =
      transactionsApi.calculateFee(tx).explicitGet()

    def calculateWavesFee(tx: Transaction): Long = {
      val (Waves, _, feeInWaves) = calculateFee(tx): @unchecked
      feeInWaves
    }

    def transactionMeta(transactionId: ByteStr): TransactionMeta =
      transactionsApi
        .transactionById(transactionId)
        .getOrElse(throw new NoSuchElementException(s"No meta for $transactionId"))

    def invokeScriptResult(transactionId: ByteStr): InvokeScriptResult =
      transactionMeta(transactionId) match {
        case hsc: TransactionMeta.HasStateChanges => hsc.invokeScriptResult.get
        case _                                    => ???
      }

    def addressTransactions(address: Address): Seq[TransactionMeta] =
      transactionsApi.transactionsByAddress(address, None, Set.empty, None).toListL.runSyncUnsafe()
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
    db.get(Keys.height)
  }

  def solidStateSnapshot(): SortedMap[String, String] = {
    val builder = SortedMap.newBuilder[String, String]
    db.iterateOver(Array.emptyByteArray)(e =>
      builder.addOne(EthEncoding.toHexString(e.getKey).drop(2) -> EthEncoding.toHexString(e.getValue).drop(2))
    )
    builder.result()
  }

  def lastBlock: Block = {
    blockchainUpdater.lastBlockId
      .flatMap(blockchainUpdater.liquidBlock)
      .orElse(levelDBWriter.lastBlock)
      .getOrElse(TestBlock.create(Nil))
  }

  def liquidDiff: Diff = {
    def liquidDiff = blockchainUpdater.bestLiquidDiff.getOrElse(Diff())
    def totalDiff  = liquidDiff.combineE(utxPool.discardedMicrosDiff()).explicitGet()
    utxPool.priorityPool.optimisticRead(totalDiff)(_ => true)
  }

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

  def nftList(address: Address): Seq[(IssuedAsset, AssetDescription)] = db.withResource { resource =>
    AddressPortfolio
      .nftIterator(resource, address, liquidDiff, None, blockchainUpdater.assetDescription)
      .toSeq
  }

  def addressTransactions(address: Address, from: Option[ByteStr] = None): Seq[(Height, Transaction)] =
    transactionsApi
      .transactionsByAddress(address, None, Set(), from)
      .map(meta => (meta.height, meta.transaction))
      .toListL
      .runSyncUnsafe()

  def portfolio(address: Address): Seq[(IssuedAsset, Long)] =
    db.withResource { resource =>
      AddressPortfolio
        .assetBalanceIterator(
          resource,
          address,
          liquidDiff,
          id => blockchainUpdater.assetDescription(id).exists(!_.nft)
        )
        .toSeq
    }

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

  def appendBlock(txs: Transaction*): Block = {
    val block = createBlock(Block.PlainBlockVersion, txs)
    appendBlock(block)
    lastBlock
  }

  def appendKeyBlock(ref: Option[ByteStr] = None): Block = {
    val block          = createBlock(Block.NgBlockVersion, Nil, ref.orElse(Some(lastBlockId)))
    val discardedDiffs = appendBlock(block)
    utxPool.setPriorityDiffs(discardedDiffs)
    utxPool.cleanUnconfirmed()
    lastBlock
  }

  def appendMicroBlockE(txs: Transaction*): Either[Throwable, BlockId] =
    Try(appendMicroBlock(txs*)).toEither

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

  def createBlock(
      version: Byte,
      txs: Seq[Transaction],
      ref: Option[ByteStr] = blockchainUpdater.lastBlockId,
      strictTime: Boolean = false,
      generator: KeyPair = defaultSigner
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
          .getValidBlockDelay(blockchain.height, generator, parent.baseTarget, blockchain.balance(generator.toAddress) max 1e12.toLong)
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
        signer = generator
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
    Some(Height(blockchainUpdater.height) -> liquidDiff),
    db,
    () => blockchainWithDiscardedDiffs(),
    utxPool,
    tx => Future.successful(utxPool.putIfNew(tx)),
    h => blocksApi.blockAtHeight(h)
  )

  val accountsApi: CommonAccountsApi = CommonAccountsApi(
    () => liquidDiff,
    db,
    () => blockchainWithDiscardedDiffs()
  )

  val assetsApi: CommonAssetsApi = CommonAssetsApi(
    () => liquidDiff,
    db,
    () => blockchainWithDiscardedDiffs()
  )
}

object Domain {
  implicit class BlockchainUpdaterExt[A <: BlockchainUpdater](bcu: A) {
    def processBlock(block: Block): Either[ValidationError, Seq[Diff]] =
      bcu.processBlock(block, block.header.generationSignature)
  }
}
