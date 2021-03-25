package com.wavesplatform.history

import cats.syntax.option._
import com.wavesplatform.account.Address
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.{AddressPortfolio, AddressTransactions, CommonBlocksApi}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database
import com.wavesplatform.database.{DBExt, Keys, LevelDBWriter}
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{BlockchainUpdater, _}
import org.iq80.leveldb.DB

case class Domain(db: DB, blockchainUpdater: BlockchainUpdaterImpl, levelDBWriter: LevelDBWriter) {
  import Domain._

  @volatile
  var triggers: Seq[BlockchainUpdateTriggers] = Nil

  def blockchain: BlockchainUpdaterImpl = blockchainUpdater

  def lastBlock: Block = {
    blockchainUpdater
      .liquidBlock(blockchainUpdater.lastBlockId.get)
      .orElse(levelDBWriter.lastBlock)
      .get
  }

  def liquidDiff: Diff =
    blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty)

  def effBalance(a: Address): Long = blockchainUpdater.effectiveBalance(a, 1000)

  def appendBlock(b: Block): Seq[Diff] = blockchainUpdater.processBlock(b).explicitGet()

  def removeAfter(blockId: ByteStr): DiscardedBlocks = blockchainUpdater.removeAfter(blockId).explicitGet()

  def appendMicroBlock(b: MicroBlock): BlockId = blockchainUpdater.processMicroBlock(b).explicitGet()

  def lastBlockId: ByteStr = blockchainUpdater.lastBlockId.get

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
      .map { case (h, tx, _) => h -> tx }
      .toSeq

  def portfolio(address: Address): Seq[(IssuedAsset, Long)] = Domain.portfolio(address, db, blockchainUpdater)

  def appendBlock(txs: Transaction*): Block = {
    val block = createBlock(Block.PlainBlockVersion, txs)
    appendBlock(block)
    lastBlock
  }

  def appendKeyBlock(): Block = {
    val block = createBlock(Block.NgBlockVersion, Nil)
    appendBlock(block)
    lastBlock
  }

  def appendMicroBlock(txs: Transaction*): Unit = {
    val lastBlock = this.lastBlock
    val block     = lastBlock.copy(transactionData = lastBlock.transactionData ++ txs)
    val signature = com.wavesplatform.crypto.sign(defaultSigner.privateKey, block.bodyBytes())
    val mb        = MicroBlock.buildAndSign(lastBlock.header.version, defaultSigner, txs, blockchainUpdater.lastBlockId.get, signature).explicitGet()
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

  def createBlock(version: Byte, txs: Seq[Transaction]): Block = {
    val reference = blockchainUpdater.lastBlockId.getOrElse(randomSig)
    val timestamp = System.currentTimeMillis()
    Block
      .buildAndSign(
        version = version,
        timestamp = timestamp,
        reference = reference,
        baseTarget = blockchainUpdater.lastBlockHeader.fold(60L)(_.header.baseTarget),
        generationSignature = com.wavesplatform.history.generationSignature,
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
    ): Option[(BlockMeta, Seq[(Transaction, Boolean)])] =
      loadBlockMetaAt(db, blockchainUpdater)(height).map { meta =>
        meta -> blockchainUpdater
          .liquidTransactions(meta.id)
          .orElse(db.readOnly(ro => database.loadTransactions(Height(height), ro)))
          .fold(Seq.empty[(Transaction, Boolean)])(identity)
      }

    CommonBlocksApi(blockchainUpdater, loadBlockMetaAt(db, blockchainUpdater), loadBlockInfoAt(db, blockchainUpdater))
  }
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
