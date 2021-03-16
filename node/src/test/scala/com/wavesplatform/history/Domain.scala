package com.wavesplatform.history

import cats.syntax.option._
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.{AddressPortfolio, AddressTransactions}
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.{DBExt, LevelDBWriter}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state._
import com.wavesplatform.transaction.{BlockchainUpdater, _}
import com.wavesplatform.transaction.Asset.IssuedAsset
import org.iq80.leveldb.DB

//noinspection ScalaStyle
case class Domain(db: DB, blockchainUpdater: BlockchainUpdaterImpl, levelDBWriter: LevelDBWriter) {
  import Domain._

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

  def appendKeyBlock(ref: ByteStr = null): Block = {
    val block = createBlock(Block.NgBlockVersion, Nil, ref)
    appendBlock(block)
    lastBlock
  }

  def appendMicroBlock(txs: Transaction*): Unit = {
    val lastBlock = this.lastBlock
    val block     = lastBlock.copy(transactionData = lastBlock.transactionData ++ txs)
    val signature = com.wavesplatform.crypto.sign(defaultSigner.privateKey, block.bodyBytes())
    val mb        = MicroBlock.buildAndSign(lastBlock.header.version, defaultSigner, txs, blockchainUpdater.lastBlockId.get, signature).explicitGet()
    blockchainUpdater.processMicroBlock(mb)
  }

  def createBlock(version: Byte, txs: Seq[Transaction], ref: ByteStr = null): Block = {
    val reference = Option(ref).orElse(blockchainUpdater.lastBlockId).getOrElse(randomSig)
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
