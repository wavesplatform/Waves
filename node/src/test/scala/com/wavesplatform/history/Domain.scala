package com.wavesplatform.history

import cats.syntax.option._
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.{AddressPortfolio, AddressTransactions}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.{DBExt, LevelDBWriter}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{BlockchainUpdater, DiscardedTransactions, _}
import org.iq80.leveldb.DB

case class Domain(db: DB, blockchainUpdater: BlockchainUpdaterImpl, levelDBWriter: LevelDBWriter) {
  import Domain._
  def effBalance(a: Address): Long = blockchainUpdater.effectiveBalance(a, 1000)

  def appendBlock(b: Block): Option[DiscardedTransactions] = blockchainUpdater.processBlock(b).explicitGet()

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

  def addressTransactions(address: Address, from: Option[ByteStr] = None): Seq[(Height, Transaction)] = db.withResource { resource =>
    AddressTransactions
      .allAddressTransactions(
        resource,
        blockchainUpdater.bestLiquidDiff.map(diff => Height(blockchainUpdater.height) -> diff),
        address,
        None,
        Set.empty,
        from
      )
      .map { case (h, tx, _) => h -> tx }
      .toSeq
  }

  def portfolio(address: Address): Seq[(IssuedAsset, Long)] = Domain.portfolio(address, db, blockchainUpdater)

  def lastBlock: SignedBlockHeader = blockchainUpdater.lastBlockHeader.get
}

object Domain {
  implicit class BlockchainUpdaterExt[A <: BlockchainUpdater](bcu: A) {
    def processBlock(block: Block): Either[ValidationError, Option[DiscardedTransactions]] =
      bcu.processBlock(block, block.header.generationSignature)
  }

  def portfolio(address: Address, db: DB, blockchainUpdater: BlockchainUpdaterImpl): Seq[(IssuedAsset, Long)] = db.withResource { resource =>
    AddressPortfolio
      .assetBalanceIterator(resource, address, blockchainUpdater.bestLiquidDiff.orEmpty, id => blockchainUpdater.assetDescription(id).exists(!_.nft))
      .toSeq
  }
}
