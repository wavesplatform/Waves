package com.wavesplatform.history

import cats.syntax.option._
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.{AddressPortfolio, AddressTransactions}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.{DBExt, LevelDBWriter}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state._
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.{BlockchainUpdater, DiscardedTransactions, _}
import org.iq80.leveldb.DB

case class Domain(blockchainUpdater: BlockchainUpdaterImpl, levelDBWriter: LevelDBWriter) {
  import Domain._
  def effBalance(a: Address): Long = blockchainUpdater.effectiveBalance(a, 1000)

  def appendBlock(b: Block): Option[DiscardedTransactions] = blockchainUpdater.processBlock(b).explicitGet()

  def removeAfter(blockId: ByteStr): DiscardedBlocks = blockchainUpdater.removeAfter(blockId).explicitGet()

  def lastBlockId: ByteStr = blockchainUpdater.lastBlockId.get

  def carryFee: Long = blockchainUpdater.carryFee

  def balance(address: Address): Long               = blockchainUpdater.balance(address)
  def balance(address: Address, asset: Asset): Long = blockchainUpdater.balance(address, asset)

  def nftList(db: DB, address: Address): Seq[IssueTransaction] = db.withResource { resource =>
    AddressPortfolio
      .loadNftList(resource, address, blockchainUpdater.bestLiquidDiff.orEmpty, id => blockchainUpdater.assetDescription(id).exists(_.isNFT), None)
      .toSeq
  }

  def addressTransactions(db: DB, address: Address): Seq[(Height, Transaction)] = db.withResource { resource =>
    AddressTransactions
      .allAddressTransactions(
        resource,
        blockchainUpdater.bestLiquidDiff.map(diff => Height(blockchainUpdater.height) -> diff),
        address,
        None,
        Set.empty,
        None
      )
      .toSeq
  }
}

object Domain {
  implicit class BlockchainUpdaterExt[A <: BlockchainUpdater](bcu: A) {
    def processBlock(block: Block): Either[ValidationError, Option[DiscardedTransactions]] =
      bcu.processBlock(block, block.header.generationSignature)
  }
}
