package com.wavesplatform.state

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.{Asset, Transaction, ValidationError}

trait Blockchain {
  def height: Int
  def score: BigInt
  def scoreOf(blockId: ByteStr): Option[BigInt]

  def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)]
  def blockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)]

  def lastBlock: Option[Block]
  def carryFee: Long
  def blockBytes(height: Int): Option[Array[Byte]]
  def blockBytes(blockId: ByteStr): Option[Array[Byte]]

  def heightOf(blockId: ByteStr): Option[Int]

  /** Returns the most recent block IDs, starting from the most recent  one */
  def lastBlockIds(howMany: Int): Seq[ByteStr]

  /** Returns a chain of blocks starting with the block with the given ID (from oldest to newest) */
  def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]]

  def parent(block: Block, back: Int = 1): Option[Block]

  /** Features related */
  def approvedFeatures: Map[Short, Int]
  def activatedFeatures: Map[Short, Int]
  def featureVotes(height: Int): Map[Short, Int]

  def portfolio(a: Address): Portfolio

  def transactionInfo(id: ByteStr): Option[(Int, Transaction)]
  def transactionHeight(id: ByteStr): Option[Int]

  def addressTransactions(address: Address,
                          types: Set[Transaction.Type],
                          count: Int,
                          fromId: Option[ByteStr]): Either[String, Seq[(Int, Transaction)]]

  def containsTransaction(tx: Transaction): Boolean

  def assetDescription(id: IssuedAsset): Option[AssetDescription]

  def resolveAlias(a: Alias): Either[ValidationError, Address]

  def leaseDetails(leaseId: ByteStr): Option[LeaseDetails]

  def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee

  /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
  def balanceSnapshots(address: Address, from: Int, to: BlockId): Seq[BalanceSnapshot]

  def accountScript(address: Address): Option[Script]
  def hasScript(address: Address): Boolean

  def assetScript(id: IssuedAsset): Option[Script]
  def hasAssetScript(id: IssuedAsset): Boolean

  def accountData(acc: Address): AccountDataInfo
  def accountData(acc: Address, key: String): Option[DataEntry[_]]

  def leaseBalance(address: Address): LeaseBalance

  def balance(address: Address, mayBeAssetId: Asset = Waves): Long

  def assetDistribution(asset: IssuedAsset): AssetDistribution
  def assetDistributionAtHeight(asset: IssuedAsset,
                                height: Int,
                                count: Int,
                                fromAddress: Option[Address]): Either[ValidationError, AssetDistributionPage]
  def wavesDistribution(height: Int): Either[ValidationError, Map[Address, Long]]

  // the following methods are used exclusively by patches
  def allActiveLeases: Set[LeaseTransaction]

  /** Builds a new portfolio map by applying a partial function to all portfolios on which the function is defined.
    *
    * @note Portfolios passed to `pf` only contain Waves and Leasing balances to improve performance */
  def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A]

  def append(diff: Diff, carryFee: Long, block: Block): Unit
  def rollbackTo(targetBlockId: ByteStr): Either[String, Seq[Block]]
}
