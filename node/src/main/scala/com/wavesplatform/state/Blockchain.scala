package com.wavesplatform.state

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, Transaction}

trait Blockchain {
  def settings: BlockchainSettings

  def height: Int
  def score: BigInt

  def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int, Int, ByteStr)]

  def lastBlock: Option[Block]
  def carryFee: Long

  def heightOf(blockId: ByteStr): Option[Int]

  def parentHeader(block: BlockHeader, back: Int = 1): Option[BlockHeader]

  /** Features related */
  def approvedFeatures: Map[Short, Int]
  def activatedFeatures: Map[Short, Int]
  def featureVotes(height: Int): Map[Short, Int]

  /** Block reward related */
  def blockReward(height: Int): Option[Long]
  def blockRewardVotes(height: Int): Seq[Long]

  def wavesAmount(height: Int): BigInt

  def transferById(id: ByteStr): Option[(Int, TransferTransaction)]
  def transactionInfo(id: ByteStr): Option[(Int, Transaction)]
  def transactionHeight(id: ByteStr): Option[Int]

  def containsTransaction(tx: Transaction): Boolean

  def assetDescription(id: IssuedAsset): Option[AssetDescription]

  def resolveAlias(a: Alias): Either[ValidationError, Address]

  def leaseDetails(leaseId: ByteStr): Option[LeaseDetails]

  def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee

  /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
  def balanceSnapshots(address: Address, from: Int, to: BlockId): Seq[BalanceSnapshot]

  def accountScript(address: Address): Option[(Script, Long)]
  def hasAccountScript(address: Address): Boolean

  def assetScript(id: IssuedAsset): Option[(Script, Long)]
  def hasAssetScript(id: IssuedAsset): Boolean

  def accountData(acc: Address, key: String): Option[DataEntry[_]]

  def leaseBalance(address: Address): LeaseBalance

  def balance(address: Address, mayBeAssetId: Asset = Waves): Long

  def collectActiveLeases(from: Int, to: Int)(filter: LeaseTransaction => Boolean): Seq[LeaseTransaction]

  /** Builds a new portfolio map by applying a partial function to all portfolios on which the function is defined.
    *
    * @note Portfolios passed to `pf` only contain Waves and Leasing balances to improve performance */
  def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A]
}
