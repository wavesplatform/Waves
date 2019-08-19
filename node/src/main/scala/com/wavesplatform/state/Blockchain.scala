package com.wavesplatform.state

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.extensions.{AddressTransactions, BlockchainExtensions, Distributions}
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, Transaction}

trait Blockchain {
  def settings: BlockchainSettings

  def height: Int
  def score: BigInt

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

  def parentHeader(block: BlockHeader, back: Int = 1): Option[BlockHeader]

  def totalFee(height: Int): Option[Long]

  /** Features related */
  def approvedFeatures: Map[Short, Int]
  def activatedFeatures: Map[Short, Int]
  def featureVotes(height: Int): Map[Short, Int]

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

  def accountScript(address: Address): Option[Script]
  def hasScript(address: Address): Boolean

  def assetScript(id: IssuedAsset): Option[Script]
  def hasAssetScript(id: IssuedAsset): Boolean

  def accountDataKeys(address: Address): Seq[String]
  def accountData(acc: Address, key: String): Option[DataEntry[_]]
  def accountData(acc: Address): AccountDataInfo

  def leaseBalance(address: Address): LeaseBalance

  def balance(address: Address, mayBeAssetId: Asset = Waves): Long

  // the following methods are used exclusively by patches
  def collectActiveLeases[T](pf: PartialFunction[LeaseTransaction, T]): Seq[T]
  final def allActiveLeases: Seq[LeaseTransaction] = collectActiveLeases { case lt => lt }

  /** Builds a new portfolio map by applying a partial function to all portfolios on which the function is defined.
    *
    * @note Portfolios passed to `pf` only contain Waves and Leasing balances to improve performance */
  def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A]

  def invokeScriptResult(txId: TransactionId): Either[ValidationError, InvokeScriptResult]
}

object Blockchain extends BlockchainExtensions {
  override implicit def addressTransactions(value: Blockchain): AddressTransactions = super.addressTransactions(value)

  override implicit def distributions(value: Blockchain): Distributions = super.distributions(value)
}
