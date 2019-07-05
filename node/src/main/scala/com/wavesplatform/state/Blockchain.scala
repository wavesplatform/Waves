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
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, Transaction, TransactionParser, TransactionParsers}
import com.wavesplatform.utils.CloseableIterator
import monix.reactive.Observable

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

  def portfolio(a: Address): Portfolio

  def transferById(id: ByteStr): Option[(Int, TransferTransaction)]
  def transactionInfo(id: ByteStr): Option[(Int, Transaction)]
  def transactionHeight(id: ByteStr): Option[Int]

  def nftList(address: Address, from: Option[IssuedAsset]): CloseableIterator[IssueTransaction]

  private[state] def addressTransactionsIterator(address: Address, types: Set[TransactionParser], fromId: Option[ByteStr]): CloseableIterator[(Height, Transaction)] =
    CloseableIterator.empty // Fix stub[Blockchain] in tests

  def addressTransactionsObs(address: Address, types: Set[TransactionParser], fromId: Option[ByteStr]): Observable[(Height, Transaction)] =
    Observable.defer {
      val iterator = addressTransactionsIterator(address, types, fromId)
      Observable.fromIterator(iterator, () => iterator.close())
    }

  def collectAddressTransactions[T](address: Address, types: Set[TransactionParser], fromId: Option[ByteStr], count: Int = Int.MaxValue)(
      pf: PartialFunction[(Height, Transaction), T]): Seq[T] = {
    addressTransactionsIterator(address, types, fromId)
      .collect { case heightAndTx if pf.isDefinedAt(heightAndTx) => pf(heightAndTx) }
      .closeAfter(_.toVector)
  }

  // Compatibility
  def addressTransactions(address: Address,
                          types: Set[Transaction.Type],
                          count: Int,
                          fromId: Option[ByteStr]): Either[String, Seq[(Height, Transaction)]] = {
    def createTransactionsList(): Seq[(Height, Transaction)] =
      collectAddressTransactions(address, TransactionParsers.forTypeSet(types), fromId, count) { case tx => tx }

    fromId match {
      case Some(id) => transactionInfo(id).toRight(s"Transaction $id does not exist").map(_ => createTransactionsList())
      case None     => Right(createTransactionsList())
    }
  }

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

  def assetDistribution(asset: IssuedAsset): AssetDistribution
  def assetDistributionAtHeight(asset: IssuedAsset,
                                height: Int,
                                count: Int,
                                fromAddress: Option[Address]): Either[ValidationError, AssetDistributionPage]
  def wavesDistribution(height: Int): Either[ValidationError, Map[Address, Long]]

  // the following methods are used exclusively by patches
  def collectActiveLeases[T](pf: PartialFunction[LeaseTransaction, T]): Seq[T]
  final def allActiveLeases: Seq[LeaseTransaction] = collectActiveLeases { case lt => lt }

  /** Builds a new portfolio map by applying a partial function to all portfolios on which the function is defined.
    *
    * @note Portfolios passed to `pf` only contain Waves and Leasing balances to improve performance */
  def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A]

  def invokeScriptResult(txId: TransactionId): Either[ValidationError, InvokeScriptResult]
}
