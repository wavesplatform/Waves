package com.wavesplatform.blockchain

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{
  AccountScriptInfo,
  AssetDescription,
  AssetScriptInfo,
  BalanceSnapshot,
  Blockchain,
  DataEntry,
  LeaseBalance,
  Portfolio,
  TransactionId,
  TxMeta,
  VolumeAndFee
}
import com.wavesplatform.transaction.TxValidationError.AliasDoesNotExist
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import com.wavesplatform.transaction.{Asset, ERC20Address, Transaction}
import com.wavesplatform.utils.ScorexLogging

class RideBlockchain[TagT](storage: SharedBlockchainStorage[TagT], tag: TagT) extends Blockchain with ScorexLogging {
  override def settings: BlockchainSettings = storage.settings

  // TODO use utils/evaluate through REST API
  // Ride: isDataStorageUntouched
  override def hasData(address: Address): Boolean = kill(s"hasData($address)") // data.contains(address)

  // Ride: get*Value (data), get* (data)
  override def accountData(address: Address, key: String): Option[DataEntry[?]] = storage.data.get(height, (address, key), tag)

  // Ride: scriptHash
  override def accountScript(address: Address): Option[AccountScriptInfo] = storage.accountScripts.get(height, address, tag)

  // Indirectly
  override def hasAccountScript(address: Address): Boolean = accountScript(address).nonEmpty

  // Ride: blockInfoByHeight, lastBlock
  override def blockHeader(height: Int): Option[SignedBlockHeader] = storage.getBlockHeader(height, tag)

  // Ride: blockInfoByHeight
  override def hitSource(height: Int): Option[ByteStr] = storage.getVrf(height, tag)

  // Ride: wavesBalance, height, lastBlock TODO: a binding in Ride?
  override def height: Int = storage.height

  override def activatedFeatures: Map[Short, Int] = storage.activatedFeatures

  // Ride: assetInfo
  override def assetDescription(id: Asset.IssuedAsset): Option[AssetDescription] = storage.assets.get(height, id, tag)

  // Ride (indirectly): asset script validation
  override def assetScript(id: Asset.IssuedAsset): Option[AssetScriptInfo] = assetDescription(id).flatMap(_.script)

  // Ride: get*Value (data), get* (data), isDataStorageUntouched, balance, scriptHash, wavesBalance
  override def resolveAlias(a: Alias): Either[ValidationError, Address] =
    storage.getAlias(a, tag).toRight(AliasDoesNotExist(a): ValidationError)

  private def withPortfolios(address: Address): Portfolio = storage.portfolios.get(height, address, tag).getOrElse(Portfolio.empty)

  // Ride: wavesBalance
  override def leaseBalance(address: Address): LeaseBalance = withPortfolios(address).lease

  // Ride: assetBalance, wavesBalance
  override def balance(address: Address, mayBeAssetId: Asset): Long = withPortfolios(address).balanceOf(mayBeAssetId)

  // Ride: wavesBalance (specifies to=None)
  /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
  override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] =
    // "to" always None
    // TODO this should work correctly
    List(BalanceSnapshot(height, withPortfolios(address)))
  // input.balanceSnapshots.getOrElse(address, Seq(BalanceSnapshot(height, 0, 0, 0))).filter(_.height >= from)

  private def withTransactions(id: ByteStr): Option[(TxMeta, Option[TransferTransactionLike])] =
    storage.transactions.getWithTransferLike(height, TransactionId(id), tag)

  // Ride: transactionHeightById
  override def transactionMeta(id: ByteStr): Option[TxMeta] = withTransactions(id).map(_._1)

  // Ride: transferTransactionById
  override def transferById(id: ByteStr): Option[(Int, TransferTransactionLike)] =
    withTransactions(id).flatMap { case (meta, tx) => tx.map((meta.height, _)) }

  override def score: BigInt = kill("score")

  override def carryFee: Long = kill("carryFee")

  override def heightOf(blockId: ByteStr): Option[Int] = kill("heightOf")

  /** Features related */
  override def approvedFeatures: Map[Short, Int] = kill("approvedFeatures")

  override def featureVotes(height: Int): Map[Short, Int] = kill("featureVotes")

  override def containsTransaction(tx: Transaction): Boolean = kill("containsTransaction")

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = kill("leaseDetails")

  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee = kill("filledVolumeAndFee")

  override def transactionInfo(id: BlockId) = kill("transactionInfo")

  /** Block reward related */
  override def blockReward(height: Int): Option[Long] = kill("blockReward")

  override def blockRewardVotes(height: Int): Seq[Long] = kill("blockRewardVotes")

  override def wavesAmount(height: Int): BigInt = kill("wavesAmount")

  override def balanceAtHeight(address: Address, height: Int, assetId: Asset): Option[(Int, Long)] = kill("balanceAtHeight")

  // GET /eth/assets
  override def resolveERC20Address(address: ERC20Address): Option[Asset.IssuedAsset] = kill("resolveERC20Address")

  private def kill(methodName: String) = throw new RuntimeException(methodName)
}
