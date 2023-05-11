package com.wavesplatform.ride.runner.blockchain

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{AssetScriptInfo, Blockchain, TxMeta, VolumeAndFee}
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import com.wavesplatform.transaction.{Asset, ERC20Address, Transaction}
import com.wavesplatform.utils.ScorexLogging

trait SupportedBlockchain extends Blockchain with ScorexLogging {
  // TODO #16 We don't support it for now, use GET /utils/script/evaluate
  // Ride: isDataStorageUntouched
  override def hasData(address: Address): Boolean = kill(s"hasData($address)")

  // Ride: get*Value (data), get* (data)
  // override def accountData(address: Address, key: String): Option[DataEntry[?]]

  // Ride: scriptHash
  // override def accountScript(address: Address): Option[AccountScriptInfo]

  // Indirectly
  override def hasAccountScript(address: Address): Boolean = accountScript(address).nonEmpty

  // Ride: blockInfoByHeight, lastBlock
//  override def blockHeader(height: Int): Option[SignedBlockHeader]

  // Ride: blockInfoByHeight
//  override def hitSource(height: Int): Option[ByteStr]

  // Ride: wavesBalance, height, lastBlock
//  override def height: Int = sharedBlockchain.heightUntagged

//  override def activatedFeatures: Map[Short, Int]

  // Ride: assetInfo
//  override def assetDescription(id: Asset.IssuedAsset): Option[AssetDescription]

  // Ride (indirectly): asset script validation
  override def assetScript(id: Asset.IssuedAsset): Option[AssetScriptInfo] = assetDescription(id).flatMap(_.script)

  // Ride: get*Value (data), get* (data), isDataStorageUntouched, balance, scriptHash, wavesBalance
//  override def resolveAlias(a: Alias): Either[ValidationError, Address]

  // Ride: wavesBalance
//  override def leaseBalance(address: Address): LeaseBalance

  // Ride: assetBalance, wavesBalance
//  override def balance(address: Address, mayBeAssetId: Asset): Long

  // Retrieves Waves balance snapshot in the [from, to] range (inclusive)
  // Ride: wavesBalance (specifies to=None), "to" always None and means "to the end"
//  override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot]

  // Ride: transactionHeightById
//  override def transactionMeta(id: ByteStr): Option[TxMeta]

  // Ride: transferTransactionById
  override def transferById(id: ByteStr): Option[(Int, TransferTransactionLike)] = kill("transferById")

  override def score: BigInt = kill("score")

  override def carryFee: Long = kill("carryFee")

  override def heightOf(blockId: ByteStr): Option[Int] = kill("heightOf")

  /** Features related */
  override def approvedFeatures: Map[Short, Int] = kill("approvedFeatures")

  override def featureVotes(height: Int): Map[Short, Int] = kill("featureVotes")

  override def containsTransaction(tx: Transaction): Boolean = kill("containsTransaction")

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = kill("leaseDetails")

  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee = kill("filledVolumeAndFee")

  override def transactionInfo(id: BlockId): Option[(TxMeta, Transaction)] = kill("transactionInfo")

  /** Block reward related */
  override def blockReward(height: Int): Option[Long] = kill("blockReward")

  override def blockRewardVotes(height: Int): Seq[Long] = kill("blockRewardVotes")

  override def wavesAmount(height: Int): BigInt = kill("wavesAmount")

  override def balanceAtHeight(address: Address, height: Int, assetId: Asset): Option[(Int, Long)] = kill("balanceAtHeight")

  // Not needed for now
  override def resolveERC20Address(address: ERC20Address): Option[Asset.IssuedAsset] = kill("resolveERC20Address")

  private def kill(methodName: String) = throw new RuntimeException(s"$methodName is not supported, contact with developers")
}
