package com.wavesplatform.ride.runner.blockchain

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{AssetScriptInfo, Blockchain, LeaseBalance, LeaseDetails, TxMeta, VolumeAndFee}
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import com.wavesplatform.transaction.{Asset, ERC20Address, Transaction}
import com.wavesplatform.utils.ScorexLogging

trait SupportedBlockchain extends Blockchain with ScorexLogging {
  // We don't support it for now (no demand), use GET /utils/script/evaluate if you need it.
  // Ride: isDataStorageUntouched
  override def hasData(address: Address): Boolean = kill(s"hasData($address)")

  // Ride: get*Value (data), get* (data)
  // override def accountData(address: Address, key: String): Option[DataEntry[?]]

  // Ride: scriptHash
  // override def accountScript(address: Address): Option[AccountScriptInfo]

  // Indirectly
  override def hasAccountScript(address: Address): Boolean = accountScript(address).nonEmpty

  // Ride: blockInfoByHeight, lastBlock
  // override def blockHeader(height: Int): Option[SignedBlockHeader]

  // Ride: blockInfoByHeight
  // override def hitSource(height: Int): Option[ByteStr]

  // Ride: blockInfoByHeight
  //  override def blockReward(height: Int): Option[Long] = kill("blockReward")

  // Ride: wavesBalance, height, lastBlock
  // override def height: Int = sharedBlockchain.heightUntagged

  // override def activatedFeatures: Map[Short, Int]

  // Ride: assetInfo
  // override def assetDescription(id: Asset.IssuedAsset): Option[AssetDescription]

  // Ride (indirectly): asset script validation
  override def assetScript(id: Asset.IssuedAsset): Option[AssetScriptInfo] = assetDescription(id).flatMap(_.script)

  // Ride: get*Value (data), get* (data), isDataStorageUntouched, balance, scriptHash, wavesBalance
  // override def resolveAlias(a: Alias): Either[ValidationError, Address]

  // Ride: wavesBalance
  // override def leaseBalance(address: Address): LeaseBalance

  // Ride: assetBalance, wavesBalance
  // override def balance(address: Address, mayBeAssetId: Asset): Long

  // Ride: accountWavesBalanceOf
  // Blockchain.effectiveBalance, BlockchainExt.hasBannedEffectiveBalance -> WavesEnvironment.accountWavesBalanceOf
  // override def effectiveBalanceBanHeights(address: Address): Seq[Int]

  // Retrieves Waves balance snapshot in the [from, to] range (inclusive)
  // Ride: wavesBalance (specifies to=None), "to" always None and means "to the end"
  // override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot]

  // Ride: transactionHeightById
  // override def transactionMeta(id: ByteStr): Option[TxMeta]

  // Ride: transferTransactionById
  // We don't support this, because there is no demand.
  override def transferById(id: ByteStr): Option[(Int, TransferTransactionLike)] = kill("transferById")

  // Ride: transactionById
  // We don't support his, because 1) there is no demand 2) it works only for V1 and V2 scripts, see versionSpecificFuncs in WavesContext
  override def transactionInfo(id: BlockId): Option[(TxMeta, Transaction)] = kill("transactionInfo")

  override def score: BigInt = kill("score")

  override def heightOf(blockId: ByteStr): Option[Int] = kill("heightOf")

  /** Features related */
  override def approvedFeatures: Map[Short, Int] = kill("approvedFeatures")

  override def featureVotes(height: Int): Map[Short, Int] = kill("featureVotes")

  override def containsTransaction(tx: Transaction): Boolean = kill("containsTransaction")

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = kill("leaseDetails")

  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee = kill("filledVolumeAndFee")

  override def blockRewardVotes(height: Int): Seq[Long] = kill("blockRewardVotes")

  override def wavesAmount(height: Int): BigInt = kill("wavesAmount")

  override def balanceAtHeight(address: Address, height: Int, assetId: Asset): Option[(Int, Long)] = kill("balanceAtHeight")

  // Not needed for now.
  // Return None, because it is used in AssetTransactionsDiff.issue, otherwise we can't issue assets in scripts.
  override def resolveERC20Address(address: ERC20Address): Option[Asset.IssuedAsset] = None

  override def carryFee(refId: Option[BlockId]): Long = kill("carryFee")

  override def transactionInfos(ids: Seq[BlockId]): Seq[Option[(TxMeta, Transaction)]] = kill("transactionInfos")

  override def leaseBalances(addresses: Seq[Address]): Map[Address, LeaseBalance] = kill("leaseBalances")

  override def balances(req: Seq[(Address, Asset)]): Map[(Address, Asset), Long] = kill("balances")

  override def wavesBalances(addresses: Seq[Address]): Map[Address, Long] = kill("wavesBalances")

  override def lastStateHash(refId: Option[BlockId]): BlockId = kill("lastStateHash")

  private def kill(methodName: String) = throw new RuntimeException(s"$methodName is not supported, contact with developers")
}
