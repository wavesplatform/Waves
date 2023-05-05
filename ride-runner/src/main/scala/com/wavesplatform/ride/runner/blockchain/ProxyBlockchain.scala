package com.wavesplatform.ride.runner.blockchain

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.blockchain.SignedBlockHeaderWithVrf
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.ride.runner.storage.{CacheKey, RideScriptRunRequest, SharedBlockchainStorage}
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.{
  AccountScriptInfo,
  AssetDescription,
  AssetScriptInfo,
  BalanceSnapshot,
  DataEntry,
  Height,
  LeaseBalance,
  TransactionId,
  TxMeta
}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.TxValidationError.AliasDoesNotExist

// TODO It seems we can remove this
class ProxyBlockchain(sharedBlockchain: SharedBlockchainStorage[RideScriptRunRequest]) extends SupportedBlockchain {
  override def settings: BlockchainSettings = sharedBlockchain.blockchainSettings

  // Ride: get*Value (data), get* (data)
  override def accountData(address: Address, key: String): Option[DataEntry[?]] = sharedBlockchain.getOrFetch(CacheKey.AccountData(address, key))

  // Ride: scriptHash
  override def accountScript(address: Address): Option[AccountScriptInfo] =
    sharedBlockchain.getOrFetch(CacheKey.AccountScript(address)).map(_.scriptInfo)

  // Ride: blockInfoByHeight, lastBlock
  override def blockHeader(height: Int): Option[SignedBlockHeader] = blockHeaderWithVrf(Height(height)).map(_.header)

  // Ride: blockInfoByHeight
  override def hitSource(height: Int): Option[ByteStr] = blockHeaderWithVrf(Height(height)).map(_.vrf)

  // TODO #?, tag)
  private def blockHeaderWithVrf(height: Height): Option[SignedBlockHeaderWithVrf] = sharedBlockchain.getOrFetchBlock(height)

  // Ride: wavesBalance, height, lastBlock
  override def height: Int = sharedBlockchain.heightUntagged

  override def activatedFeatures: Map[Short, Int] = sharedBlockchain.activatedFeatures

  // Ride: assetInfo
  override def assetDescription(id: Asset.IssuedAsset): Option[AssetDescription] =
    sharedBlockchain.getOrFetch(CacheKey.Asset(id)).map(_.assetDescription)

  // Ride (indirectly): asset script validation
  override def assetScript(id: Asset.IssuedAsset): Option[AssetScriptInfo] = assetDescription(id).flatMap(_.script)

  // Ride: get*Value (data), get* (data), isDataStorageUntouched, balance, scriptHash, wavesBalance
  override def resolveAlias(a: Alias): Either[ValidationError, Address] =
    sharedBlockchain.getOrFetch(CacheKey.Alias(a)).toRight(AliasDoesNotExist(a): ValidationError)

  // Ride: wavesBalance
  override def leaseBalance(address: Address): LeaseBalance =
    sharedBlockchain.getOrFetch(CacheKey.AccountLeaseBalance(address)).getOrElse(LeaseBalance.empty)

  // Ride: assetBalance, wavesBalance
  override def balance(address: Address, mayBeAssetId: Asset): Long =
    sharedBlockchain.getOrFetch(CacheKey.AccountBalance(address, mayBeAssetId)).getOrElse(0L)

  // Retrieves Waves balance snapshot in the [from, to] range (inclusive)
  // Ride: wavesBalance (specifies to=None), "to" always None and means "to the end"
  override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] = {
    // NOTE: This code leads to a wrong generating balance, but we see no use-cases for now
    val lb           = leaseBalance(address)
    val wavesBalance = balance(address, Asset.Waves)
    List(BalanceSnapshot(height, wavesBalance, lb.in, lb.out))
  }

  private def withTransactions(id: ByteStr): Option[Height] = sharedBlockchain.getOrFetch(CacheKey.Transaction(TransactionId(id)))

  // Ride: transactionHeightById
  override def transactionMeta(id: ByteStr): Option[TxMeta] = {
    // Other information is not used
    withTransactions(id).map(TxMeta(_, succeeded = true, 0))
  }
}
