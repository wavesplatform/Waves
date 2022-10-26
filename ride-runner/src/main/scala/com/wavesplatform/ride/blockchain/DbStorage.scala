package com.wavesplatform.ride.blockchain

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, DataEntry, Portfolio, TxMeta}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.transfer.TransferTransactionLike

// TODO F abstraction? But getBalances?
trait DbStorage {
  def getAccountDataEntry(address: Address, key: String): BlockchainData[DataEntry[_]]
  def getAccountScript(address: Address): BlockchainData[AccountScriptInfo]
  def getBlockHeader(height: Int): BlockchainData[SignedBlockHeader]
  def getVrf(height: Int): BlockchainData[ByteStr]
  def getActivatedFeatures(height: Int): BlockchainData[Map[Short, Int]]
  def getAssetDescription(asset: Asset.IssuedAsset): BlockchainData[AssetDescription]
  def resolveAlias(alias: Alias): BlockchainData[Address]
  def getBalances(address: Address): BlockchainData[Portfolio]
  def getTransaction(id: ByteStr): BlockchainData[(TxMeta, Option[TransferTransactionLike])]
}

class EmptyDbStorage() extends DbStorage {
  override def getAccountDataEntry(address: Address, key: String): BlockchainData[DataEntry[_]]       = BlockchainData.Unknown
  override def getAccountScript(address: Address): BlockchainData[AccountScriptInfo]                  = BlockchainData.Unknown
  override def getBlockHeader(height: Int): BlockchainData[SignedBlockHeader]                         = BlockchainData.Unknown
  override def getVrf(height: Int): BlockchainData[ByteStr]                                           = BlockchainData.Unknown
  override def getActivatedFeatures(height: Int): BlockchainData[Map[Short, Int]]                     = BlockchainData.Unknown
  override def getAssetDescription(asset: Asset.IssuedAsset): BlockchainData[AssetDescription]        = BlockchainData.Unknown
  override def resolveAlias(alias: Alias): BlockchainData[Address]                                    = BlockchainData.Unknown
  override def getBalances(address: Address): BlockchainData[Portfolio]                               = BlockchainData.Unknown
  override def getTransaction(id: ByteStr): BlockchainData[(TxMeta, Option[TransferTransactionLike])] = BlockchainData.Unknown
}
