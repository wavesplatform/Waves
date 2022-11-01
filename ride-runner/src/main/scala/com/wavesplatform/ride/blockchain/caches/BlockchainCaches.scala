package com.wavesplatform.ride.blockchain.caches

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.ride.blockchain.BlockchainData
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, DataEntry, Portfolio, TransactionId, TxMeta}
import com.wavesplatform.transaction.{Asset, Transaction}

// TODO F abstraction? But getBalances?
trait BlockchainCaches {
  // TODO There is a pattern
  def getAccountDataEntry(address: Address, key: String, maxHeight: Int): BlockchainData[DataEntry[_]]
  def setAccountDataEntry(address: Address, key: String, height: Int, data: BlockchainData[DataEntry[_]]): Unit

  def getAccountScript(address: Address, maxHeight: Int): BlockchainData[AccountScriptInfo]
  def setAccountScript(address: Address, height: Int, data: BlockchainData[AccountScriptInfo]): Unit

  def getBlockHeader(height: Int): BlockchainData[SignedBlockHeader]
  def setBlockHeader(height: Int, data: BlockchainData[SignedBlockHeader]): Unit

  def getHeight: Option[Int]
  def setHeight(data: Int): Unit

  def getVrf(height: Int): BlockchainData[ByteStr]
  def setVrf(height: Int, data: BlockchainData[ByteStr]): Unit

  def getActivatedFeatures(): BlockchainData[Map[Short, Int]]
  def setActivatedFeatures(data: Map[Short, Int]): Unit

  def getAssetDescription(asset: Asset.IssuedAsset, maxHeight: Int): BlockchainData[AssetDescription]
  def setAssetDescription(asset: Asset.IssuedAsset, height: Int, data: BlockchainData[AssetDescription]): Unit

  def resolveAlias(alias: Alias): BlockchainData[Address]
  def setAlias(alias: Alias, data: BlockchainData[Address]): Unit

  def getBalances(address: Address, maxHeight: Int): BlockchainData[Portfolio]
  def setBalances(address: Address, height: Int, data: BlockchainData[Portfolio]): Unit

  def getTransaction(id: TransactionId): BlockchainData[(TxMeta, Option[Transaction])]
  def setTransaction(id: TransactionId, data: BlockchainData[(TxMeta, Option[Transaction])]): Unit
}

object EmptyBlockchainCaches extends BlockchainCaches {
  override def getAccountDataEntry(address: Address, key: String, maxHeight: Int): BlockchainData[DataEntry[_]]          = BlockchainData.Unknown
  override def setAccountDataEntry(address: Address, key: String, height: Int, data: BlockchainData[DataEntry[_]]): Unit = {}

  override def getAccountScript(address: Address, maxHeight: Int): BlockchainData[AccountScriptInfo]          = BlockchainData.Unknown
  override def setAccountScript(address: Address, height: Int, data: BlockchainData[AccountScriptInfo]): Unit = {}

  override def getBlockHeader(height: Int): BlockchainData[SignedBlockHeader]             = BlockchainData.Unknown
  override def setBlockHeader(height: Int, data: BlockchainData[SignedBlockHeader]): Unit = {}

  override def getHeight: Option[Int]     = None
  override def setHeight(data: Int): Unit = {}

  override def getVrf(height: Int): BlockchainData[ByteStr]             = BlockchainData.Unknown
  override def setVrf(height: Int, data: BlockchainData[ByteStr]): Unit = {}

  override def getActivatedFeatures(): BlockchainData[Map[Short, Int]] = BlockchainData.Unknown
  override def setActivatedFeatures(data: Map[Short, Int]): Unit       = {}

  override def getAssetDescription(asset: Asset.IssuedAsset, maxHeight: Int): BlockchainData[AssetDescription]          = BlockchainData.Unknown
  override def setAssetDescription(asset: Asset.IssuedAsset, height: Int, data: BlockchainData[AssetDescription]): Unit = {}

  override def resolveAlias(alias: Alias): BlockchainData[Address]         = BlockchainData.Unknown
  override def setAlias(alias: Alias, data: BlockchainData[Address]): Unit = {}

  override def getBalances(address: Address, maxHeight: Int): BlockchainData[Portfolio]          = BlockchainData.Unknown
  override def setBalances(address: Address, height: Int, data: BlockchainData[Portfolio]): Unit = {}

  override def getTransaction(id: TransactionId): BlockchainData[(TxMeta, Option[Transaction])]             = BlockchainData.Unknown
  override def setTransaction(id: TransactionId, data: BlockchainData[(TxMeta, Option[Transaction])]): Unit = {}
}
