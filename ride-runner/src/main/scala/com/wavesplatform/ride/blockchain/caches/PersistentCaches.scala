package com.wavesplatform.ride.blockchain.caches

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.ride.blockchain.{AccountDataKey, BlockchainData}
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, DataEntry, Portfolio, TransactionId, TxMeta}
import com.wavesplatform.transaction.{Asset, Transaction}

// TODO F abstraction? But getBalances?
trait PersistentCaches {
  def accountDataEntries: PersistentCache[AccountDataKey, DataEntry[?]]
  def accountScripts: PersistentCache[Address, AccountScriptInfo]
  def assetDescriptions: PersistentCache[Asset.IssuedAsset, AssetDescription]
  def balances: PersistentCache[Address, Portfolio]
  def transactions: PersistentCache[TransactionId, (TxMeta, Option[Transaction])]

  def getBlockHeader(height: Int): BlockchainData[SignedBlockHeader]
  def setBlockHeader(height: Int, data: BlockchainData[SignedBlockHeader]): Unit

  def getHeight: Option[Int]
  def setHeight(data: Int): Unit

  def getVrf(height: Int): BlockchainData[ByteStr]
  def setVrf(height: Int, data: BlockchainData[ByteStr]): Unit

  def getActivatedFeatures(): BlockchainData[Map[Short, Int]]
  def setActivatedFeatures(data: Map[Short, Int]): Unit

  def resolveAlias(alias: Alias): BlockchainData[Address]
  def setAlias(alias: Alias, data: BlockchainData[Address]): Unit
}
