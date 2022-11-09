package com.wavesplatform.storage.persistent

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.blockchain.{AccountDataKey, RemoteData}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, DataEntry, Portfolio, TransactionId, TxMeta}
import com.wavesplatform.transaction.{Asset, Transaction}

trait PersistentCaches {
  def accountDataEntries: PersistentCache[AccountDataKey, DataEntry[?]]
  def accountScripts: PersistentCache[Address, AccountScriptInfo]
  def assetDescriptions: PersistentCache[Asset.IssuedAsset, AssetDescription]
  def balances: PersistentCache[Address, Portfolio]
  def transactions: PersistentCache[TransactionId, (TxMeta, Option[Transaction])]

  def getBlockHeader(height: Int): RemoteData[SignedBlockHeader]
  def setBlockHeader(height: Int, data: RemoteData[SignedBlockHeader]): Unit

  def getHeight: Option[Int]
  def setHeight(data: Int): Unit

  def getVrf(height: Int): RemoteData[ByteStr]
  def setVrf(height: Int, data: RemoteData[ByteStr]): Unit

  def getActivatedFeatures(): RemoteData[Map[Short, Int]]
  def setActivatedFeatures(data: Map[Short, Int]): Unit

  def resolveAlias(alias: Alias): RemoteData[Address]
  def setAlias(alias: Alias, data: RemoteData[Address]): Unit
}
