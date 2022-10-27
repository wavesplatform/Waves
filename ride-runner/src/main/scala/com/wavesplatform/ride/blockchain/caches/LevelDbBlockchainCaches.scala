package com.wavesplatform.ride.blockchain.caches

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.ride.blockchain.BlockchainData
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, DataEntry, Portfolio, TxMeta}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import org.iq80.leveldb.DB

// TODO better name
class LevelDbBlockchainCaches(db: DB) extends BlockchainCaches {

  override def getAccountDataEntry(address: Address, key: String): BlockchainData[DataEntry[_]] = ???

  override def getAccountScript(address: Address): BlockchainData[AccountScriptInfo] = ???

  override def getBlockHeader(height: Int): BlockchainData[SignedBlockHeader] = ???

  override def getHeight: Option[Int] = ???

  override def getVrf(height: Int): BlockchainData[ByteStr] = ???

  override def getActivatedFeatures(height: Int): BlockchainData[Map[Short, Int]] = ???

  override def getAssetDescription(asset: Asset.IssuedAsset): BlockchainData[AssetDescription] = ???

  override def resolveAlias(alias: Alias): BlockchainData[Address] = ???

  override def getBalances(address: Address): BlockchainData[Portfolio] = ???

  override def getTransaction(id: ByteStr): BlockchainData[(TxMeta, Option[TransferTransactionLike])] = ???

  override def setAccountDataEntry(address: Address, key: String, data: BlockchainData[DataEntry[_]]): Unit = {}

  override def setAccountScript(address: Address, data: BlockchainData[AccountScriptInfo]): Unit = {}

  override def setBlockHeader(height: Int, data: BlockchainData[SignedBlockHeader]): Unit = {}

  override def setHeight(data: Int): Unit = {}

  override def setVrf(height: Int, data: BlockchainData[ByteStr]): Unit = {}

  override def setActivatedFeatures(height: Int, data: BlockchainData[Map[Short, Int]]): Unit = {}

  override def setAssetDescription(asset: Asset.IssuedAsset, data: BlockchainData[AssetDescription]): Unit = {}

  override def setAlias(alias: Alias, data: BlockchainData[Address]): Unit = {}

  override def setBalances(address: Address, data: BlockchainData[Portfolio]): Unit = {}

  override def setTransaction(id: ByteStr, data: BlockchainData[(TxMeta, Option[TransferTransactionLike])]): Unit = {}
}
