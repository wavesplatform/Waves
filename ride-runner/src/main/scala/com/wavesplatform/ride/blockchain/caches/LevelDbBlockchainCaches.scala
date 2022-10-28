package com.wavesplatform.ride.blockchain.caches

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{DBExt, Key}
import com.wavesplatform.ride.blockchain.BlockchainData
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, DataEntry, Portfolio, TxMeta}
import com.wavesplatform.transaction.{Asset, Transaction}
import org.iq80.leveldb.DB

// TODO better name
class LevelDbBlockchainCaches(db: DB) extends BlockchainCaches {
//  private def defaultHeight                             = 0 // TODO
//  private def getAddressId(address: Address): AddressId = ???

  override def getAccountDataEntry(address: Address, key: String): BlockchainData[DataEntry[_]] =
    readFromDb(CacheKeys.AccountDataEntries.mkKey((address, key)))

  override def setAccountDataEntry(address: Address, key: String, data: BlockchainData[DataEntry[_]]): Unit =
    writeToDb(CacheKeys.AccountDataEntries.mkKey((address, key)), data)

  override def getAccountScript(address: Address): BlockchainData[AccountScriptInfo] =
    readFromDb(CacheKeys.AccountScripts.mkKey(address))

  override def setAccountScript(address: Address, data: BlockchainData[AccountScriptInfo]): Unit =
    writeToDb(CacheKeys.AccountScripts.mkKey(address), data)

  override def getBlockHeader(height: Int): BlockchainData[SignedBlockHeader] =
    readFromDb(CacheKeys.SignedBlockHeaders.mkKey(height))

  override def setBlockHeader(height: Int, data: BlockchainData[SignedBlockHeader]): Unit =
    writeToDb(CacheKeys.SignedBlockHeaders.mkKey(height), data)

  private val heightDbKey                 = CacheKeys.Height.mkKey(())
  override def getHeight: Option[Int]     = db.readOnly(_.getOpt(heightDbKey))
  override def setHeight(data: Int): Unit = db.readWrite(_.put(heightDbKey, data))

  override def getVrf(height: Int): BlockchainData[ByteStr] =
    readFromDb(CacheKeys.VRF.mkKey(height))

  override def setVrf(height: Int, data: BlockchainData[ByteStr]): Unit =
    writeToDb(CacheKeys.VRF.mkKey(height), data)

  private val activatedFeaturesDbKey = CacheKeys.ActivatedFeatures.mkKey(())
  override def getActivatedFeatures(height: Int): BlockchainData[Map[Short, Int]] =
    db.readOnly(_.getOpt(activatedFeaturesDbKey)) match {
      case None     => BlockchainData.Unknown
      case Some(xs) => BlockchainData.Cached(xs)
    }

  override def setActivatedFeatures(height: Int, data: Map[Short, Int]): Unit =
    db.readWrite(_.put(activatedFeaturesDbKey, data))

  override def getAssetDescription(asset: Asset.IssuedAsset): BlockchainData[AssetDescription] =
    readFromDb(CacheKeys.AssetDescriptions.mkKey(asset))

  override def setAssetDescription(asset: Asset.IssuedAsset, data: BlockchainData[AssetDescription]): Unit =
    writeToDb(CacheKeys.AssetDescriptions.mkKey(asset), data)

  override def resolveAlias(alias: Alias): BlockchainData[Address] =
    readFromDb(CacheKeys.Aliases.mkKey(alias))

  override def setAlias(alias: Alias, data: BlockchainData[Address]): Unit =
    writeToDb(CacheKeys.Aliases.mkKey(alias), data)

  override def getBalances(address: Address): BlockchainData[Portfolio] =
    readFromDb(CacheKeys.Portfolios.mkKey(address))

  override def setBalances(address: Address, data: BlockchainData[Portfolio]): Unit =
    writeToDb(CacheKeys.Portfolios.mkKey(address), data)

  override def getTransaction(id: ByteStr): BlockchainData[(TxMeta, Option[Transaction])] =
    readFromDb(CacheKeys.Transactions.mkKey(id))

  override def setTransaction(id: ByteStr, data: BlockchainData[(TxMeta, Option[Transaction])]): Unit =
    writeToDb(CacheKeys.Transactions.mkKey(id), data)

  private def readFromDb[T](dbKey: Key[Option[T]]): BlockchainData[T] = {
    db.readOnly { ro =>
      val x = ro.getOpt(dbKey)
      x.fold[BlockchainData[T]](BlockchainData.Unknown)(BlockchainData.loaded)
    }
  }

  private def writeToDb[T](dbKey: Key[Option[T]], data: BlockchainData[T]): Unit =
    db.readWrite { rw =>
      if (data.loaded) rw.put(dbKey, data.mayBeValue)
      else rw.delete(dbKey)
    }
}
