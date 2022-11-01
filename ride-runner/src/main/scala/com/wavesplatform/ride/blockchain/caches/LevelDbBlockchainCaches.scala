package com.wavesplatform.ride.blockchain.caches

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{AddressId, DBExt, Key}
import com.wavesplatform.ride.blockchain.BlockchainData
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, DataEntry, Portfolio, TxMeta}
import com.wavesplatform.transaction.{Asset, Transaction}
import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.DB

import java.util.concurrent.atomic.AtomicLong
import scala.util.chaining.scalaUtilChainingOps

// TODO unify with BlockchainGrpcApi? Or create a bridge?
// TODO better name
class LevelDbBlockchainCaches(db: DB) extends BlockchainCaches with ScorexLogging {
  private val lastAddressIdKey = CacheKeys.LastAddressId.mkKey(())
  private val lastAddressId = new AtomicLong(db.readOnly(_.getOpt(lastAddressIdKey).getOrElse(-1L)))

  // TODO caching from NODE
  private def getOrMkAddressId(address: Address): AddressId = db.readWrite { rw =>
    val key = CacheKeys.AddressIds.mkKey(address)
    rw.getOpt(key) match {
      case Some(r) => r
      case None =>
        val newId = AddressId(lastAddressId.incrementAndGet())
        log.trace(s"getOrMkAddressId($address): new $newId")
        rw.put(key, newId)
        rw.put(lastAddressIdKey, newId)
        newId
    }
  }

  override def getAccountDataEntry(address: Address, key: String): BlockchainData[DataEntry[_]] =
    readFromDb(CacheKeys.AccountDataEntries.mkKey((getOrMkAddressId(address), key))).tap { r =>
      log.trace(s"getAccountDataEntry($address, '$key'): ${r.toFoundStr("value", _.value)}")
    }

  override def setAccountDataEntry(address: Address, key: String, data: BlockchainData[DataEntry[_]]): Unit = {
    writeToDb(CacheKeys.AccountDataEntries.mkKey((getOrMkAddressId(address), key)), data)
    log.trace(s"setAccountDataEntry($address, '$key')")
  }

  override def getAccountScript(address: Address): BlockchainData[AccountScriptInfo] =
    readFromDb(CacheKeys.AccountScripts.mkKey(getOrMkAddressId(address))).tap { r =>
      log.trace(s"getAccountScript($address): ${r.toFoundStr("hash", _.script.hashCode())}")
    }

  override def setAccountScript(address: Address, data: BlockchainData[AccountScriptInfo]): Unit = {
    writeToDb(CacheKeys.AccountScripts.mkKey(getOrMkAddressId(address)), data)
    log.trace(s"setAccountScript($address)")
  }

  override def getBlockHeader(height: Int): BlockchainData[SignedBlockHeader] =
    readFromDb(CacheKeys.SignedBlockHeaders.mkKey(height)).tap { r =>
      log.trace(s"getBlockHeader($height): ${r.toFoundStr("id", _.id())}")
    }

  override def setBlockHeader(height: Int, data: BlockchainData[SignedBlockHeader]): Unit = {
    writeToDb(CacheKeys.SignedBlockHeaders.mkKey(height), data)
    log.trace(s"setBlockHeader($height)")
  }

  private val heightDbKey = CacheKeys.Height.mkKey(())
  override def getHeight: Option[Int] = db.readOnly(_.getOpt(heightDbKey)).tap { r =>
    log.trace(s"getHeight: $r")
  }
  override def setHeight(data: Int): Unit = {
    db.readWrite(_.put(heightDbKey, data))
    log.trace(s"setHeight($data)")
  }

  override def getVrf(height: Int): BlockchainData[ByteStr] =
    readFromDb(CacheKeys.VRF.mkKey(height)).tap { r =>
      log.trace(s"getVrf($height): $r")
    }

  override def setVrf(height: Int, data: BlockchainData[ByteStr]): Unit = {
    writeToDb(CacheKeys.VRF.mkKey(height), data)
    log.trace(s"setVrf($height)")
  }

  private val activatedFeaturesDbKey = CacheKeys.ActivatedFeatures.mkKey(())
  override def getActivatedFeatures(): BlockchainData[Map[Short, Int]] = {
    val r = db.readOnly(_.getOpt(activatedFeaturesDbKey)) match {
      case None     => BlockchainData.Unknown
      case Some(xs) => BlockchainData.Cached(xs)
    }

    log.trace(s"getActivatedFeatures: ${r.toFoundStr(_.mkString(", "))}")
    r
  }

  override def setActivatedFeatures(data: Map[Short, Int]): Unit = {
    db.readWrite(_.put(activatedFeaturesDbKey, data))
    log.trace("setActivatedFeatures")
  }

  override def getAssetDescription(asset: Asset.IssuedAsset): BlockchainData[AssetDescription] =
    readFromDb(CacheKeys.AssetDescriptions.mkKey(asset)).tap { r =>
      log.trace(s"getAssetDescription($asset): ${r.toFoundStr(_.toString)}")
    }

  override def setAssetDescription(asset: Asset.IssuedAsset, data: BlockchainData[AssetDescription]): Unit = {
    writeToDb(CacheKeys.AssetDescriptions.mkKey(asset), data)
    log.trace(s"setAssetDescription($asset)")
  }

  override def resolveAlias(alias: Alias): BlockchainData[Address] =
    readFromDb(CacheKeys.Aliases.mkKey(alias)).tap { r =>
      log.trace(s"resolveAlias($alias): ${r.toFoundStr()}")
    }

  override def setAlias(alias: Alias, data: BlockchainData[Address]): Unit = {
    writeToDb(CacheKeys.Aliases.mkKey(alias), data)
    log.trace(s"setAlias($alias)")
  }

  override def getBalances(address: Address): BlockchainData[Portfolio] =
    readFromDb(CacheKeys.Portfolios.mkKey(getOrMkAddressId(address))).tap { r =>
      log.trace(s"getBalances($address): ${r.toFoundStr("assets", _.assets)}")
    }

  override def setBalances(address: Address, data: BlockchainData[Portfolio]): Unit = {
    writeToDb(CacheKeys.Portfolios.mkKey(getOrMkAddressId(address)), data)
    log.trace(s"setBalances($address)")
  }

  override def getTransaction(id: ByteStr): BlockchainData[(TxMeta, Option[Transaction])] =
    readFromDb(CacheKeys.Transactions.mkKey(id)).tap { r =>
      log.trace(s"getTransaction($id): ${r.toFoundStr { case (meta, tx) => s"meta=${meta.height}, tpe=${tx.map(_.tpe)}" }}")
    }

  override def setTransaction(id: ByteStr, data: BlockchainData[(TxMeta, Option[Transaction])]): Unit = {
    writeToDb(CacheKeys.Transactions.mkKey(id), data)
    log.trace(s"setTransaction($id)")
  }

  private def readFromDb[T](dbKey: Key[Option[T]]): BlockchainData[T] =
    db.readOnly { ro =>
      val x = ro.getOpt(dbKey)
      x.fold[BlockchainData[T]](BlockchainData.Unknown)(BlockchainData.loaded)
    }

  private def writeToDb[T](dbKey: Key[Option[T]], data: BlockchainData[T]): Unit =
    db.readWrite { rw =>
      if (data.loaded) rw.put(dbKey, data.mayBeValue)
      else rw.delete(dbKey)
    }
}
