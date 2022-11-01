package com.wavesplatform.ride.blockchain.caches

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{AddressId, DBExt, Key, RW, ReadOnlyDB}
import com.wavesplatform.ride.blockchain.BlockchainData
import com.wavesplatform.ride.blockchain.caches.LevelDbBlockchainCaches.{ReadOnlyDBOps, ReadWriteDBOps}
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
  private val lastAddressId    = new AtomicLong(db.readOnly(_.getOpt(lastAddressIdKey).getOrElse(-1L)))

  // TODO caching from NODE
  private def getOrMkAddressId(ro: ReadOnlyDB, address: Address): AddressId = {
    val key = CacheKeys.AddressIds.mkKey(address)
    ro.getOpt(key) match {
      case Some(r) => r
      case None =>
        val newId = AddressId(lastAddressId.incrementAndGet())
        db.readWrite { rw =>
          log.trace(s"getOrMkAddressId($address): new $newId")
          rw.put(key, newId)
          rw.put(lastAddressIdKey, newId)
        }
        newId
    }
  }

  private def getOrMkAddressId(rw: RW, address: Address): AddressId = {
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

  override def getAccountDataEntry(address: Address, key: String, maxHeight: Int): BlockchainData[DataEntry[_]] =
    db
      .readOnly { ro =>
        val addressId = getOrMkAddressId(ro, address)
        ro.readHistoricalFromDb(
          CacheKeys.AccountDataEntriesHistory.mkKey((addressId, key)),
          h => CacheKeys.AccountDataEntries.mkKey((addressId, key, h)),
          maxHeight
        )
      }
      .tap { r => log.trace(s"getAccountDataEntry($address, '$key', $maxHeight): ${r.toFoundStr("value", _.value)}") }

  override def setAccountDataEntry(address: Address, key: String, height: Int, data: BlockchainData[DataEntry[_]]): Unit = {
    db.readWrite { rw =>
      val addressId = getOrMkAddressId(rw, address)
      rw.writeHistoricalToDb(
        CacheKeys.AccountDataEntriesHistory.mkKey((addressId, key)),
        h => CacheKeys.AccountDataEntries.mkKey((addressId, key, h)),
        height,
        data
      )
    }
    log.trace(s"setAccountDataEntry($address, '$key', $height)")
  }

  override def getAccountScript(address: Address, maxHeight: Int): BlockchainData[AccountScriptInfo] =
    db
      .readOnly { ro =>
        val addressId = getOrMkAddressId(ro, address)
        ro.readHistoricalFromDb(
          CacheKeys.AccountScriptsHistory.mkKey(addressId),
          h => CacheKeys.AccountScripts.mkKey((addressId, h)),
          maxHeight
        )
      }
      .tap { r => log.trace(s"getAccountScript($address, $maxHeight): ${r.toFoundStr("hash", _.script.hashCode())}") }

  override def setAccountScript(address: Address, height: Int, data: BlockchainData[AccountScriptInfo]): Unit = {
    db.readWrite { rw =>
      val addressId = getOrMkAddressId(rw, address)
      rw.writeHistoricalToDb(
        CacheKeys.AccountScriptsHistory.mkKey(addressId),
        h => CacheKeys.AccountScripts.mkKey((addressId, h)),
        height,
        data
      )
    }
    log.trace(s"setAccountScript($address, $height)")
  }

  override def getBlockHeader(height: Int): BlockchainData[SignedBlockHeader] =
    db
      .readOnly { _.readFromDb(CacheKeys.SignedBlockHeaders.mkKey(height)) }
      .tap { r => log.trace(s"getBlockHeader($height): ${r.toFoundStr("id", _.id())}") }

  override def setBlockHeader(height: Int, data: BlockchainData[SignedBlockHeader]): Unit = {
    db.readWrite { _.writeToDb(CacheKeys.SignedBlockHeaders.mkKey(height), data) }
    log.trace(s"setBlockHeader($height)")
  }

  private val heightDbKey             = CacheKeys.Height.mkKey(())
  override def getHeight: Option[Int] = db.readOnly(_.getOpt(heightDbKey)).tap { r => log.trace(s"getHeight: $r") }
  override def setHeight(data: Int): Unit = {
    db.readWrite(_.put(heightDbKey, data))
    log.trace(s"setHeight($data)")
  }

  override def getVrf(height: Int): BlockchainData[ByteStr] =
    db.readOnly(_.readFromDb(CacheKeys.VRF.mkKey(height))).tap { r => log.trace(s"getVrf($height): $r") }

  override def setVrf(height: Int, data: BlockchainData[ByteStr]): Unit = {
    db.readWrite { _.writeToDb(CacheKeys.VRF.mkKey(height), data) }
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
    db.readWrite { _.put(activatedFeaturesDbKey, data) }
    log.trace("setActivatedFeatures")
  }

  override def getAssetDescription(asset: Asset.IssuedAsset, maxHeight: Int): BlockchainData[AssetDescription] =
    db
      .readOnly { ro =>
        ro.readHistoricalFromDb(
          CacheKeys.AssetDescriptionsHistory.mkKey(asset),
          h => CacheKeys.AssetDescriptions.mkKey((asset, h)),
          maxHeight
        )
      }
      .tap { r => log.trace(s"getAssetDescription($asset, $maxHeight): ${r.toFoundStr(_.toString)}") }

  override def setAssetDescription(asset: Asset.IssuedAsset, height: Int, data: BlockchainData[AssetDescription]): Unit = {
    db.readWrite { rw =>
      rw.writeHistoricalToDb(
        CacheKeys.AssetDescriptionsHistory.mkKey(asset),
        h => CacheKeys.AssetDescriptions.mkKey((asset, h)),
        height,
        data
      )
    }
    log.trace(s"setAssetDescription($asset, $height)")
  }

  override def resolveAlias(alias: Alias): BlockchainData[Address] =
    db
      .readOnly { _.readFromDb(CacheKeys.Aliases.mkKey(alias)) }
      .tap { r => log.trace(s"resolveAlias($alias): ${r.toFoundStr()}") }

  override def setAlias(alias: Alias, data: BlockchainData[Address]): Unit = {
    db.readWrite { _.writeToDb(CacheKeys.Aliases.mkKey(alias), data) }
    log.trace(s"setAlias($alias)")
  }

  override def getBalances(address: Address, maxHeight: Int): BlockchainData[Portfolio] =
    db
      .readOnly { ro =>
        val addressId = getOrMkAddressId(ro, address)
        ro.readHistoricalFromDb(
          CacheKeys.PortfoliosHistory.mkKey(addressId),
          h => CacheKeys.Portfolios.mkKey((addressId, h)),
          maxHeight
        )
      }
      .tap { r => log.trace(s"getBalances($address): ${r.toFoundStr("assets", _.assets)}") }

  override def setBalances(address: Address, height: Int, data: BlockchainData[Portfolio]): Unit = {
    db.readWrite { rw =>
      val addressId = getOrMkAddressId(rw, address)
      rw.writeHistoricalToDb(
        CacheKeys.PortfoliosHistory.mkKey(addressId),
        h => CacheKeys.Portfolios.mkKey((addressId, h)),
        height,
        data
      )
    }
    log.trace(s"setBalances($address)")
  }

  override def getTransaction(id: ByteStr): BlockchainData[(TxMeta, Option[Transaction])] =
    db
      .readOnly { _.readFromDb(CacheKeys.Transactions.mkKey(id)) }
      .tap { r => log.trace(s"getTransaction($id): ${r.toFoundStr { case (meta, tx) => s"meta=${meta.height}, tpe=${tx.map(_.tpe)}" }}") }

  override def setTransaction(id: ByteStr, data: BlockchainData[(TxMeta, Option[Transaction])]): Unit = {
    db.readWrite { _.writeToDb(CacheKeys.Transactions.mkKey(id), data) }
    log.trace(s"setTransaction($id)")
  }
}

object LevelDbBlockchainCaches {
  implicit final class ReadOnlyDBOps(val self: ReadOnlyDB) extends AnyVal {
    def readHistoricalFromDb[T](
        historyKey: Key[Seq[Int]],
        dataOnHeightKey: Int => Key[Option[T]],
        maxHeight: Int
    ): BlockchainData[T] = {
      val height = self.getOpt(historyKey).getOrElse(Seq.empty).find(_ <= maxHeight) // ordered from the newest to the oldest
      height
        .flatMap(height => self.getOpt(dataOnHeightKey(height)))
        .fold[BlockchainData[T]](BlockchainData.Unknown)(BlockchainData.loaded)
    }

    def readFromDb[T](dbKey: Key[Option[T]]): BlockchainData[T] = {
      val x = self.getOpt(dbKey)
      x.fold[BlockchainData[T]](BlockchainData.Unknown)(BlockchainData.loaded)
    }
  }

  implicit final class ReadWriteDBOps(val self: RW) extends AnyVal {
    def writeHistoricalToDb[T](
        historyKey: Key[Seq[Int]],
        dataOnHeightKey: Int => Key[Option[T]],
        height: Int,
        data: BlockchainData[T]
    ): Unit = {
      self.put(historyKey, self.getOpt(historyKey).getOrElse(Seq.empty).prepended(height))
      self.put(dataOnHeightKey(height), data.mayBeValue)
    }

    def writeToDb[T](dbKey: Key[Option[T]], data: BlockchainData[T]): Unit = {
      if (data.loaded) self.put(dbKey, data.mayBeValue)
      else self.delete(dbKey)
    }
  }
}
