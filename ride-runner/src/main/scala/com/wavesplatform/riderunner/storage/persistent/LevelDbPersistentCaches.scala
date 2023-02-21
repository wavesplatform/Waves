package com.wavesplatform.riderunner.storage.persistent

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.collections.syntax.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{AddressId, DBExt, Key, RW, ReadOnlyDB}
import com.wavesplatform.riderunner.storage.persistent.LevelDbPersistentCaches.{ReadOnlyDBOps, ReadWriteDBOps}
import com.wavesplatform.riderunner.storage.{AccountAssetKey, AccountDataKey}
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, DataEntry, EmptyDataEntry, Height, LeaseBalance, TransactionId}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.ScorexLogging
import org.rocksdb.RocksDB

import java.util.concurrent.atomic.AtomicLong
import scala.util.chaining.scalaUtilChainingOps

class LevelDbPersistentCaches(db: RocksDB) extends PersistentCaches with ScorexLogging {
  private val lastAddressIdKey = CacheKeys.LastAddressId.mkKey(())
  private val lastAddressId    = new AtomicLong(db.readOnly(_.getOpt(lastAddressIdKey).getOrElse(-1L)))

  override val accountDataEntries: PersistentCache[AccountDataKey, DataEntry[?]] = new PersistentCache[AccountDataKey, DataEntry[?]]
    with ScorexLogging {
    override def get(maxHeight: Int, key: (Address, String)): RemoteData[DataEntry[?]] =
      db
        .readOnly { ro =>
          val addressId = getOrMkAddressId(ro, key._1)
          ro.readHistoricalFromDbOpt(
            CacheKeys.AccountDataEntriesHistory.mkKey((addressId, key._2)),
            h => CacheKeys.AccountDataEntries.mkKey((addressId, key._2, h)),
            maxHeight
          )
        }
        .tap { r => log.trace(s"get($key, $maxHeight): ${r.toFoundStr("value", _.value)}") }

    override def set(atHeight: Int, key: (Address, String), data: RemoteData[DataEntry[?]]): Unit = {
      db.readWrite { rw =>
        val addressId = getOrMkAddressId(rw, key._1)
        rw.writeHistoricalToDbOpt(
          CacheKeys.AccountDataEntriesHistory.mkKey((addressId, key._2)),
          h => CacheKeys.AccountDataEntries.mkKey((addressId, key._2, h)),
          atHeight,
          data.flatMap {
            // HACK, see DataTxSerializer.serializeEntry because
            case _: EmptyDataEntry => RemoteData.Absence
            case x                 => RemoteData.Cached(x)
          }
        )
      }
      log.trace(s"set($key, $atHeight)")
    }

    override def remove(fromHeight: Int, key: (Address, String)): RemoteData[DataEntry[?]] =
      db
        .readWrite { rw =>
          val addressId = getOrMkAddressId(rw, key._1)
          rw.removeAfterAndGetLatestExistedOpt(
            CacheKeys.AccountDataEntriesHistory.mkKey((addressId, key._2)),
            h => CacheKeys.AccountDataEntries.mkKey((addressId, key._2, h)),
            fromHeight
          )
        }
        .tap { _ => log.trace(s"remove($key, $fromHeight)") }
  }

  override val accountScripts: PersistentCache[Address, AccountScriptInfo] = new PersistentCache[Address, AccountScriptInfo] with ScorexLogging {
    override def get(maxHeight: Int, key: Address): RemoteData[AccountScriptInfo] =
      db
        .readOnly { ro =>
          val addressId = getOrMkAddressId(ro, key)
          ro.readHistoricalFromDbOpt(
            CacheKeys.AccountScriptsHistory.mkKey(addressId),
            h => CacheKeys.AccountScripts.mkKey((addressId, h)),
            maxHeight
          )
        }
        .tap { r => log.trace(s"get($key, $maxHeight): ${r.toFoundStr("hash", _.script.hashCode())}") }

    override def set(atHeight: Int, key: Address, data: RemoteData[AccountScriptInfo]): Unit = {
      db.readWrite { rw =>
        val addressId = getOrMkAddressId(rw, key)
        rw.writeHistoricalToDbOpt(
          CacheKeys.AccountScriptsHistory.mkKey(addressId),
          h => CacheKeys.AccountScripts.mkKey((addressId, h)),
          atHeight,
          data
        )
      }
      log.trace(s"set($key, $atHeight)")
    }

    override def remove(fromHeight: Int, key: Address): RemoteData[AccountScriptInfo] =
      db
        .readWrite { rw =>
          val addressId = getOrMkAddressId(rw, key)
          rw.removeAfterAndGetLatestExistedOpt(
            CacheKeys.AccountScriptsHistory.mkKey(addressId),
            h => CacheKeys.AccountScripts.mkKey((addressId, h)),
            fromHeight
          )
        }
        .tap { _ => log.trace(s"remove($key, $fromHeight)") }
  }

  override val assetDescriptions: PersistentCache[Asset.IssuedAsset, AssetDescription] = new PersistentCache[Asset.IssuedAsset, AssetDescription]
    with ScorexLogging {
    override def get(maxHeight: Int, key: Asset.IssuedAsset): RemoteData[AssetDescription] =
      db
        .readOnly { ro =>
          ro.readHistoricalFromDbOpt(
            CacheKeys.AssetDescriptionsHistory.mkKey(key),
            h => CacheKeys.AssetDescriptions.mkKey((key, h)),
            maxHeight
          )
        }
        .tap { r => log.trace(s"get($key, $maxHeight): ${r.toFoundStr(_.toString)}") }

    override def set(atHeight: Int, key: Asset.IssuedAsset, data: RemoteData[AssetDescription]): Unit = {
      db.readWrite { rw =>
        rw.writeHistoricalToDbOpt(
          CacheKeys.AssetDescriptionsHistory.mkKey(key),
          h => CacheKeys.AssetDescriptions.mkKey((key, h)),
          atHeight,
          data
        )
      }
      log.trace(s"set($key, $atHeight)")
    }

    override def remove(fromHeight: Int, key: Asset.IssuedAsset): RemoteData[AssetDescription] =
      db
        .readWrite { rw =>
          rw.removeAfterAndGetLatestExistedOpt(
            CacheKeys.AssetDescriptionsHistory.mkKey(key),
            h => CacheKeys.AssetDescriptions.mkKey((key, h)),
            fromHeight
          )
        }
        .tap { _ => log.trace(s"remove($key, $fromHeight)") }
  }

  override def aliases: PersistentCache[Alias, Address] = new PersistentCache[Alias, Address] with ScorexLogging {
    override def get(maxHeight: Int, key: Alias): RemoteData[Address] =
      db
        .readOnly {
          _.readFromDbOpt(CacheKeys.Aliases.mkKey(key))
        }
        .tap { r => log.trace(s"get($key): ${r.toFoundStr()}") }

    override def set(atHeight: Int, key: Alias, data: RemoteData[Address]): Unit = {
      db.readWrite {
        _.writeToDb(CacheKeys.Aliases.mkKey(key), data)
      }
      log.trace(s"set($key)")
    }

    // Not supported, so we reload the data during rollbacks
    override def remove(fromHeight: Int, key: Alias): RemoteData[Address] = {
      db.readWrite { rw => rw.delete(CacheKeys.Aliases.mkKey(key)) }
      RemoteData.Unknown
    }
  }

  override val accountBalances: PersistentCache[AccountAssetKey, Long] = new PersistentCache[AccountAssetKey, Long] with ScorexLogging {
    override def get(maxHeight: Int, key: AccountAssetKey): RemoteData[Long] =
      db
        .readOnly { ro =>
          val (address, asset) = key
          val addressId        = getOrMkAddressId(ro, address)
          ro.readHistoricalFromDb(
            CacheKeys.AccountAssetsHistory.mkKey((addressId, asset)),
            h => CacheKeys.AccountAssets.mkKey((addressId, asset, h)),
            maxHeight
          )
        }
        .tap { r => log.trace(s"get($key): $r") }

    override def set(atHeight: Int, key: AccountAssetKey, data: RemoteData[Long]): Unit = {
      db.readWrite { rw =>
        val (address, asset) = key
        val addressId        = getOrMkAddressId(rw, address)
        rw.writeHistoricalToDb(
          CacheKeys.AccountAssetsHistory.mkKey((addressId, asset)),
          h => CacheKeys.AccountAssets.mkKey((addressId, asset, h)),
          atHeight,
          data,
          0L
        )
      }
      log.trace(s"set($key)")
    }

    override def remove(fromHeight: Int, key: AccountAssetKey): RemoteData[Long] =
      db
        .readWrite { rw =>
          val (address, asset) = key
          val addressId        = getOrMkAddressId(rw, address)
          rw.removeAfterAndGetLatestExisted(
            CacheKeys.AccountAssetsHistory.mkKey((addressId, asset)),
            h => CacheKeys.AccountAssets.mkKey((addressId, asset, h)),
            fromHeight
          )
        }
        .tap { _ => log.trace(s"remove($key, $fromHeight)") }
  }

  override def accountLeaseBalances: PersistentCache[Address, LeaseBalance] = new PersistentCache[Address, LeaseBalance] with ScorexLogging {
    override def get(maxHeight: Int, key: Address): RemoteData[LeaseBalance] =
      db
        .readOnly { ro =>
          val addressId = getOrMkAddressId(ro, key)
          ro.readHistoricalFromDb(
            CacheKeys.AccountLeaseBalancesHistory.mkKey(addressId),
            h => CacheKeys.AccountLeaseBalances.mkKey((addressId, h)),
            maxHeight
          )
        }
        .tap { r => log.trace(s"get($key, $maxHeight): ${r.toFoundStr()}") }

    override def set(atHeight: Int, key: Address, data: RemoteData[LeaseBalance]): Unit = {
      db.readWrite { rw =>
        val addressId = getOrMkAddressId(rw, key)
        rw.writeHistoricalToDb(
          CacheKeys.AccountLeaseBalancesHistory.mkKey(addressId),
          h => CacheKeys.AccountLeaseBalances.mkKey((addressId, h)),
          atHeight,
          data,
          LeaseBalance.empty
        )
      }
      log.trace(s"set($key, $atHeight)")
    }

    override def remove(fromHeight: Int, key: Address): RemoteData[LeaseBalance] =
      db
        .readWrite { rw =>
          val addressId = getOrMkAddressId(rw, key)
          rw.removeAfterAndGetLatestExisted(
            CacheKeys.AccountLeaseBalancesHistory.mkKey(addressId),
            h => CacheKeys.AccountLeaseBalances.mkKey((addressId, h)),
            fromHeight
          )
        }
        .tap { _ => log.trace(s"remove($key, $fromHeight)") }
  }

  override val transactions: TransactionPersistentCache = new TransactionPersistentCache with ScorexLogging {
    override def getHeight(txId: TransactionId): RemoteData[Height] =
      db
        .readOnly {
          _.readFromDbOpt(CacheKeys.Transactions.mkKey(txId)).map(Height(_))
        }
        .tap { r => log.trace(s"get($txId): ${r.toFoundStr { h => s"height=$h" }}") }

    override def setHeight(txId: TransactionId, height: RemoteData[Height]): Unit = {
      db.readWrite {
        _.writeToDb(CacheKeys.Transactions.mkKey(txId), height)
      }
      log.trace(s"set($txId)")
    }

    override def remove(txId: TransactionId): Unit = db.readWrite(_.delete(CacheKeys.Transactions.mkKey(txId)))
  }

  override val blockHeaders = new BlockPersistentCache {
    private val Key = CacheKeys.SignedBlockHeaders

    private val heightKey            = CacheKeys.Height.mkKey(())
    @volatile private var lastHeight = db.readOnly(_.getOpt(heightKey))

    override def getLastHeight: Option[Int] = lastHeight

    override def get(height: Int): Option[SignedBlockHeader] =
      db
        .readOnly {
          _.getOpt(Key.mkKey(height))
        }
        .tap { r => log.trace(s"get($height): ${r.toFoundStr("id", _.id())}") }

    override def getFrom(height: Int, n: Int): List[SignedBlockHeader] = {
      val lastHeight = height + n - 1
      val startKey   = Key.mkKey(height)
      val result     = List.newBuilder[SignedBlockHeader]
      db.readOnly { ro =>
        ro.iterateFrom(Key.prefixBytes, startKey.keyBytes) { entry =>
          val currentHeight = Key.parseKey(entry.getKey)
          val goNext        = currentHeight <= lastHeight
          if (goNext) result.addOne(Key.parseValue(entry.getValue))
          goNext
        }
      }
      result.result()
    }

    override def set(height: Int, data: SignedBlockHeader): Unit = {
      db.readWrite { rw =>
        rw.put(Key.mkKey(height), data)
        if (lastHeight.forall(_ < height)) {
          lastHeight = Some(height)
          rw.put(heightKey, height)
        }
      }
      log.trace(s"set($height)")
    }

    override def removeFrom(fromHeight: Int): Unit = {
      val first = Key.mkKey(fromHeight)
      db.readWrite { rw =>
        rw.iterateFrom(Key.prefixBytes, first.keyBytes) { x =>
          rw.delete(x.getKey)
          true
        }
      }

      // TODO #30 Get the last height in one batch
      db.readWrite { rw =>
        val newLastHeight = fromHeight - 1
        lastHeight = if (rw.prefixExists(Key.mkKey(newLastHeight).keyBytes)) {
          rw.put(heightKey, newLastHeight)
          Some(newLastHeight)
        } else {
          rw.delete(heightKey)
          None
        }
      }
    }
  }

  override def vrf: VrfPersistentCache = new VrfPersistentCache with ScorexLogging {
    val Key = CacheKeys.VRF

    override def get(height: Int): RemoteData[ByteStr] =
      db
        .readOnly { ro => ro.getOpt(Key.mkKey(height)) }
        .fold[RemoteData[ByteStr]](RemoteData.Unknown)(RemoteData.loaded)
        .tap { r => log.trace(s"get($height): $r") }

    override def set(height: Int, vrf: Option[ByteStr]): Unit = {
      db.readWrite(_.put(Key.mkKey(height), vrf))
      log.trace(s"set($height)")
    }

    override def removeFrom(height: Int): Unit = {
      val first = Key.mkKey(height)
      db.readWrite { rw =>
        rw.iterateFrom(Key.prefixBytes, first.keyBytes) { x =>
          rw.delete(x.getKey)
          true
        }
      }
    }
  }

  private val activatedFeaturesDbKey = CacheKeys.ActivatedFeatures.mkKey(())

  override def getActivatedFeatures(): RemoteData[Map[Short, Int]] = {
    val r = db.readOnly(_.getOpt(activatedFeaturesDbKey)) match {
      case None     => RemoteData.Unknown
      case Some(xs) => RemoteData.Cached(xs)
    }

    log.trace(s"getActivatedFeatures: ${r.toFoundStr(_.mkString(", "))}")
    r
  }

  override def setActivatedFeatures(data: Map[Short, Int]): Unit = {
    db.readWrite {
      _.put(activatedFeaturesDbKey, data)
    }
    log.trace("setActivatedFeatures")
  }

  // TODO #12: Caching from NODE
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
}

object LevelDbPersistentCaches {
  implicit final class ReadOnlyDBOps(val self: ReadOnlyDB) extends AnyVal {
    def readHistoricalFromDbOpt[T](
        historyKey: Key[Seq[Int]],
        dataOnHeightKey: Int => Key[Option[T]],
        maxHeight: Int
    ): RemoteData[T] = {
      val height = self.getOpt(historyKey).getOrElse(Seq.empty).find(_ <= maxHeight) // ordered from the newest to the oldest
      height
        .flatMap(height => self.getOpt(dataOnHeightKey(height)))
        .fold[RemoteData[T]](RemoteData.Unknown)(RemoteData.loaded)
    }

    def readHistoricalFromDb[T](
        historyKey: Key[Seq[Int]],
        dataOnHeightKey: Int => Key[T],
        maxHeight: Int
    ): RemoteData[T] = {
      val height = self.getOpt(historyKey).getOrElse(Seq.empty).find(_ <= maxHeight) // ordered from the newest to the oldest
      height
        .map(height => self.get(dataOnHeightKey(height)))
        .fold[RemoteData[T]](RemoteData.Unknown)(RemoteData.Cached(_))
    }

    def readFromDbOpt[T](dbKey: Key[Option[T]]): RemoteData[T] = {
      val x = self.getOpt(dbKey)
      x.fold[RemoteData[T]](RemoteData.Unknown)(RemoteData.loaded)
    }

    def readFromDb[T](dbKey: Key[T]): RemoteData[T] = {
      val x = self.getOpt(dbKey)
      x.fold[RemoteData[T]](RemoteData.Unknown)(RemoteData.Cached(_))
    }
  }

  implicit final class ReadWriteDBOps(val self: RW) extends AnyVal {
    def writeHistoricalToDbOpt[T](
        historyKey: Key[Seq[Int]],
        dataOnHeightKey: Int => Key[Option[T]],
        height: Int,
        data: RemoteData[T]
    ): Unit = {
      self.put(historyKey, self.getOpt(historyKey).getOrElse(Seq.empty).prepended(height))
      self.put(dataOnHeightKey(height), data.mayBeValue)
    }

    def writeHistoricalToDb[T](
        historyKey: Key[Seq[Int]],
        dataOnHeightKey: Int => Key[T],
        height: Int,
        data: RemoteData[T],
        default: => T
    ): Unit = {
      self.put(historyKey, self.getOpt(historyKey).getOrElse(Seq.empty).prepended(height))
      self.put(dataOnHeightKey(height), data.mayBeValue.getOrElse(default))
    }

    def removeAfterAndGetLatestExistedOpt[T](
        historyKey: Key[Seq[Int]],
        dataOnHeightKey: Int => Key[Option[T]],
        fromHeight: Int
    ): RemoteData[T] = {
      val history = self.getOpt(historyKey).getOrElse(Seq.empty)
      if (history.isEmpty) RemoteData.Unknown
      else {
        val (removedHistory, updatedHistory) = history.partition(_ >= fromHeight) // TODO #13: binary search
        self.put(historyKey, updatedHistory) // not deleting, because it will be added with a high probability
        removedHistory.foreach(h => self.delete(dataOnHeightKey(h)))

        updatedHistory.headOption match {
          case None    => RemoteData.Unknown
          case Some(h) => self.readFromDbOpt(dataOnHeightKey(h))
        }
      }
    }

    def removeAfterAndGetLatestExisted[T](
        historyKey: Key[Seq[Int]],
        dataOnHeightKey: Int => Key[T],
        fromHeight: Int
    ): RemoteData[T] = {
      val history = self.getOpt(historyKey).getOrElse(Seq.empty)
      if (history.isEmpty) RemoteData.Unknown
      else {
        val (removedHistory, updatedHistory) = history.partition(_ >= fromHeight) // TODO #13: binary search
        self.put(historyKey, updatedHistory) // not deleting, because it will be added with a high probability
        removedHistory.foreach(h => self.delete(dataOnHeightKey(h)))

        updatedHistory.headOption match {
          case None    => RemoteData.Unknown
          case Some(h) => self.readFromDb(dataOnHeightKey(h))
        }
      }
    }

    def writeToDb[T](dbKey: Key[Option[T]], data: RemoteData[T]): Unit = {
      if (data.loaded) self.put(dbKey, data.mayBeValue)
      else self.delete(dbKey)
    }
  }
}
