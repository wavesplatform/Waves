package com.wavesplatform.riderunner.storage.persistent

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.collections.syntax.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{AddressId, Key, ReadOnlyDB}
import com.wavesplatform.riderunner.stats.KamonCaffeineStats
import com.wavesplatform.riderunner.storage.StorageContext.{ReadOnly, ReadWrite}
import com.wavesplatform.riderunner.storage.persistent.DefaultPersistentCaches.ReadOnlyDBOps
import com.wavesplatform.riderunner.storage.{AccountAssetKey, AccountDataKey, KeyIndexStorage, RemoteData, Storage}
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, DataEntry, EmptyDataEntry, Height, LeaseBalance, TransactionId}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.{LoggerFacade, ScorexLogging}
import org.slf4j.LoggerFactory

import java.lang.Long as JLong
import java.util.concurrent.atomic.AtomicLong
import scala.util.chaining.scalaUtilChainingOps

class DefaultPersistentCaches private (storage: Storage, initialBlockHeadersLastHeight: Option[Int]) extends PersistentCaches with ScorexLogging {
  private val lastAddressIdKey = CacheKeys.LastAddressId.mkKey(())
  private val lastAddressId    = new AtomicLong(storage.readOnly(_.db.getOpt(lastAddressIdKey).getOrElse(-1L)))

  override val accountDataEntries: PersistentCache[AccountDataKey, DataEntry[?]] = new PersistentCache[AccountDataKey, DataEntry[?]] {
    protected lazy val log = LoggerFacade(LoggerFactory.getLogger("LevelDbPersistentCaches.accountData"))

    override def getAllKeys()(implicit ctx: ReadOnly): List[(Address, String)] = for {
      (addressId, key) <- KeyIndexStorage.mkList(CacheKeys.AccountDataEntriesHistory)
      address          <- getAddress(addressId)
    } yield (address, key)

    override def get(maxHeight: Int, key: (Address, String))(implicit ctx: ReadWrite): RemoteData[DataEntry[?]] =
      RemoteData
        .cachedOrUnknown(getAddressId(key._1))
        .flatMap { addressId =>
          // TODO move to ctx
          ctx.db.readHistoricalFromDbOpt(
            CacheKeys.AccountDataEntriesHistory.mkKey((addressId, key._2)),
            h => CacheKeys.AccountDataEntries.mkKey((addressId, key._2, h)),
            maxHeight
          )
        }
        .tap { r => log.trace(s"get($key, $maxHeight): ${r.toFoundStr("value", _.value)}") }

    override def set(atHeight: Int, key: (Address, String), data: RemoteData[DataEntry[?]])(implicit ctx: ReadWrite): Unit = {
      val addressId = getOrMkAddressId(key._1)
      ctx.writeHistoricalToDbOpt(
        CacheKeys.AccountDataEntriesHistory.mkKey((addressId, key._2)),
        h => CacheKeys.AccountDataEntries.mkKey((addressId, key._2, h)),
        atHeight,
        data.flatMap {
          // HACK, see DataTxSerializer.serializeEntry because
          case _: EmptyDataEntry => RemoteData.Absence
          case x                 => RemoteData.Cached(x)
        }
      )
      log.trace(s"set($key, $atHeight)")
    }

    override def remove(fromHeight: Int, key: (Address, String))(implicit ctx: ReadWrite): RemoteData[DataEntry[?]] = {
      val addressId = getOrMkAddressId(key._1)
      ctx
        .removeAfterAndGetLatestExistedOpt(
          CacheKeys.AccountDataEntriesHistory.mkKey((addressId, key._2)),
          h => CacheKeys.AccountDataEntries.mkKey((addressId, key._2, h)),
          fromHeight
        )
        .tap { _ => log.trace(s"remove($key, $fromHeight)") }
    }
  }

  override val accountScripts: PersistentCache[Address, AccountScriptInfo] = new PersistentCache[Address, AccountScriptInfo] {
    protected lazy val log = LoggerFacade(LoggerFactory.getLogger("LevelDbPersistentCaches.accountScripts"))

    override def getAllKeys()(implicit ctx: ReadOnly): List[Address] = for {
      addressId <- KeyIndexStorage.mkList(CacheKeys.AccountScriptsHistory)
      address   <- getAddress(addressId)
    } yield address

    override def get(maxHeight: Int, key: Address)(implicit ctx: ReadWrite): RemoteData[AccountScriptInfo] =
      RemoteData
        .cachedOrUnknown(getAddressId(key))
        .flatMap { addressId =>
          ctx.db.readHistoricalFromDbOpt(
            CacheKeys.AccountScriptsHistory.mkKey(addressId),
            h => CacheKeys.AccountScripts.mkKey((addressId, h)),
            maxHeight
          )
        }
        .tap { r => log.trace(s"get($key, $maxHeight): ${r.toFoundStr("hash", _.script.hashCode())}") }

    override def set(atHeight: Int, key: Address, data: RemoteData[AccountScriptInfo])(implicit ctx: ReadWrite): Unit = {
      val addressId = getOrMkAddressId(key)
      ctx.writeHistoricalToDbOpt(
        CacheKeys.AccountScriptsHistory.mkKey(addressId),
        h => CacheKeys.AccountScripts.mkKey((addressId, h)),
        atHeight,
        data
      )
      log.trace(s"set($key, $atHeight)")
    }

    override def remove(fromHeight: Int, key: Address)(implicit ctx: ReadWrite): RemoteData[AccountScriptInfo] = {
      val addressId = getOrMkAddressId(key)
      ctx
        .removeAfterAndGetLatestExistedOpt(
          CacheKeys.AccountScriptsHistory.mkKey(addressId),
          h => CacheKeys.AccountScripts.mkKey((addressId, h)),
          fromHeight
        )
        .tap { _ => log.trace(s"remove($key, $fromHeight)") }
    }
  }

  override val assetDescriptions: PersistentCache[Asset.IssuedAsset, AssetDescription] = new PersistentCache[Asset.IssuedAsset, AssetDescription] {
    protected lazy val log = LoggerFacade(LoggerFactory.getLogger("LevelDbPersistentCaches.assetDescriptions"))

    override def getAllKeys()(implicit ctx: ReadOnly): List[Asset.IssuedAsset] = KeyIndexStorage.mkList(CacheKeys.AssetDescriptionsHistory)

    override def get(maxHeight: Int, key: Asset.IssuedAsset)(implicit ctx: ReadWrite): RemoteData[AssetDescription] =
      ctx.db
        .readHistoricalFromDbOpt(
          CacheKeys.AssetDescriptionsHistory.mkKey(key),
          h => CacheKeys.AssetDescriptions.mkKey((key, h)),
          maxHeight
        )
        .tap { r => log.trace(s"get($key, $maxHeight): ${r.toFoundStr(_.toString)}") }

    override def set(atHeight: Int, key: Asset.IssuedAsset, data: RemoteData[AssetDescription])(implicit ctx: ReadWrite): Unit = {
      ctx.writeHistoricalToDbOpt(
        CacheKeys.AssetDescriptionsHistory.mkKey(key),
        h => CacheKeys.AssetDescriptions.mkKey((key, h)),
        atHeight,
        data
      )
      log.trace(s"set($key, $atHeight)")
    }

    override def remove(fromHeight: Int, key: Asset.IssuedAsset)(implicit ctx: ReadWrite): RemoteData[AssetDescription] = {
      ctx
        .removeAfterAndGetLatestExistedOpt(
          CacheKeys.AssetDescriptionsHistory.mkKey(key),
          h => CacheKeys.AssetDescriptions.mkKey((key, h)),
          fromHeight
        )
        .tap { _ => log.trace(s"remove($key, $fromHeight)") }
    }
  }

  override def aliases: PersistentCache[Alias, Address] = new PersistentCache[Alias, Address] {
    protected lazy val log = LoggerFacade(LoggerFactory.getLogger("LevelDbPersistentCaches.aliases"))

    override def getAllKeys()(implicit ctx: ReadOnly): List[Alias] = List.empty // KeyIndexStorage.mkList(CacheKeys.Aliases)

    override def get(maxHeight: Int, key: Alias)(implicit ctx: ReadWrite): RemoteData[Address] =
      ctx.db
        .readFromDbOpt(CacheKeys.Aliases.mkKey(key))
        .tap { r => log.trace(s"get($key): ${r.toFoundStr()}") }

    override def set(atHeight: Int, key: Alias, data: RemoteData[Address])(implicit ctx: ReadWrite): Unit = {
      ctx.writeToDb(CacheKeys.Aliases.mkKey(key), data)
      log.trace(s"set($key)")
    }

    // Not supported, so we reload the data during rollbacks
    override def remove(fromHeight: Int, key: Alias)(implicit ctx: ReadWrite): RemoteData[Address] = {
      ctx.db.delete(CacheKeys.Aliases.mkKey(key))
      RemoteData.Unknown
    }
  }

  override val accountBalances: PersistentCache[AccountAssetKey, Long] = new PersistentCache[AccountAssetKey, Long] {
    protected lazy val log = LoggerFacade(LoggerFactory.getLogger("LevelDbPersistentCaches.accountBalances"))

    override def getAllKeys()(implicit ctx: ReadOnly): List[AccountAssetKey] = for {
      (addressId, assetId) <- KeyIndexStorage.mkList(CacheKeys.AccountAssetsHistory)
      address              <- getAddress(addressId)
    } yield (address, assetId)

    override def get(maxHeight: Int, key: AccountAssetKey)(implicit ctx: ReadWrite): RemoteData[Long] = {
      val (address, asset) = key
      RemoteData
        .cachedOrUnknown(getAddressId(address))
        .flatMap { addressId =>
          ctx.db.readHistoricalFromDb(
            CacheKeys.AccountAssetsHistory.mkKey((addressId, asset)),
            h => CacheKeys.AccountAssets.mkKey((addressId, asset, h)),
            maxHeight
          )
        }
        .tap { r => log.trace(s"get($key): $r") }
    }

    override def set(atHeight: Int, key: AccountAssetKey, data: RemoteData[Long])(implicit ctx: ReadWrite): Unit = {
      val (address, asset) = key
      val addressId        = getOrMkAddressId(address)
      ctx.writeHistoricalToDb(
        CacheKeys.AccountAssetsHistory.mkKey((addressId, asset)),
        h => CacheKeys.AccountAssets.mkKey((addressId, asset, h)),
        atHeight,
        data,
        0L
      )
      log.trace(s"set($key)")
    }

    override def remove(fromHeight: Int, key: AccountAssetKey)(implicit ctx: ReadWrite): RemoteData[Long] = {
      val (address, asset) = key
      val addressId        = getOrMkAddressId(address)
      ctx
        .removeAfterAndGetLatestExisted(
          CacheKeys.AccountAssetsHistory.mkKey((addressId, asset)),
          h => CacheKeys.AccountAssets.mkKey((addressId, asset, h)),
          fromHeight
        )
        .tap { _ => log.trace(s"remove($key, $fromHeight)") }
    }
  }

  override def accountLeaseBalances: PersistentCache[Address, LeaseBalance] = new PersistentCache[Address, LeaseBalance] {
    protected lazy val log = LoggerFacade(LoggerFactory.getLogger("LevelDbPersistentCaches.accountLeaseBalances"))

    override def getAllKeys()(implicit ctx: ReadOnly): List[Address] = for {
      addressId <- KeyIndexStorage.mkList(CacheKeys.AccountLeaseBalancesHistory)
      address   <- getAddress(addressId)
    } yield address

    override def get(maxHeight: Int, key: Address)(implicit ctx: ReadWrite): RemoteData[LeaseBalance] =
      RemoteData
        .cachedOrUnknown(getAddressId(key))
        .flatMap { addressId =>
          ctx.db.readHistoricalFromDb(
            CacheKeys.AccountLeaseBalancesHistory.mkKey(addressId),
            h => CacheKeys.AccountLeaseBalances.mkKey((addressId, h)),
            maxHeight
          )
        }
        .tap { r => log.trace(s"get($key, $maxHeight): ${r.toFoundStr()}") }

    override def set(atHeight: Int, key: Address, data: RemoteData[LeaseBalance])(implicit ctx: ReadWrite): Unit = {
      val addressId = getOrMkAddressId(key)
      ctx.writeHistoricalToDb(
        CacheKeys.AccountLeaseBalancesHistory.mkKey(addressId),
        h => CacheKeys.AccountLeaseBalances.mkKey((addressId, h)),
        atHeight,
        data,
        LeaseBalance.empty
      )
      log.trace(s"set($key, $atHeight)")
    }

    override def remove(fromHeight: Int, key: Address)(implicit ctx: ReadWrite): RemoteData[LeaseBalance] = {
      val addressId = getOrMkAddressId(key)
      ctx
        .removeAfterAndGetLatestExisted(
          CacheKeys.AccountLeaseBalancesHistory.mkKey(addressId),
          h => CacheKeys.AccountLeaseBalances.mkKey((addressId, h)),
          fromHeight
        )
        .tap { _ => log.trace(s"remove($key, $fromHeight)") }
    }
  }

  override val transactions: TransactionPersistentCache = new TransactionPersistentCache {
    protected lazy val log = LoggerFacade(LoggerFactory.getLogger("LevelDbPersistentCaches.transactions"))

    override def getHeight(txId: TransactionId)(implicit ctx: ReadOnly): RemoteData[Height] =
      ctx.db
        .readFromDbOpt(CacheKeys.Transactions.mkKey(txId))
        .map(Height(_))
        .tap { r => log.trace(s"get($txId): ${r.toFoundStr { h => s"height=$h" }}") }

    override def setHeight(txId: TransactionId, height: RemoteData[Height])(implicit ctx: ReadWrite): Unit = {
      ctx.writeToDb(CacheKeys.Transactions.mkKey(txId), height)
      log.trace(s"set($txId)")
    }

    override def remove(txId: TransactionId)(implicit ctx: ReadWrite): Unit = ctx.db.delete(CacheKeys.Transactions.mkKey(txId))
  }

  override val blockHeaders = new BlockPersistentCache {
    protected lazy val log = LoggerFacade(LoggerFactory.getLogger("LevelDbPersistentCaches.blockHeaders"))

    private val Key = CacheKeys.SignedBlockHeaders

    @volatile private var lastHeight = initialBlockHeadersLastHeight

    override def getLastHeight(implicit ctx: ReadOnly): Option[Int] = lastHeight

    override def get(height: Int)(implicit ctx: ReadOnly): Option[SignedBlockHeader] =
      ctx.db
        .getOpt(Key.mkKey(height))
        .tap { r => log.trace(s"get($height): ${r.toFoundStr("id", _.id())}") }

    override def getFrom(height: Int, n: Int)(implicit ctx: ReadOnly): List[SignedBlockHeader] = {
      val lastHeight = height + n - 1
      val startKey   = Key.mkKey(height)
      val result     = List.newBuilder[SignedBlockHeader]
      ctx.db.iterateFrom(Key.prefixBytes, startKey.keyBytes) { entry =>
        val currentHeight = Key.parseKey(entry.getKey)
        val goNext        = currentHeight <= lastHeight
        if (goNext) result.addOne(Key.parseValue(entry.getValue))
        goNext
      }
      result.result()
    }

    override def set(height: Int, data: SignedBlockHeader)(implicit ctx: ReadWrite): Unit = {
      ctx.db.put(Key.mkKey(height), data)
      if (lastHeight.forall(_ < height)) {
        lastHeight = Some(height)
        ctx.db.put(CacheKeys.Height.Key, height)
      }
      log.trace(s"set($height)")
    }

    override def removeFrom(fromHeight: Int)(implicit ctx: ReadWrite): Unit = {
      val first = Key.mkKey(fromHeight)
      ctx.db.iterateFrom(Key.prefixBytes, first.keyBytes) { x =>
        ctx.db.delete(x.getKey)
        true
      }

      val newLastHeight = fromHeight - 1
      lastHeight = if (ctx.db.prefixExists(Key.mkKey(newLastHeight).keyBytes)) {
        ctx.db.put(CacheKeys.Height.Key, newLastHeight)
        Some(newLastHeight)
      } else {
        ctx.db.delete(CacheKeys.Height.Key)
        None
      }
    }
  }

  override def vrf: VrfPersistentCache = new VrfPersistentCache {
    protected lazy val log = LoggerFacade(LoggerFactory.getLogger("LevelDbPersistentCaches.vrf"))

    val Key = CacheKeys.VRF

    override def get(height: Int)(implicit ctx: ReadOnly): RemoteData[ByteStr] =
      ctx.db
        .getOpt(Key.mkKey(height))
        .fold[RemoteData[ByteStr]](RemoteData.Unknown)(RemoteData.loaded)
        .tap { r => log.trace(s"get($height): $r") }

    override def set(height: Int, vrf: Option[ByteStr])(implicit ctx: ReadWrite): Unit = {
      ctx.db.put(Key.mkKey(height), vrf)
      log.trace(s"set($height)")
    }

    override def removeFrom(height: Int)(implicit ctx: ReadWrite): Unit = {
      val first = Key.mkKey(height)
      ctx.db.iterateFrom(Key.prefixBytes, first.keyBytes) { x =>
        ctx.db.delete(x.getKey)
        true
      }
    }
  }

  private val activatedFeaturesDbKey = CacheKeys.ActivatedFeatures.mkKey(())

  override def getActivatedFeatures(): RemoteData[Map[Short, Int]] = {
    val r = storage.readOnly(_.db.getOpt(activatedFeaturesDbKey)) match {
      case None     => RemoteData.Unknown
      case Some(xs) => RemoteData.Cached(xs)
    }

    log.trace(s"getActivatedFeatures: ${r.toFoundStr(_.mkString(", "))}")
    r
  }

  override def setActivatedFeatures(data: Map[Short, Int]): Unit = {
    storage.readWrite(_.db.put(activatedFeaturesDbKey, data))
    log.trace("setActivatedFeatures")
  }

  // TODO #12: Caching from NODE, settings
  private val addressIdCache: Cache[Address, java.lang.Long] =
    Caffeine
      .newBuilder()
      .maximumSize(1000)
      .softValues()
      .recordStats(() => new KamonCaffeineStats("Addresses"))
      .build()

  private def getAddress(addressId: AddressId)(implicit ctx: ReadOnly): Option[Address] =
    ctx.db.getOpt(CacheKeys.IdToAddress.mkKey(addressId))

  private def getAddressId(address: Address)(implicit ctx: ReadOnly): Option[AddressId] =
    Option(
      addressIdCache.get(
        address,
        { address =>
          ctx.db.getOpt(CacheKeys.AddressToId.mkKey(address)).fold[JLong](null)(x => JLong.valueOf(x))
        }
      )
    ).map(x => AddressId.apply(x.toLong))

  private def getOrMkAddressId(address: Address)(implicit ctx: ReadWrite): AddressId = AddressId(
    addressIdCache
      .get(
        address,
        { address =>
          val key = CacheKeys.AddressToId.mkKey(address)
          val r = ctx.db.getOpt(key) match {
            case Some(r) => r
            case None =>
              val newId = AddressId(lastAddressId.incrementAndGet())
              log.trace(s"getOrMkAddressId($address): new $newId")
              ctx.db.put(key, newId)
              ctx.db.put(CacheKeys.IdToAddress.mkKey(newId), address)
              ctx.db.put(lastAddressIdKey, newId)
              newId
          }

          JLong.valueOf(r.toLong)
        }
      )
      .toLong
  )
}

object DefaultPersistentCaches {
  def apply(storage: Storage)(implicit ctx: ReadOnly): DefaultPersistentCaches =
    new DefaultPersistentCaches(storage, ctx.db.getOpt(CacheKeys.Height.Key))

  implicit final class ReadOnlyDBOps(val self: ReadOnlyDB) extends AnyVal {
    // TODO move to ctx
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
}
