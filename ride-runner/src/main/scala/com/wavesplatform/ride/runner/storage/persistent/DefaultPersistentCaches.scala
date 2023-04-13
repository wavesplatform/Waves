package com.wavesplatform.ride.runner.storage.persistent

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.blockchain.SignedBlockHeaderWithVrf
import com.wavesplatform.collections.syntax.*
import com.wavesplatform.database.AddressId
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite, RideDbAccess}
import com.wavesplatform.ride.runner.stats.KamonCaffeineStats
import com.wavesplatform.ride.runner.storage.{AccountAssetKey, AccountDataKey, RemoteData}
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, DataEntry, EmptyDataEntry, Height, LeaseBalance, TransactionId}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.{LoggerFacade, ScorexLogging}
import org.slf4j.LoggerFactory

import java.lang.Long as JLong
import java.util.concurrent.atomic.AtomicLong
import scala.util.chaining.scalaUtilChainingOps

class DefaultPersistentCaches private (storage: RideDbAccess, initialBlockHeadersLastHeight: Option[Height])
    extends PersistentCaches
    with ScorexLogging {
  override val addressIds: AddressIdPersistentCache = new AddressIdPersistentCache {
    private val lastAddressIdKey = KvPairs.LastAddressId.at(())
    private val lastAddressId    = new AtomicLong(storage.readOnly(_.getOpt(lastAddressIdKey).getOrElse(-1L)))

    // TODO #12: Caching from NODE, settings
    private val addressIdCache: Cache[Address, java.lang.Long] =
      Caffeine
        .newBuilder()
        .maximumSize(1000)
        .softValues()
        .recordStats(() => new KamonCaffeineStats("Addresses"))
        .build()

    override def getAddress(addressId: AddressId)(implicit ctx: ReadOnly): Option[Address] =
      ctx.getOpt(KvPairs.IdToAddress.at(addressId))

    override def getAddressId(address: Address)(implicit ctx: ReadOnly): Option[AddressId] =
      Option(
        addressIdCache.get(
          address,
          { address =>
            ctx.getOpt(KvPairs.AddressToId.at(address)).fold[JLong](null)(x => JLong.valueOf(x))
          }
        )
      ).map(x => AddressId.apply(x.toLong))

    override def getOrMkAddressId(address: Address)(implicit ctx: ReadWrite): AddressId = AddressId(
      addressIdCache
        .get(
          address,
          { address =>
            val key = KvPairs.AddressToId.at(address)
            val r = ctx.getOpt(key) match {
              case Some(r) => r
              case None =>
                val newId = AddressId(lastAddressId.incrementAndGet())
                log.trace(s"getOrMkAddressId($address): new $newId")
                ctx.put(key, newId)
                ctx.put(KvPairs.IdToAddress.at(newId), address)
                ctx.put(lastAddressIdKey, newId)
                newId
            }

            JLong.valueOf(r.toLong)
          }
        )
        .toLong
    )
  }

  override val accountDataEntries: PersistentCache[AccountDataKey, DataEntry[?]] = new PersistentCache[AccountDataKey, DataEntry[?]] {
    protected lazy val log = LoggerFacade(LoggerFactory.getLogger("DefaultPersistentCaches.accountData"))

    override def getAllKeys()(implicit ctx: ReadOnly): List[(Address, String)] = for {
      (addressId, key) <- ctx.readKeys(KvPairs.AccountDataEntriesHistory)
      address          <- addressIds.getAddress(addressId)
    } yield (address, key)

    override def get(maxHeight: Height, key: (Address, String))(implicit ctx: ReadWrite): RemoteData[DataEntry[?]] =
      RemoteData
        .cachedOrUnknown(addressIds.getAddressId(key._1))
        .flatMap { addressId =>
          val k = (addressId, key._2)
          ctx.readHistoricalFromDbOpt(
            KvPairs.AccountDataEntriesHistory.at(k),
            h => KvPairs.AccountDataEntries.at((h, k)),
            maxHeight
          )
        }
        .tap { r => log.trace(s"get($key, $maxHeight): ${r.toFoundStr("value", _.value)}") }

    override def set(atHeight: Height, key: (Address, String), data: RemoteData[DataEntry[?]])(implicit ctx: ReadWrite): Unit = {
      val addressId = addressIds.getOrMkAddressId(key._1)
      val k         = (addressId, key._2)
      ctx.writeHistoricalToDbOpt(
        KvPairs.AccountDataEntriesHistory.at(k),
        h => KvPairs.AccountDataEntries.at((h, k)),
        atHeight,
        data.flatMap {
          // HACK, see DataTxSerializer.serializeEntry because
          case _: EmptyDataEntry => RemoteData.Absence
          case x                 => RemoteData.Cached(x)
        }
      )
      log.trace(s"set($key, $atHeight)")
    }

    override def removeFrom(fromHeight: Height, key: (Address, String))(implicit ctx: ReadWrite): RemoteData[DataEntry[?]] = {
      val addressId = addressIds.getOrMkAddressId(key._1)
      val k         = (addressId, key._2)
      ctx
        .removeFromAndGetLatestExistedOpt(
          KvPairs.AccountDataEntriesHistory.at(k),
          h => KvPairs.AccountDataEntries.at((h, k)),
          fromHeight
        )
        .tap { _ => log.trace(s"remove($key, $fromHeight)") }
    }

    override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): List[(Address, String)] =
      ctx
        .removeFrom(KvPairs.AccountDataEntriesHistory, KvPairs.AccountDataEntries, fromHeight)
        .flatMap { case (addrId, dataKey) => addressIds.getAddress(addrId).map((_, dataKey)) }
  }

  override val accountScripts: PersistentCache[Address, AccountScriptInfo] = new PersistentCache[Address, AccountScriptInfo] {
    protected lazy val log = LoggerFacade(LoggerFactory.getLogger("DefaultPersistentCaches.accountScripts"))

    override def getAllKeys()(implicit ctx: ReadOnly): List[Address] = for {
      addressId <- ctx.readKeys(KvPairs.AccountScriptsHistory)
      address   <- addressIds.getAddress(addressId)
    } yield address

    override def get(maxHeight: Height, key: Address)(implicit ctx: ReadWrite): RemoteData[AccountScriptInfo] =
      RemoteData
        .cachedOrUnknown(addressIds.getAddressId(key))
        .flatMap { addressId =>
          ctx.readHistoricalFromDbOpt(
            KvPairs.AccountScriptsHistory.at(addressId),
            h => KvPairs.AccountScripts.at((h, addressId)),
            maxHeight
          )
        }
        .tap { r => log.trace(s"get($key, $maxHeight): ${r.toFoundStr("hash", _.script.hashCode())}") }

    override def set(atHeight: Height, key: Address, data: RemoteData[AccountScriptInfo])(implicit ctx: ReadWrite): Unit = {
      val addressId = addressIds.getOrMkAddressId(key)
      ctx.writeHistoricalToDbOpt(
        KvPairs.AccountScriptsHistory.at(addressId),
        h => KvPairs.AccountScripts.at((h, addressId)),
        atHeight,
        data
      )
      log.trace(s"set($key, $atHeight)")
    }

    override def removeFrom(fromHeight: Height, key: Address)(implicit ctx: ReadWrite): RemoteData[AccountScriptInfo] = {
      val addressId = addressIds.getOrMkAddressId(key)
      ctx
        .removeFromAndGetLatestExistedOpt(
          KvPairs.AccountScriptsHistory.at(addressId),
          h => KvPairs.AccountScripts.at((h, addressId)),
          fromHeight
        )
        .tap { _ => log.trace(s"remove($key, $fromHeight)") }
    }

    override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): List[Address] =
      ctx
        .removeFrom(KvPairs.AccountScriptsHistory, KvPairs.AccountScripts, fromHeight)
        .flatMap(addressIds.getAddress)
  }

  override val assetDescriptions: PersistentCache[Asset.IssuedAsset, AssetDescription] = new PersistentCache[Asset.IssuedAsset, AssetDescription] {
    protected lazy val log = LoggerFacade(LoggerFactory.getLogger("DefaultPersistentCaches.assetDescriptions"))

    override def getAllKeys()(implicit ctx: ReadOnly): List[Asset.IssuedAsset] = ctx.readKeys(KvPairs.AssetDescriptionsHistory)

    override def get(maxHeight: Height, key: Asset.IssuedAsset)(implicit ctx: ReadWrite): RemoteData[AssetDescription] =
      ctx
        .readHistoricalFromDbOpt(
          KvPairs.AssetDescriptionsHistory.at(key),
          h => KvPairs.AssetDescriptions.at((h, key)),
          maxHeight
        )
        .tap { r => log.trace(s"get($key, $maxHeight): ${r.toFoundStr(_.toString)}") }

    override def set(atHeight: Height, key: Asset.IssuedAsset, data: RemoteData[AssetDescription])(implicit ctx: ReadWrite): Unit = {
      ctx.writeHistoricalToDbOpt(
        KvPairs.AssetDescriptionsHistory.at(key),
        h => KvPairs.AssetDescriptions.at((h, key)),
        atHeight,
        data
      )
      log.trace(s"set($key, $atHeight)")
    }

    override def removeFrom(fromHeight: Height, key: Asset.IssuedAsset)(implicit ctx: ReadWrite): RemoteData[AssetDescription] = {
      ctx
        .removeFromAndGetLatestExistedOpt(
          KvPairs.AssetDescriptionsHistory.at(key),
          h => KvPairs.AssetDescriptions.at((h, key)),
          fromHeight
        )
        .tap { _ => log.trace(s"remove($key, $fromHeight)") }
    }

    override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): List[Asset.IssuedAsset] =
      ctx.removeFrom(KvPairs.AssetDescriptionsHistory, KvPairs.AssetDescriptions, fromHeight)
  }

  override def aliases: PersistentCache[Alias, Address] = new PersistentCache[Alias, Address] {
    protected lazy val log = LoggerFacade(LoggerFactory.getLogger("DefaultPersistentCaches.aliases"))

    override def getAllKeys()(implicit ctx: ReadOnly): List[Alias] = List.empty // ctx.readKeys(CacheKeys.Aliases)

    override def get(maxHeight: Height, key: Alias)(implicit ctx: ReadWrite): RemoteData[Address] =
      ctx
        .getRemoteDataOpt(KvPairs.Aliases.at(key))
        .tap { r => log.trace(s"get($key): ${r.toFoundStr()}") }

    override def set(atHeight: Height, key: Alias, data: RemoteData[Address])(implicit ctx: ReadWrite): Unit = {
      ctx.writeToDb(KvPairs.Aliases.at(key), data)
      log.trace(s"set($key)")
    }

    // Not supported, so we reload the data during rollbacks
    override def removeFrom(fromHeight: Height, key: Alias)(implicit ctx: ReadWrite): RemoteData[Address] = {
      ctx.delete(KvPairs.Aliases.at(key))
      RemoteData.Unknown
    }

    override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): List[Alias] = List.empty // TODO #74
  }

  override val accountBalances: PersistentCache[AccountAssetKey, Long] = new PersistentCache[AccountAssetKey, Long] {
    protected lazy val log = LoggerFacade(LoggerFactory.getLogger("DefaultPersistentCaches.accountBalances"))

    override def getAllKeys()(implicit ctx: ReadOnly): List[AccountAssetKey] = for {
      (addressId, assetId) <- ctx.readKeys(KvPairs.AccountAssetsHistory)
      address              <- addressIds.getAddress(addressId)
    } yield (address, assetId)

    override def get(maxHeight: Height, key: AccountAssetKey)(implicit ctx: ReadWrite): RemoteData[Long] = {
      val (address, asset) = key
      RemoteData
        .cachedOrUnknown(addressIds.getAddressId(address))
        .flatMap { addressId =>
          val k = (addressId, asset)
          ctx.readHistoricalFromDb(
            KvPairs.AccountAssetsHistory.at(k),
            h => KvPairs.AccountAssets.at((h, k)),
            maxHeight
          )
        }
        .tap { r => log.trace(s"get($key): $r") }
    }

    override def set(atHeight: Height, key: AccountAssetKey, data: RemoteData[Long])(implicit ctx: ReadWrite): Unit = {
      val (address, asset) = key
      val addressId        = addressIds.getOrMkAddressId(address)
      val k                = (addressId, asset)
      ctx.writeHistoricalToDb(
        KvPairs.AccountAssetsHistory.at(k),
        h => KvPairs.AccountAssets.at((h, k)),
        atHeight,
        data,
        0L
      )
      log.trace(s"set($key)")
    }

    override def removeFrom(fromHeight: Height, key: AccountAssetKey)(implicit ctx: ReadWrite): RemoteData[Long] = {
      val (address, asset) = key
      val addressId        = addressIds.getOrMkAddressId(address)
      val k                = (addressId, asset)
      ctx
        .removeFromAndGetLatestExisted(
          KvPairs.AccountAssetsHistory.at(k),
          h => KvPairs.AccountAssets.at((h, k)),
          fromHeight
        )
        .tap { _ => log.trace(s"removeFrom($fromHeight, $key)") }
    }

    override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): List[(Address, Asset)] =
      ctx
        .removeFrom(KvPairs.AccountAssetsHistory, KvPairs.AccountAssets, fromHeight)
        .flatMap { case (addrId, dataKey) => addressIds.getAddress(addrId).map((_, dataKey)) }
  }

  override def accountLeaseBalances: PersistentCache[Address, LeaseBalance] = new PersistentCache[Address, LeaseBalance] {
    protected lazy val log = LoggerFacade(LoggerFactory.getLogger("DefaultPersistentCaches.accountLeaseBalances"))

    override def getAllKeys()(implicit ctx: ReadOnly): List[Address] = for {
      addressId <- ctx.readKeys(KvPairs.AccountLeaseBalancesHistory)
      address   <- addressIds.getAddress(addressId)
    } yield address

    override def get(maxHeight: Height, key: Address)(implicit ctx: ReadWrite): RemoteData[LeaseBalance] =
      RemoteData
        .cachedOrUnknown(addressIds.getAddressId(key))
        .flatMap { addressId =>
          ctx.readHistoricalFromDb(
            KvPairs.AccountLeaseBalancesHistory.at(addressId),
            h => KvPairs.AccountLeaseBalances.at((h, addressId)),
            maxHeight
          )
        }
        .tap { r => log.trace(s"get($key, $maxHeight): ${r.toFoundStr()}") }

    override def set(atHeight: Height, key: Address, data: RemoteData[LeaseBalance])(implicit ctx: ReadWrite): Unit = {
      val addressId = addressIds.getOrMkAddressId(key)
      ctx.writeHistoricalToDb(
        KvPairs.AccountLeaseBalancesHistory.at(addressId),
        h => KvPairs.AccountLeaseBalances.at((h, addressId)),
        atHeight,
        data,
        LeaseBalance.empty
      )
      log.trace(s"set($key, $atHeight)")
    }

    override def removeFrom(fromHeight: Height, key: Address)(implicit ctx: ReadWrite): RemoteData[LeaseBalance] = {
      val addressId = addressIds.getOrMkAddressId(key)
      ctx
        .removeFromAndGetLatestExisted(
          KvPairs.AccountLeaseBalancesHistory.at(addressId),
          h => KvPairs.AccountLeaseBalances.at((h, addressId)),
          fromHeight
        )
        .tap { _ => log.trace(s"remove($key, $fromHeight)") }
    }

    override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): List[Address] =
      ctx
        .removeFrom(KvPairs.AccountLeaseBalancesHistory, KvPairs.AccountLeaseBalances, fromHeight)
        .flatMap(addressIds.getAddress)
  }

  override val transactions: TransactionPersistentCache = new TransactionPersistentCache {
    protected lazy val log = LoggerFacade(LoggerFactory.getLogger("DefaultPersistentCaches.transactions"))

    override def getHeight(txId: TransactionId)(implicit ctx: ReadOnly): RemoteData[Height] =
      ctx
        .getRemoteDataOpt(KvPairs.Transactions.at(txId))
        .map(Height(_))
        .tap { r => log.trace(s"get($txId): ${r.toFoundStr { h => s"height=$h" }}") }

    override def setHeight(txId: TransactionId, height: RemoteData[Height])(implicit ctx: ReadWrite): Unit = {
      ctx.writeToDb(KvPairs.Transactions.at(txId), height)
      log.trace(s"set($txId)")
    }

    override def remove(txId: TransactionId)(implicit ctx: ReadWrite): Unit = ctx.delete(KvPairs.Transactions.at(txId))

    override def removeAllFrom(fromHeight: Int)(implicit ctx: ReadWrite): List[TransactionId] = {
      // TODO ?
      List.empty
    }
  }

  override val blockHeaders = new BlockPersistentCache {
    protected lazy val log = LoggerFacade(LoggerFactory.getLogger("DefaultPersistentCaches.blockHeaders"))

    private val Key = KvPairs.SignedBlockHeadersWithVrf

    @volatile private var lastHeight = initialBlockHeadersLastHeight

    override def getLastHeight(implicit ctx: ReadOnly): Option[Height] = lastHeight

    override def get(height: Height)(implicit ctx: ReadOnly): Option[SignedBlockHeaderWithVrf] =
      ctx
        .getOpt(Key.at(height))
        .tap { r => log.trace(s"get($height): ${r.toFoundStr("id", _.header.id())}") }

    override def getFrom(height: Height, n: Int)(implicit ctx: ReadOnly): List[SignedBlockHeaderWithVrf] = {
      val lastHeight = height + n - 1
      val startKey   = Key.at(height)
      val result     = List.newBuilder[SignedBlockHeaderWithVrf]
      ctx.iterateFrom(Key.prefixBytes, startKey.keyBytes) { entry =>
        val currentHeight = Key.parseKey(entry.getKey)
        val goNext        = currentHeight <= lastHeight
        if (goNext) result.addOne(Key.parseValue(entry.getValue))
        goNext
      }
      result.result()
    }

    override def set(height: Height, data: SignedBlockHeaderWithVrf)(implicit ctx: ReadWrite): Unit = {
      ctx.put(Key.at(height), data)
      log.trace(s"set($height)")
    }

    override def setLastHeight(height: Height)(implicit ctx: ReadWrite): Unit =
      if (lastHeight.forall(_ < height)) {
        lastHeight = Some(height)
        ctx.put(KvPairs.Height.at(()), height)
      }

    override def removeFrom(fromHeight: Height)(implicit ctx: ReadWrite): Unit = {
      val first = Key.at(fromHeight)
      ctx.iterateFrom(Key.prefixBytes, first.keyBytes) { x =>
        ctx.delete(x.getKey)
        true
      }

      val newLastHeight = Height(fromHeight - 1)
      lastHeight = if (ctx.has(Key.at(newLastHeight))) {
        ctx.put(KvPairs.Height.Key, newLastHeight)
        Some(newLastHeight)
      } else {
        ctx.delete(KvPairs.Height.Key)
        None
      }
      log.trace(s"removeFrom($fromHeight)")
    }
  }

  private val activatedFeaturesDbKey = KvPairs.ActivatedFeatures.at(())

  override def getActivatedFeatures(): RemoteData[Map[Short, Height]] = {
    val r = storage.readOnly(_.getOpt(activatedFeaturesDbKey)) match {
      case None     => RemoteData.Unknown
      case Some(xs) => RemoteData.Cached(xs)
    }

    log.trace(s"getActivatedFeatures: ${r.toFoundStr(_.mkString(", "))}")
    r
  }

  override def setActivatedFeatures(data: Map[Short, Height]): Unit = {
    storage.readWrite(_.put(activatedFeaturesDbKey, data))
    log.trace("setActivatedFeatures")
  }
}

object DefaultPersistentCaches {
  def apply(storage: RideDbAccess)(implicit ctx: ReadOnly): DefaultPersistentCaches =
    new DefaultPersistentCaches(storage, ctx.getOpt(KvPairs.Height.Key))
}
