package com.wavesplatform.ride.runner.storage.persistent

import cats.syntax.option.*
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.blockchain.SignedBlockHeaderWithVrf
import com.wavesplatform.collections.syntax.*
import com.wavesplatform.database.AddressId
import com.wavesplatform.database.rocksdb.Key
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite, RideDbAccess}
import com.wavesplatform.ride.runner.stats.KamonCaffeineStats
import com.wavesplatform.ride.runner.storage.*
import com.wavesplatform.state.{DataEntry, EmptyDataEntry, Height, LeaseBalance, TransactionId}
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
      ).map(x => AddressId(x.toLong))

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
                log.trace(s"New address $address is assigned to $newId")
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

  override val accountDataEntries: PersistentCache[CacheKey.AccountData, DataEntry[?]] = new PersistentCache[CacheKey.AccountData, DataEntry[?]] {
    protected val log = mkLogger("AccountDataEntries")

    override def getAllKeys()(implicit ctx: ReadOnly): List[CacheKey.AccountData] = for {
      (addressId, key) <- ctx.collectKeys(KvPairs.AccountDataEntriesHistory)
      address          <- addressIds.getAddress(addressId)
    } yield CacheKey.AccountData(address, key)

    // TODO: What if I move this to ReadOnly / ReadWrite?
    override def get(maxHeight: Height, key: CacheKey.AccountData)(implicit ctx: ReadWrite): RemoteData[DataEntry[?]] =
      RemoteData
        .cachedOrUnknown(addressIds.getAddressId(key.address))
        .flatMap { addressId =>
          val k = (addressId, key.dataKey)
          ctx.readHistoricalFromDbOpt(
            KvPairs.AccountDataEntriesHistory.at(k),
            h => KvPairs.AccountDataEntries.at((h, k)),
            maxHeight
          )
        }
        .tap { r => log.trace(s"get($key, $maxHeight): ${r.toFoundStr("value", _.value)}") }

    override def set(atHeight: Height, key: CacheKey.AccountData, data: RemoteData[DataEntry[?]])(implicit ctx: ReadWrite): Unit = {
      val addressId = addressIds.getOrMkAddressId(key.address)
      val k         = (addressId, key.dataKey)
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
      log.trace(s"set($key, $atHeight) = ${data.toFoundStr()}")
    }

    override def removeFrom(fromHeight: Height, key: CacheKey.AccountData)(implicit ctx: ReadWrite): RemoteData[DataEntry[?]] = {
      val addressId = addressIds.getOrMkAddressId(key.address)
      val k         = (addressId, key.dataKey)
      ctx
        .removeFromAndGetLatestExistedOpt(
          KvPairs.AccountDataEntriesHistory.at(k),
          h => KvPairs.AccountDataEntries.at((h, k)),
          fromHeight
        )
        .tap { _ => log.trace(s"remove($key, $fromHeight)") }
    }

    override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): List[CacheKey.AccountData] =
      ctx
        .removeFrom(KvPairs.AccountDataEntriesHistory, KvPairs.AccountDataEntries, fromHeight)
        .flatMap { case (addrId, dataKey) => addressIds.getAddress(addrId).map(CacheKey.AccountData(_, dataKey)) }
  }

  override val accountScripts: PersistentCache[Address, WeighedAccountScriptInfo] = new PersistentCache[Address, WeighedAccountScriptInfo] {
    protected val log = mkLogger("AccountScripts")

    override def getAllKeys()(implicit ctx: ReadOnly): List[Address] = for {
      addressId <- ctx.collectKeys(KvPairs.AccountScriptsHistory)
      address   <- addressIds.getAddress(addressId)
    } yield address

    override def get(maxHeight: Height, key: Address)(implicit ctx: ReadWrite): RemoteData[WeighedAccountScriptInfo] =
      RemoteData
        .cachedOrUnknown(addressIds.getAddressId(key))
        .flatMap { addressId =>
          ctx.readHistoricalFromDbOpt(
            KvPairs.AccountScriptsHistory.at(addressId),
            h => KvPairs.AccountScripts.at((h, addressId)),
            maxHeight
          )
        }
        .tap { r => log.trace(s"get($key, $maxHeight): ${r.toFoundStr("hash", _.scriptInfo.script.hashCode())}") }

    override def set(atHeight: Height, key: Address, data: RemoteData[WeighedAccountScriptInfo])(implicit ctx: ReadWrite): Unit = {
      val addressId = addressIds.getOrMkAddressId(key)
      ctx.writeHistoricalToDbOpt(
        KvPairs.AccountScriptsHistory.at(addressId),
        h => KvPairs.AccountScripts.at((h, addressId)),
        atHeight,
        data
      )
      log.trace(s"set($key, $atHeight) = ${data.map(_.hashCode()).toFoundStr()}")
    }

    override def removeFrom(fromHeight: Height, key: Address)(implicit ctx: ReadWrite): RemoteData[WeighedAccountScriptInfo] = {
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

  override val assetDescriptions: PersistentCache[Asset.IssuedAsset, WeighedAssetDescription] =
    new PersistentCache[Asset.IssuedAsset, WeighedAssetDescription] {
      protected val log = mkLogger("AssetDescriptions")

      override def getAllKeys()(implicit ctx: ReadOnly): List[Asset.IssuedAsset] = ctx.collectKeys(KvPairs.AssetDescriptionsHistory)

      override def get(maxHeight: Height, key: Asset.IssuedAsset)(implicit ctx: ReadWrite): RemoteData[WeighedAssetDescription] =
        ctx
          .readHistoricalFromDbOpt(
            KvPairs.AssetDescriptionsHistory.at(key),
            h => KvPairs.AssetDescriptions.at((h, key)),
            maxHeight
          )
          .tap { r => log.trace(s"get($key, $maxHeight): ${r.toFoundStr(_.assetDescription.toString)}") }

      override def set(atHeight: Height, key: Asset.IssuedAsset, data: RemoteData[WeighedAssetDescription])(implicit ctx: ReadWrite): Unit = {
        ctx.writeHistoricalToDbOpt(
          KvPairs.AssetDescriptionsHistory.at(key),
          h => KvPairs.AssetDescriptions.at((h, key)),
          atHeight,
          data
        )
        log.trace(s"set($key, $atHeight) = ${data.toFoundStr()}")
      }

      override def removeFrom(fromHeight: Height, key: Asset.IssuedAsset)(implicit ctx: ReadWrite): RemoteData[WeighedAssetDescription] = {
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

  override val aliases: AliasPersistentCache = new AliasPersistentCache {
    protected val log = mkLogger("Aliases")

    override def getAllKeys(fromHeight: Height)(implicit ctx: ReadOnly): Seq[CacheKey.Alias] = ctx
      .collect(KvPairs.AliasesByHeight, fromHeight)(_.value)
      .map(CacheKey.Alias)

    override def getAddress(key: CacheKey.Alias)(implicit ctx: ReadOnly): RemoteData[Address] =
      getRemoteDataOpt(key)._2.tap { r => log.trace(s"get($key): ${r.toFoundStr()}") }

    override def setAddress(atHeight: Height, key: CacheKey.Alias, data: RemoteData[Address])(implicit ctx: ReadWrite): Unit = {
      val (prevHeight, _) = getRemoteDataOpt(key)
      prevHeight.foreach { prevHeight =>
        updateEntriesOnHeight(prevHeight)(_.filterNot(_ == key.alias))
      }
      updateEntriesOnHeight(atHeight)(key.alias :: _)

      writeToDb(atHeight, key, data)
      log.trace(s"set($key, $data)")
    }

    // TODO same as in transactions
    override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): Vector[CacheKey.Alias] = {
      var removed = Vector.empty[CacheKey.Alias]
      ctx.iterateOverPrefix(KvPairs.AliasesByHeight, fromHeight) { p =>
        val onHeight = p.value
        removed = removed.prependedAll(onHeight.map(CacheKey.Alias))

        ctx.delete(p.dbEntry.getKey)
        onHeight.foreach(k => ctx.delete(KvPairs.Aliases.at(k)))
      }
      removed
    }

    private def updateEntriesOnHeight(height: Height)(f: List[Alias] => List[Alias])(implicit ctx: ReadWrite): Unit =
      ctx.update(onHeightKey(height), List.empty)(f)

    private def getRemoteDataOpt(key: CacheKey.Alias)(implicit ctx: ReadOnly): (Option[Height], RemoteData[Address]) =
      ctx
        .getOpt(dataKey(key))
        .fold[(Option[Height], RemoteData[Address])]((none, RemoteData.Unknown)) { case (h, x) => (h.some, RemoteData.loaded(x)) }

    private def writeToDb(atHeight: Height, key: CacheKey.Alias, data: RemoteData[Address])(implicit ctx: ReadWrite): Unit =
      if (data.loaded) ctx.put(dataKey(key), (atHeight, data.mayBeValue))
      else ctx.delete(dataKey(key))

    private def onHeightKey(height: Height): Key[List[Alias]]                = KvPairs.AliasesByHeight.at(height)
    private def dataKey(key: CacheKey.Alias): Key[(Height, Option[Address])] = KvPairs.Aliases.at(key.alias)
  }

  override val accountBalances: PersistentCache[AccountAssetKey, Long] = new PersistentCache[AccountAssetKey, Long] {
    protected val log = mkLogger("AccountBalances")

    override def getAllKeys()(implicit ctx: ReadOnly): List[AccountAssetKey] = for {
      (addressId, assetId) <- ctx.collectKeys(KvPairs.AccountAssetsHistory)
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
      log.trace(s"set($key) = ${data.toFoundStr()}")
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

  override val accountLeaseBalances: PersistentCache[Address, LeaseBalance] = new PersistentCache[Address, LeaseBalance] {
    protected val log = mkLogger("AccountLeaseBalances")

    override def getAllKeys()(implicit ctx: ReadOnly): List[Address] = for {
      addressId <- ctx.collectKeys(KvPairs.AccountLeaseBalancesHistory)
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
      log.trace(s"set($key, $atHeight) = ${data.toFoundStr()}")
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
    protected val log = mkLogger("Transactions")

    override def getAllKeys(fromHeight: Height)(implicit ctx: ReadOnly): Seq[TransactionId] =
      ctx.collect(KvPairs.TransactionsByHeight, fromHeight)(_.value)

    override def getHeight(txId: TransactionId)(implicit ctx: ReadOnly): RemoteData[Height] =
      ctx
        .getRemoteDataOpt(KvPairs.Transactions.at(txId))
        .tap { r => log.trace(s"get($txId): ${r.toFoundStr { h => s"height=$h" }}") }

    /** TODO #121 Don't use this in one batch because of stale reads!
      */
    override def setHeight(txId: TransactionId, height: RemoteData[Height])(implicit ctx: ReadWrite): Unit = {
      val prevTxHeight = ctx.getRemoteDataOpt(KvPairs.Transactions.at(txId))
      prevTxHeight.mayBeValue.foreach { prevHeight =>
        updateTxsOnHeight(prevHeight)(_.filterNot(_ == txId))
      }
      height.mayBeValue.foreach(updateTxsOnHeight(_)(txId :: _))

      ctx.writeToDb(KvPairs.Transactions.at(txId), height)
      log.trace(s"set($txId, $height)")
    }

    private def updateTxsOnHeight(height: Height)(f: List[TransactionId] => List[TransactionId])(implicit ctx: ReadWrite): Unit =
      ctx.update(KvPairs.TransactionsByHeight.at(height), List.empty)(f)

    override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): Seq[TransactionId] = {
      var removedTxs = Vector.empty[TransactionId]
      ctx.iterateOverPrefix(KvPairs.TransactionsByHeight, fromHeight) { p =>
        val txsOnHeight = p.value
        removedTxs = removedTxs.prependedAll(txsOnHeight)

        ctx.delete(p.dbEntry.getKey)
        txsOnHeight.foreach(txId => ctx.delete(KvPairs.Transactions.at(txId)))
      }
      removedTxs
    }
  }

  override val blockHeaders: BlockPersistentCache = new BlockPersistentCache {
    protected val log = mkLogger("BlockHeaders")

    private val Key = KvPairs.SignedBlockHeadersWithVrf

    @volatile private var lastHeight = initialBlockHeadersLastHeight

    override def getLastHeight(implicit ctx: ReadOnly): Option[Height] = lastHeight

    override def get(height: Height)(implicit ctx: ReadOnly): Option[SignedBlockHeaderWithVrf] =
      ctx
        .getOpt(Key.at(height))
        .tap { r => log.trace(s"get($height): ${r.toFoundStr("id", _.header.id())}") }

    override def getFrom(height: Height, n: Int)(implicit ctx: ReadOnly): List[SignedBlockHeaderWithVrf] = {
      val lastHeight = height + n - 1
      val result     = List.newBuilder[SignedBlockHeaderWithVrf]
      ctx.iterateOverPrefixContinue(Key, height) { p =>
        val goNext = p.key <= lastHeight
        if (goNext) result.addOne(p.value)
        goNext
      }
      result.result()
    }

    override def set(height: Height, data: SignedBlockHeaderWithVrf)(implicit ctx: ReadWrite): Unit = {
      ctx.put(Key.at(height), data)
      log.trace(s"set($height)")
    }

    override def setLastHeight(height: Height)(implicit ctx: ReadWrite): Unit = {
      lastHeight = Some(height)
      ctx.put(KvPairs.Height.at(()), height)
      log.trace(s"setLastHeight($height)")
    }

    override def removeFrom(fromHeight: Height)(implicit ctx: ReadWrite): Unit = {
      ctx.iterateOverPrefix(Key.at(fromHeight)) { x => ctx.delete(x.getKey) }

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

  protected def mkLogger(name: String) = LoggerFacade(
    LoggerFactory.getLogger(s"com.wavesplatform.ride.runner.storage.persistent.DefaultPersistentCaches.$name")
  )
}

object DefaultPersistentCaches {
  def apply(storage: RideDbAccess)(implicit ctx: ReadOnly): DefaultPersistentCaches =
    new DefaultPersistentCaches(storage, getLastHeight())

  def getLastHeight()(implicit ctx: ReadOnly): Option[Height] = ctx.getOpt(KvPairs.Height.Key)
}
