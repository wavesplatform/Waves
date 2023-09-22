package com.wavesplatform.ride.runner.caches.disk

import cats.syntax.option.*
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.blockchain.SignedBlockHeaderWithVrf
import com.wavesplatform.collections.syntax.*
import com.wavesplatform.database.AddressId
import com.wavesplatform.database.rocksdb.Key
import com.wavesplatform.ride.runner.caches.*
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite, RideDbAccess}
import com.wavesplatform.ride.runner.stats.KamonCaffeineStats
import com.wavesplatform.state.{DataEntry, EmptyDataEntry, Height, LeaseBalance, TransactionId}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.utils.{LoggerFacade, ScorexLogging}
import org.slf4j.LoggerFactory

import java.lang.Long as JLong
import java.util.concurrent.atomic.AtomicLong
import scala.util.chaining.scalaUtilChainingOps

class DefaultDiskCaches private (storage: RideDbAccess, initialBlockHeadersLastHeight: Option[Height]) extends DiskCaches with ScorexLogging {
  override val addressIds: AddressIdDiskCache = new AddressIdDiskCache {
    private val lastAddressIdKey = KvPairs.LastAddressId.at(())
    private val lastAddressId    = new AtomicLong(storage.directReadOnly(_.getOpt(lastAddressIdKey).getOrElse(-1L)))

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

  override val accountDataEntries: DiskCache[(Address, String), DataEntry[?]] = new DiskCache[(Address, String), DataEntry[?]] {
    protected val log = mkLogger("AccountDataEntries")

    override def get(maxHeight: Height, key: (Address, String))(implicit ctx: ReadOnly): RemoteData[DataEntry[?]] = get(maxHeight, key._1, key._2)
    private def get(maxHeight: Height, address: Address, dataKey: String)(implicit ctx: ReadOnly): RemoteData[DataEntry[?]] =
      RemoteData
        .cachedOrUnknown(addressIds.getAddressId(address))
        .flatMap { addressId =>
          val k = (addressId, dataKey)
          ctx.readFromDb(k, KvPairs.AccountDataEntriesHistory, maxHeight)
        }
        .tap { r => log.trace(s"get($address, $dataKey, $maxHeight): ${r.toFoundStr("value", _.value)}") }

    override def set(atHeight: Height, key: (Address, String), data: RemoteData[DataEntry[?]])(implicit ctx: ReadWrite): Unit =
      set(atHeight, key._1, key._2, data)

    private def set(atHeight: Height, address: Address, dataKey: String, data: RemoteData[DataEntry[?]])(implicit ctx: ReadWrite): Unit = {
      val addressId = addressIds.getOrMkAddressId(address)
      val k         = (addressId, dataKey)
      ctx.writeToDb(
        k,
        KvPairs.AccountDataEntriesHistory,
        atHeight,
        data.flatMap {
          case _: EmptyDataEntry => RemoteData.Absence // HACK, see DataTxSerializer.serializeEntry
          case x                 => RemoteData.Cached(x)
        }.mayBeValue
      )
      log.trace(s"set($address, $dataKey, $atHeight) = ${data.toFoundStr()}")
    }

    override def removeFrom(fromHeight: Height, key: (Address, String))(implicit ctx: ReadWrite): RemoteData[DataEntry[?]] =
      removeFrom(fromHeight, key._1, key._2)

    private def removeFrom(fromHeight: Height, address: Address, dataKey: String)(implicit ctx: ReadWrite): RemoteData[DataEntry[?]] =
      addressIds.getAddressId(address).fold(RemoteData.unknown[DataEntry[?]]) { addressId =>
        val k = (addressId, dataKey)
        ctx
          .removeFromAndGetLatestExisted(k, KvPairs.AccountDataEntriesHistory, fromHeight)
          .tap { _ => log.trace(s"remove($address, $dataKey, $fromHeight)") }
      }

    override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): List[(Address, String)] =
      ctx
        .removeFrom(KvPairs.AccountDataEntriesHistory, fromHeight)
        .flatMap { case (addressId, dataKey) => addressIds.getAddress(addressId).map((_, dataKey)) }
  }

  override val accountScripts: DiskCache[Address, WeighedAccountScriptInfo] = new DiskCache[Address, WeighedAccountScriptInfo] {
    protected val log = mkLogger("AccountScripts")

    override def get(maxHeight: Height, key: Address)(implicit ctx: ReadOnly): RemoteData[WeighedAccountScriptInfo] =
      RemoteData
        .cachedOrUnknown(addressIds.getAddressId(key))
        .flatMap(ctx.readFromDb(_, KvPairs.AccountScriptsHistory, maxHeight))
        .tap { r => log.trace(s"get($key, $maxHeight): ${r.toFoundStr("hash", _.hashCode())}") }

    override def set(atHeight: Height, key: Address, data: RemoteData[WeighedAccountScriptInfo])(implicit
        ctx: ReadWrite
    ): Unit = {
      val addressId = addressIds.getOrMkAddressId(key)
      ctx.writeToDb(addressId, KvPairs.AccountScriptsHistory, atHeight, data.mayBeValue)
      log.trace(s"set($key, $atHeight) = ${data.map(_.hashCode()).toFoundStr()}")
    }

    override def removeFrom(fromHeight: Height, key: Address)(implicit ctx: ReadWrite): RemoteData[WeighedAccountScriptInfo] =
      addressIds.getAddressId(key).fold(RemoteData.unknown[WeighedAccountScriptInfo]) { addressId =>
        ctx
          .removeFromAndGetLatestExisted(addressId, KvPairs.AccountScriptsHistory, fromHeight)
          .tap { _ => log.trace(s"remove($key, $fromHeight)") }
      }

    override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): List[Address] =
      ctx
        .removeFrom(KvPairs.AccountScriptsHistory, fromHeight)
        .flatMap(addressIds.getAddress)
  }

  override val assetDescriptions: DiskCache[IssuedAsset, WeighedAssetDescription] = new DiskCache[IssuedAsset, WeighedAssetDescription] {
    protected val log = mkLogger("AssetDescriptions")

    override def get(maxHeight: Height, key: IssuedAsset)(implicit ctx: ReadOnly): RemoteData[WeighedAssetDescription] =
      ctx
        .readFromDb(key, KvPairs.AssetDescriptionsHistory, maxHeight)
        .tap { r => log.trace(s"get($key, $maxHeight): ${r.toFoundStr(_.assetDescription.toString)}") }

    override def set(atHeight: Height, key: IssuedAsset, data: RemoteData[WeighedAssetDescription])(implicit ctx: ReadWrite): Unit = {
      ctx.writeToDb(key, KvPairs.AssetDescriptionsHistory, atHeight, data.mayBeValue)
      log.trace(s"set($key, $atHeight) = ${data.toFoundStr()}")
    }

    override def removeFrom(fromHeight: Height, key: IssuedAsset)(implicit ctx: ReadWrite): RemoteData[WeighedAssetDescription] = {
      ctx
        .removeFromAndGetLatestExisted(key, KvPairs.AssetDescriptionsHistory, fromHeight)
        .tap { _ => log.trace(s"remove($key, $fromHeight)") }
    }

    override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): List[IssuedAsset] =
      ctx.removeFrom(KvPairs.AssetDescriptionsHistory, fromHeight)
  }

  override val aliases: AliasDiskCache = new AliasDiskCache {
    protected val log = mkLogger("Aliases")

    override def getAddress(key: Alias)(implicit ctx: ReadOnly): RemoteData[Address] =
      getRemoteDataOpt(key)._2.tap { r => log.trace(s"getAddress($key): ${r.toFoundStr()}") }

    override def setAddress(atHeight: Height, key: Alias, address: RemoteData[Address])(implicit ctx: ReadWrite): Unit = {
      val (prevHeight, _) = getRemoteDataOpt(key)
      prevHeight.foreach { prevHeight =>
        updateEntriesOnHeight(prevHeight)(_.filterNot(_ == key))
      }
      updateEntriesOnHeight(atHeight)(key :: _)

      writeToDb(atHeight, key, address.map(addressIds.getOrMkAddressId))
      log.trace(s"set($key, $address)")
    }

    override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): Vector[Alias] = {
      var removed = Vector.empty[Alias]
      ctx.iterateOverPrefix(KvPairs.AliasesByHeight, fromHeight) { p =>
        val onHeight = p.value
        removed = removed.prependedAll(onHeight)

        ctx.delete(p.dbEntry.getKey, KvPairs.AliasesByHeight.columnFamilyHandle)
        onHeight.foreach(k => ctx.delete(KvPairs.Aliases.at(k)))
      }
      removed
    }

    private def updateEntriesOnHeight(height: Height)(f: List[Alias] => List[Alias])(implicit ctx: ReadWrite): Unit =
      ctx.update(onHeightKey(height), List.empty)(f)

    private def getRemoteDataOpt(key: Alias)(implicit ctx: ReadOnly): (Option[Height], RemoteData[Address]) =
      ctx
        .getOpt(dataKey(key))
        .fold[(Option[Height], RemoteData[Address])]((none, RemoteData.Unknown)) { case (h, x) =>
          (h.some, RemoteData.loaded(x.flatMap(addressIds.getAddress)))
        }

    private def writeToDb(atHeight: Height, key: Alias, data: RemoteData[AddressId])(implicit ctx: ReadWrite): Unit =
      if (data.loaded) ctx.put(dataKey(key), (atHeight, data.mayBeValue))
      else ctx.delete(dataKey(key))

    private def onHeightKey(height: Height): Key[List[Alias]]         = KvPairs.AliasesByHeight.at(height)
    private def dataKey(key: Alias): Key[(Height, Option[AddressId])] = KvPairs.Aliases.at(key)
  }

  override val accountBalances: DiskCache[(Address, Asset), Long] = new DiskCache[(Address, Asset), Long] {
    protected val log = mkLogger("AccountBalances")

    override def get(maxHeight: Height, key: (Address, Asset))(implicit ctx: ReadOnly): RemoteData[Long] =
      get(maxHeight, key._1, key._2)

    private def get(maxHeight: Height, address: Address, asset: Asset)(implicit ctx: ReadOnly): RemoteData[Long] = {
      RemoteData
        .cachedOrUnknown(addressIds.getAddressId(address))
        .flatMap { addressId =>
          val k = (addressId, asset)
          ctx.readFromDb(k, KvPairs.AccountAssetsHistory, maxHeight)
        }
        .tap { r => log.trace(s"get($address, $asset): $r") }
    }

    override def set(atHeight: Height, key: (Address, Asset), data: RemoteData[Long])(implicit ctx: ReadWrite): Unit =
      set(atHeight, key._1, key._2, data)

    private def set(atHeight: Height, address: Address, asset: Asset, data: RemoteData[Long])(implicit ctx: ReadWrite): Unit = {
      val addressId = addressIds.getOrMkAddressId(address)
      val k         = (addressId, asset)
      ctx.writeToDb(k, KvPairs.AccountAssetsHistory, atHeight, data.getOrElse(0L))
      log.trace(s"set($address, $asset) = ${data.toFoundStr()}")
    }

    override def removeFrom(fromHeight: Height, key: (Address, Asset))(implicit ctx: ReadWrite): RemoteData[Long] =
      removeFrom(fromHeight, key._1, key._2)

    private def removeFrom(fromHeight: Height, address: Address, asset: Asset)(implicit ctx: ReadWrite): RemoteData[Long] =
      addressIds.getAddressId(address).fold(RemoteData.unknown[Long]) { addressId =>
        val k = (addressId, asset)
        ctx
          .removeFromAndGetLatestExisted(k, KvPairs.AccountAssetsHistory, fromHeight)
          .tap { _ => log.trace(s"removeFrom($fromHeight, $address, $asset)") }
      }

    override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): List[(Address, Asset)] =
      ctx
        .removeFrom(KvPairs.AccountAssetsHistory, fromHeight)
        .flatMap { case (addressId, asset) => addressIds.getAddress(addressId).map((_, asset)) }
  }

  override val accountLeaseBalances: DiskCache[Address, LeaseBalance] =
    new DiskCache[Address, LeaseBalance] {
      protected val log = mkLogger("AccountLeaseBalances")

      override def get(maxHeight: Height, key: Address)(implicit ctx: ReadOnly): RemoteData[LeaseBalance] =
        RemoteData
          .cachedOrUnknown(addressIds.getAddressId(key))
          .flatMap(ctx.readFromDb(_, KvPairs.AccountLeaseBalancesHistory, maxHeight))
          .tap { r => log.trace(s"get($key, $maxHeight): ${r.toFoundStr()}") }

      override def set(atHeight: Height, key: Address, data: RemoteData[LeaseBalance])(implicit ctx: ReadWrite): Unit = {
        val addressId = addressIds.getOrMkAddressId(key)
        ctx.writeToDb(addressId, KvPairs.AccountLeaseBalancesHistory, atHeight, data.getOrElse(LeaseBalance.empty))
        log.trace(s"set($key, $atHeight) = ${data.toFoundStr()}")
      }

      override def removeFrom(fromHeight: Height, key: Address)(implicit ctx: ReadWrite): RemoteData[LeaseBalance] =
        addressIds.getAddressId(key).fold(RemoteData.unknown[LeaseBalance]) { addressId =>
          ctx
            .removeFromAndGetLatestExisted(addressId, KvPairs.AccountLeaseBalancesHistory, fromHeight)
            .tap { _ => log.trace(s"remove($key, $fromHeight)") }
        }

      override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): List[Address] =
        ctx
          .removeFrom(KvPairs.AccountLeaseBalancesHistory, fromHeight)
          .flatMap(addressIds.getAddress(_))
    }

  override val transactions: TransactionDiskCache = new TransactionDiskCache {
    protected val log = mkLogger("Transactions")

    override def getHeight(key: TransactionId)(implicit ctx: ReadOnly): RemoteData[Height] =
      ctx
        .getRemoteDataOpt(KvPairs.Transactions.at(key))
        .tap { r => log.trace(s"get($key): ${r.toFoundStr { h => s"height=$h" }}") }

    override def setHeight(key: TransactionId, height: RemoteData[Height])(implicit ctx: ReadWrite): Unit =
      setHeight(key, ctx.getRemoteDataOpt(KvPairs.Transactions.at(key)), height)

    override def updateHeightIfExist(key: TransactionId, height: RemoteData[Height])(implicit ctx: ReadWrite): Unit = {
      val prevTxHeight = ctx.getRemoteDataOpt(KvPairs.Transactions.at(key))
      if (prevTxHeight.loaded) setHeight(key, prevTxHeight, height)
    }

    private def setHeight(key: TransactionId, prevTxHeight: RemoteData[Height], height: RemoteData[Height])(implicit
        ctx: ReadWrite
    ): Unit = {
      prevTxHeight.mayBeValue.foreach { prevHeight =>
        updateTxsOnHeight(prevHeight)(_.filterNot(_ == key))
      }
      height.mayBeValue.foreach(updateTxsOnHeight(_)(key :: _))

      ctx.writeToDb(KvPairs.Transactions.at(key), height)
      log.trace(s"set($key, $height)")
    }

    private def updateTxsOnHeight(height: Height)(f: List[TransactionId] => List[TransactionId])(implicit ctx: ReadWrite): Unit =
      ctx.update(KvPairs.TransactionsByHeight.at(height), List.empty)(f)

    override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): Seq[TransactionId] = {
      var removedTxs = Vector.empty[TransactionId]
      ctx.iterateOverPrefix(KvPairs.TransactionsByHeight, fromHeight) { p =>
        val txsOnHeight = p.value
        removedTxs = removedTxs.prependedAll(txsOnHeight)

        ctx.delete(p.dbEntry.getKey, KvPairs.TransactionsByHeight.columnFamilyHandle)
        txsOnHeight.foreach(key => ctx.delete(KvPairs.Transactions.at(key)))
      }
      removedTxs
    }
  }

  override val blockHeaders: BlockDiskCache = new BlockDiskCache {
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
      ctx.iterateOverPrefix(Key.at(fromHeight)) { x => ctx.delete(x.getKey, Key.columnFamilyHandle) }

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
    val r = storage.directReadOnly(_.getOpt(activatedFeaturesDbKey)) match {
      case None     => RemoteData.Unknown
      case Some(xs) => RemoteData.Cached(xs)
    }

    log.trace(s"getActivatedFeatures: ${r.toFoundStr(_.mkString(", "))}")
    r
  }

  override def setActivatedFeatures(data: Map[Short, Height]): Unit = {
    storage.directReadWrite(_.put(activatedFeaturesDbKey, data))
    log.trace("setActivatedFeatures")
  }

  private def mkLogger(name: String) = LoggerFacade(LoggerFactory.getLogger(s"${getClass.getName}.$name"))
}

object DefaultDiskCaches {
  def apply(storage: RideDbAccess)(implicit ctx: ReadOnly): DefaultDiskCaches =
    new DefaultDiskCaches(storage, getLastHeight())

  def getLastHeight()(implicit ctx: ReadOnly): Option[Height] = ctx.getOpt(KvPairs.Height.Key)
}
