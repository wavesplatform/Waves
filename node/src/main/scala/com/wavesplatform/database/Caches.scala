package com.wavesplatform.database

import java.util

import cats.data.Ior
import cats.implicits._
import com.google.common.cache._
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.{Block, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.metrics.LevelDBStats
import com.wavesplatform.settings.DBSettings
import com.wavesplatform.state.DiffToStateApplier.PortfolioUpdates
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.{Asset, ScriptExecutionFailed, Transaction}
import com.wavesplatform.utils.ObservedLoadingCache
import monix.reactive.Observer

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag

abstract class Caches(spendableBalanceChanged: Observer[(Address, Asset)]) extends Blockchain with Storage {
  import Caches._

  val dbSettings: DBSettings

  @volatile
  private var current = (loadHeight(), loadScore(), loadLastBlock())

  protected def loadHeight(): Int
  override def height: Int = current._1

  protected def safeRollbackHeight: Int

  protected def loadScore(): BigInt
  override def score: BigInt = current._2

  protected def loadLastBlock(): Option[Block]
  override def lastBlock: Option[Block] = current._3

  def loadScoreOf(blockId: ByteStr): Option[BigInt]

  def loadBlockInfo(height: Int): Option[SignedBlockHeader]
  override def blockHeader(height: Int): Option[SignedBlockHeader] = current match {
    case (`height`, _, maybeBlock) => maybeBlock.map(b => SignedBlockHeader(b.header, b.signature))
    case _                         => loadBlockInfo(height)
  }

  def loadHeightOf(blockId: ByteStr): Option[Int]
  override def heightOf(blockId: ByteStr): Option[Int] = current match {
    case (height, _, Some(block)) if block.id() == blockId => Some(height)
    case _                                                 => loadHeightOf(blockId)
  }

  private val blocksTs                               = new util.TreeMap[Int, Long] // Height -> block timestamp, assume sorted by key.
  private var oldestStoredBlockTimestamp             = Long.MaxValue
  private val transactionIds                         = new util.HashMap[ByteStr, Int]() // TransactionId -> height
  protected def forgetTransaction(id: ByteStr): Unit = transactionIds.remove(id)
  override def containsTransaction(tx: Transaction): Boolean = transactionIds.containsKey(tx.id()) || {
    if (tx.timestamp - 2.hours.toMillis <= oldestStoredBlockTimestamp) {
      LevelDBStats.miss.record(1)
      transactionMeta(tx.id()).nonEmpty
    } else {
      false
    }
  }
  protected def forgetBlocks(): Unit = {
    val iterator = blocksTs.entrySet().iterator()
    val (oldestBlock, oldestTs) = if (iterator.hasNext) {
      val e = iterator.next()
      e.getKey -> e.getValue
    } else {
      0 -> Long.MaxValue
    }
    oldestStoredBlockTimestamp = oldestTs
    val bts = this.lastBlock.fold(0L)(_.header.timestamp) - dbSettings.rememberBlocks.toMillis
    blocksTs.entrySet().removeIf(_.getValue < bts)
    transactionIds.entrySet().removeIf(_.getValue < oldestBlock)
  }

  private val leaseBalanceCache: LoadingCache[Address, LeaseBalance] = cache(dbSettings.maxCacheSize, loadLeaseBalance)
  protected def loadLeaseBalance(address: Address): LeaseBalance
  protected def discardLeaseBalance(address: Address): Unit = leaseBalanceCache.invalidate(address)
  override def leaseBalance(address: Address): LeaseBalance = leaseBalanceCache.get(address)

  private val balancesCache: LoadingCache[(Address, Asset), java.lang.Long] =
    observedCache(dbSettings.maxCacheSize * 16, spendableBalanceChanged, loadBalance)
  protected def clearBalancesCache(): Unit                          = balancesCache.invalidateAll()
  protected def discardBalance(key: (Address, Asset)): Unit         = balancesCache.invalidate(key)
  override def balance(address: Address, mayBeAssetId: Asset): Long = balancesCache.get(address -> mayBeAssetId)
  protected def loadBalance(req: (Address, Asset)): Long

  private val assetDescriptionCache: LoadingCache[IssuedAsset, Option[AssetDescription]] = cache(dbSettings.maxCacheSize, loadAssetDescription)
  protected def loadAssetDescription(asset: IssuedAsset): Option[AssetDescription]
  protected def discardAssetDescription(asset: IssuedAsset): Unit             = assetDescriptionCache.invalidate(asset)
  override def assetDescription(asset: IssuedAsset): Option[AssetDescription] = assetDescriptionCache.get(asset)

  private val volumeAndFeeCache: LoadingCache[ByteStr, VolumeAndFee] = cache(dbSettings.maxCacheSize, loadVolumeAndFee)
  protected def loadVolumeAndFee(orderId: ByteStr): VolumeAndFee
  protected def discardVolumeAndFee(orderId: ByteStr): Unit       = volumeAndFeeCache.invalidate(orderId)
  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee = volumeAndFeeCache.get(orderId)

  private val scriptCache: LoadingCache[Address, Option[AccountScriptInfo]] = cache(dbSettings.maxCacheSize, loadScript)
  protected def loadScript(address: Address): Option[AccountScriptInfo]
  protected def hasScriptBytes(address: Address): Boolean
  protected def discardScript(address: Address): Unit = scriptCache.invalidate(address)

  override def accountScript(address: Address): Option[AccountScriptInfo] = scriptCache.get(address)
  override def hasAccountScript(address: Address): Boolean =
    Option(scriptCache.getIfPresent(address)).fold(hasScriptBytes(address))(_.nonEmpty)

  private val assetScriptCache: LoadingCache[IssuedAsset, Option[AssetScriptInfo]] =
    cache(dbSettings.maxCacheSize, loadAssetScript)
  protected def loadAssetScript(asset: IssuedAsset): Option[AssetScriptInfo]
  protected def hasAssetScriptBytes(asset: IssuedAsset): Boolean
  protected def discardAssetScript(asset: IssuedAsset): Unit = assetScriptCache.invalidate(asset)

  override def assetScript(asset: IssuedAsset): Option[AssetScriptInfo] = assetScriptCache.get(asset)

  private var lastAddressId = loadMaxAddressId()
  protected def loadMaxAddressId(): Long

  private val addressIdCache: LoadingCache[Address, Option[AddressId]] = cache(dbSettings.maxCacheSize, loadAddressId)
  protected def loadAddressId(address: Address): Option[AddressId]

  protected def addressIdWithFallback(address: Address, newAddresses: Map[Address, AddressId]): AddressId =
    newAddresses.getOrElse(address, addressIdCache.get(address).get)

  private val accountDataCache: LoadingCache[(Address, String), Option[DataEntry[_]]] = cache(dbSettings.maxCacheSize, {
    case (k, v) => loadAccountData(k, v)
  })

  override def accountData(acc: Address, key: String): Option[DataEntry[_]] = accountDataCache.get((acc, key))
  protected def discardAccountData(addressWithKey: (Address, String)): Unit = accountDataCache.invalidate(addressWithKey)
  protected def loadAccountData(acc: Address, key: String): Option[DataEntry[_]]

  private[database] def addressId(address: Address): Option[AddressId] = addressIdCache.get(address)

  @volatile
  protected var approvedFeaturesCache: Map[Short, Int] = loadApprovedFeatures()
  protected def loadApprovedFeatures(): Map[Short, Int]
  override def approvedFeatures: Map[Short, Int] = approvedFeaturesCache

  @volatile
  protected var activatedFeaturesCache: Map[Short, Int] = loadActivatedFeatures()
  protected def loadActivatedFeatures(): Map[Short, Int]
  override def activatedFeatures: Map[Short, Int] = activatedFeaturesCache

  @volatile
  protected var continuationStatesCache: LoadingCache[ByteStr, ContinuationState] =
    cache(dbSettings.maxCacheSize, invokeTxId => loadContinuationStates(TransactionId(invokeTxId)))
  protected def loadContinuationStates(invokeTxId: TransactionId): ContinuationState
  override def continuationStates: Map[ByteStr, ContinuationState] =
    continuationStatesCache.asMap.asScala.toMap

  //noinspection ScalaStyle
  protected def doAppend(
      block: Block,
      carry: Long,
      newAddresses: Map[Address, AddressId],
      balances: Map[AddressId, Map[Asset, Long]],
      leaseBalances: Map[AddressId, LeaseBalance],
      addressTransactions: Map[AddressId, Seq[TransactionId]],
      leaseStates: Map[ByteStr, Boolean],
      issuedAssets: Map[IssuedAsset, NewAssetInfo],
      reissuedAssets: Map[IssuedAsset, Ior[AssetInfo, AssetVolumeInfo]],
      filledQuantity: Map[ByteStr, VolumeAndFee],
      scripts: Map[AddressId, Option[AccountScriptInfo]],
      assetScripts: Map[IssuedAsset, Option[AssetScriptInfo]],
      data: Map[Address, AccountDataInfo],
      aliases: Map[Alias, AddressId],
      sponsorship: Map[IssuedAsset, Sponsorship],
      totalFee: Long,
      reward: Option[Long],
      hitSource: ByteStr,
      scriptResults: Map[ByteStr, InvokeScriptResult],
      failedTransactionIds: Set[ByteStr],
      stateHash: StateHashBuilder.Result,
      continuationStates: Map[ByteStr, ContinuationState],
      addressTransactionBindings: Map[AddressId, Seq[TransactionId]]
  ): Unit

  override def append(diff: Diff, carryFee: Long, totalFee: Long, reward: Option[Long], hitSource: ByteStr, block: Block): Unit = {
    val newHeight = current._1 + 1

    val stateHash = new StateHashBuilder

    val newAddresses = Set.newBuilder[Address]
    newAddresses ++= diff.portfolios.keys.filter(addressIdCache.get(_).isEmpty)
    for (NewTransactionInfo(_, addresses, _) <- diff.transactions.values; address <- addresses if addressIdCache.get(address).isEmpty) {
      newAddresses += address
    }

    val failedTransactionIds: Set[ByteStr] =
      diff.transactions.collect { case (id, NewTransactionInfo(_, _, ScriptExecutionFailed)) => id }.toSet

    val newAddressIds = (for {
      (address, offset) <- newAddresses.result().zipWithIndex
    } yield address -> AddressId(lastAddressId + offset + 1)).toMap

    lastAddressId += newAddressIds.size

    val PortfolioUpdates(updatedBalances, updatedLeaseBalances) = DiffToStateApplier.portfolios(this, diff)

    val leaseBalances = updatedLeaseBalances.map { case (address, lb) => addressIdWithFallback(address, newAddressIds) -> lb }

    val newFills = for {
      (orderId, fillInfo) <- diff.orderFills
    } yield orderId -> volumeAndFeeCache.get(orderId).combine(fillInfo)

    val transactionList = diff.transactions.toList

    transactionList.foreach {
      case (_, NewTransactionInfo(tx, _, _)) =>
        transactionIds.put(tx.id(), newHeight)
    }

    val addressTransactions: Map[AddressId, Seq[TransactionId]] =
      transactionList
        .flatMap {
          case (_, NewTransactionInfo(tx, addrs, _)) =>
            transactionIds.put(tx.id(), newHeight) // be careful here!

            addrs.map { addr =>
              addressIdWithFallback(addr, newAddressIds) -> TransactionId(tx.id())
            }
        }
        .groupBy(_._1)
        .view
        .mapValues(_.map {
          case (_, txId) => txId
        })
        .toMap

    val addressTransactionBindings: Map[AddressId, Seq[TransactionId]] =
      diff.addressTransactionBindings
        .toList
        .flatMap { case (txId, addresses) =>
          addresses.map(address => addressId(address).get -> TransactionId(txId))
        }
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2))
        .toMap

    current = (newHeight, current._2 + block.blockScore(), Some(block))

    for {
      (address, assets) <- updatedBalances
      (asset, balance)  <- assets
    } asset match {
      case Waves              => stateHash.addWavesBalance(address, balance)
      case asset: IssuedAsset => stateHash.addAssetBalance(address, asset, balance)
    }

    updatedLeaseBalances foreach {
      case (address, balance) =>
        stateHash.addLeaseBalance(address, balance.in, balance.out)
    }

    for {
      (address, data) <- diff.accountData
      entry           <- data.data.values
    } stateHash.addDataEntry(address, entry)

    diff.aliases.foreach {
      case (alias, address) =>
        stateHash.addAlias(address, alias.name)
    }

    for {
      (address, sv) <- diff.scripts
      script = sv.map(_.script)
    } stateHash.addAccountScript(address, script)

    for {
      (address, sv) <- diff.assetScripts
      script = sv.map(_.script)
    } stateHash.addAssetScript(address, script)

    diff.leaseState.foreach {
      case (leaseId, status) =>
        stateHash.addLeaseStatus(TransactionId @@ leaseId, status)
    }

    diff.sponsorship.foreach {
      case (asset, sponsorship) =>
        stateHash.addSponsorship(asset, sponsorship match {
          case SponsorshipValue(minFee) => minFee
          case SponsorshipNoInfo        => 0L
        })
    }

    doAppend(
      block,
      carryFee,
      newAddressIds,
      updatedBalances.map { case (a, v) => addressIdWithFallback(a, newAddressIds) -> v },
      leaseBalances,
      addressTransactions,
      diff.leaseState,
      diff.issuedAssets,
      diff.updatedAssets,
      newFills,
      diff.scripts.map { case (address, s) => addressIdWithFallback(address, newAddressIds) -> s },
      diff.assetScripts,
      diff.accountData,
      diff.aliases.map { case (a, address) => a -> addressIdWithFallback(address, newAddressIds) },
      diff.sponsorship,
      totalFee,
      reward,
      hitSource,
      diff.scriptResults,
      failedTransactionIds,
      stateHash.result(),
      diff.continuationStates,
      addressTransactionBindings
    )

    val emptyData = Map.empty[(Address, String), Option[DataEntry[_]]]

    val newData =
      diff.accountData.foldLeft(emptyData) {
        case (data, (a, d)) =>
          val updData = data ++ d.data.map {
            case (k, v) =>
              (a, k) -> v.some
          }

          updData
      }

    val assetsToInvalidate =
      diff.issuedAssets.keySet ++
        diff.updatedAssets.keySet ++
        diff.sponsorship.keySet

    for ((address, id)           <- newAddressIds) addressIdCache.put(address, Some(id))
    for ((orderId, volumeAndFee) <- newFills) volumeAndFeeCache.put(orderId, volumeAndFee)
    for ((address, assetMap)     <- updatedBalances; (asset, balance) <- assetMap) balancesCache.put((address, asset), balance)
    for (id                      <- assetsToInvalidate) assetDescriptionCache.invalidate(id)
    leaseBalanceCache.putAll(updatedLeaseBalances.asJava)
    scriptCache.putAll(diff.scripts.asJava)
    assetScriptCache.putAll(diff.assetScripts.asJava)
    blocksTs.put(newHeight, block.header.timestamp)

    accountDataCache.putAll(newData.asJava)
    continuationStatesCache.putAll(diff.continuationStates.asJava)

    forgetBlocks()
  }

  protected def doRollback(targetBlockId: ByteStr): Seq[(Block, ByteStr)]

  override def rollbackTo(targetBlockId: ByteStr): Either[String, Seq[(Block, ByteStr)]] = {
    for {
      height <- heightOf(targetBlockId)
        .toRight(s"No block with signature: $targetBlockId found in blockchain")
      _ <- Either
        .cond(
          height > safeRollbackHeight,
          (),
          s"Rollback is possible only to the block at a height: ${safeRollbackHeight + 1}"
        )
      discardedBlocks = doRollback(targetBlockId)
    } yield {
      current = (loadHeight(), loadScore(), loadLastBlock())

      activatedFeaturesCache = loadActivatedFeatures()
      approvedFeaturesCache = loadApprovedFeatures()
      discardedBlocks
    }
  }
}

object Caches {
  def cache[K <: AnyRef, V <: AnyRef](maximumSize: Int, loader: K => V): LoadingCache[K, V] =
    CacheBuilder
      .newBuilder()
      .maximumSize(maximumSize)
      .build(new CacheLoader[K, V] {
        override def load(key: K): V = loader(key)
      })

  def observedCache[K <: AnyRef, V <: AnyRef](maximumSize: Int, changed: Observer[K], loader: K => V)(implicit ct: ClassTag[K]): LoadingCache[K, V] =
    new ObservedLoadingCache(cache(maximumSize, loader), changed)
}
