package com.wavesplatform.database

import java.util

import cats.syntax.monoid._
import cats.syntax.option._
import com.google.common.cache._
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.EstimatorProvider._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.metrics.LevelDBStats
import com.wavesplatform.settings.DBSettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.{Asset, Transaction}
import com.wavesplatform.utils.{ObservedLoadingCache, ScorexLogging}
import monix.reactive.Observer

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.reflect.ClassTag

abstract class Caches(spendableBalanceChanged: Observer[(Address, Asset)]) extends Blockchain with ScorexLogging {
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

  def loadBlockHeaderAndSize(height: Int): Option[(BlockHeader, Int)]
  override def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)] = {
    val c = current
    if (height == c._1) {
      c._3.map(b => (b, b.bytes().length))
    } else {
      loadBlockHeaderAndSize(height)
    }
  }

  def loadBlockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)]
  override def blockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)] = {
    val c = current
    if (c._3.exists(_.uniqueId == blockId)) {
      c._3.map(b => (b, b.bytes().length))
    } else {
      loadBlockHeaderAndSize(blockId)
    }
  }

  def loadBlockBytes(height: Int): Option[Array[Byte]]
  override def blockBytes(height: Int): Option[Array[Byte]] = {
    val c = current
    if (height == c._1) {
      c._3.map(_.bytes())
    } else {
      loadBlockBytes(height)
    }
  }

  def loadBlockBytes(blockId: ByteStr): Option[Array[Byte]]
  override def blockBytes(blockId: ByteStr): Option[Array[Byte]] = {
    val c = current
    if (c._3.exists(_.uniqueId == blockId)) {
      c._3.map(_.bytes())
    } else {
      loadBlockBytes(blockId)
    }
  }

  def loadHeightOf(blockId: ByteStr): Option[Int]
  override def heightOf(blockId: ByteStr): Option[Int] = {
    val c = current
    if (c._3.exists(_.uniqueId == blockId)) {
      Some(c._1)
    } else {
      loadHeightOf(blockId)
    }
  }

  private val blocksTs                               = new util.TreeMap[Int, Long] // Height -> block timestamp, assume sorted by key.
  private var oldestStoredBlockTimestamp             = Long.MaxValue
  private val transactionIds                         = new util.HashMap[ByteStr, Int]() // TransactionId -> height
  protected def forgetTransaction(id: ByteStr): Unit = transactionIds.remove(id)
  override def containsTransaction(tx: Transaction): Boolean = transactionIds.containsKey(tx.id()) || {
    if (tx.timestamp - 2.hours.toMillis <= oldestStoredBlockTimestamp) {
      LevelDBStats.miss.record(1)
      transactionHeight(tx.id()).nonEmpty
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
    val bts = lastBlock.fold(0L)(_.timestamp) - dbSettings.rememberBlocks.toMillis
    blocksTs.entrySet().removeIf(_.getValue < bts)
    transactionIds.entrySet().removeIf(_.getValue < oldestBlock)
  }

  private val leaseBalanceCache: LoadingCache[Address, LeaseBalance] = cache(dbSettings.maxCacheSize, loadLeaseBalance)
  protected def loadLeaseBalance(address: Address): LeaseBalance
  protected def discardLeaseBalance(address: Address): Unit = leaseBalanceCache.invalidate(address)
  override def leaseBalance(address: Address): LeaseBalance = leaseBalanceCache.get(address)

  private[database] val portfolioCache: LoadingCache[Address, Portfolio] = cache(dbSettings.maxCacheSize / 4, _ => ???)
  protected def discardPortfolio(address: Address): Unit                 = portfolioCache.invalidate(address)

  private val balancesCache: LoadingCache[(Address, Asset), java.lang.Long] =
    observedCache(dbSettings.maxCacheSize * 16, spendableBalanceChanged, loadBalance)
  protected def clearBalancesCache(): Unit = balancesCache.invalidateAll()
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

  private def withComplexity(s: Script): (Script, Long) = (s, Script.verifierComplexity(s, this.estimator).explicitGet())

  private val scriptCache: LoadingCache[Address, Option[(Script, Long)]] = cache(dbSettings.maxCacheSize, loadScript(_).map(withComplexity))
  protected def loadScript(address: Address): Option[Script]
  protected def hasScriptBytes(address: Address): Boolean
  protected def discardScript(address: Address): Unit = scriptCache.invalidate(address)

  override def accountScriptWithComplexity(address: Address): Option[(Script, Long)] = scriptCache.get(address)
  override def hasScript(address: Address): Boolean =
    Option(scriptCache.getIfPresent(address)).map(_.nonEmpty).getOrElse(hasScriptBytes(address))

  private val assetScriptCache: LoadingCache[IssuedAsset, Option[(Script, Long)]] = cache(dbSettings.maxCacheSize, loadAssetScript(_).map(withComplexity))
  protected def loadAssetScript(asset: IssuedAsset): Option[Script]
  protected def hasAssetScriptBytes(asset: IssuedAsset): Boolean
  protected def discardAssetScript(asset: IssuedAsset): Unit = assetScriptCache.invalidate(asset)

  override def assetScriptWithComplexity(asset: IssuedAsset): Option[(Script, Long)] = assetScriptCache.get(asset)
  override def hasAssetScript(asset: IssuedAsset): Boolean =
    assetScriptCache.getIfPresent(asset) match {
      case null => hasAssetScriptBytes(asset)
      case x    => x.nonEmpty
    }

  private var lastAddressId = loadMaxAddressId()
  protected def loadMaxAddressId(): BigInt

  private val addressIdCache: LoadingCache[Address, Option[BigInt]] = cache(dbSettings.maxCacheSize, loadAddressId)
  protected def loadAddressId(address: Address): Option[BigInt]

  private val accountDataCache: LoadingCache[(Address, String), Option[DataEntry[_]]] = cache(dbSettings.maxCacheSize, loadAccountData)

  override def accountData(acc: Address, key: String): Option[DataEntry[_]] = accountDataCache.get((acc, key))
  protected def discardAccountData(addressWithKey: (Address, String)): Unit = accountDataCache.invalidate(addressWithKey)
  protected def loadAccountData(addressWithKey: (Address, String)): Option[DataEntry[_]]

  private[database] def addressId(address: Address): Option[BigInt] = addressIdCache.get(address)

  @volatile
  protected var approvedFeaturesCache: Map[Short, Int] = loadApprovedFeatures()
  protected def loadApprovedFeatures(): Map[Short, Int]
  override def approvedFeatures: Map[Short, Int] = approvedFeaturesCache

  @volatile
  protected var activatedFeaturesCache: Map[Short, Int] = loadActivatedFeatures()
  protected def loadActivatedFeatures(): Map[Short, Int]
  override def activatedFeatures: Map[Short, Int] = activatedFeaturesCache

  @volatile
  protected var lastBlockRewardCache: Option[Long] = loadLastBlockReward()
  protected def loadLastBlockReward(): Option[Long]
  override def lastBlockReward: Option[Long] = loadLastBlockReward()

  //noinspection ScalaStyle
  protected def doAppend(block: Block,
                         carry: Long,
                         newAddresses: Map[Address, BigInt],
                         wavesBalances: Map[BigInt, Long],
                         assetBalances: Map[BigInt, Map[IssuedAsset, Long]],
                         leaseBalances: Map[BigInt, LeaseBalance],
                         addressTransactions: Map[AddressId, List[TransactionId]],
                         leaseStates: Map[ByteStr, Boolean],
                         reissuedAssets: Map[IssuedAsset, AssetInfo],
                         filledQuantity: Map[ByteStr, VolumeAndFee],
                         scripts: Map[BigInt, Option[(Script, Long)]],
                         assetScripts: Map[IssuedAsset, Option[(Script, Long)]],
                         data: Map[BigInt, AccountDataInfo],
                         aliases: Map[Alias, BigInt],
                         sponsorship: Map[IssuedAsset, Sponsorship],
                         totalFee: Long,
                         reward: Option[Long],
                         scriptResults: Map[ByteStr, InvokeScriptResult]): Unit

  def append(diff: Diff, carryFee: Long, totalFee: Long, reward: Option[Long], block: Block): Unit = {
    val newHeight = current._1 + 1

    val newAddresses = Set.newBuilder[Address]
    newAddresses ++= diff.portfolios.keys.filter(addressIdCache.get(_).isEmpty)
    for ((_, _, addresses) <- diff.transactions.values; address <- addresses if addressIdCache.get(address).isEmpty) {
      newAddresses += address
    }

    val newAddressIds = (for {
      (address, offset) <- newAddresses.result().zipWithIndex
    } yield address -> (lastAddressId + offset + 1)).toMap

    def addressId(address: Address): BigInt = (newAddressIds.get(address) orElse addressIdCache.get(address)).get

    lastAddressId += newAddressIds.size

    log.trace(s"CACHE newAddressIds = $newAddressIds")
    log.trace(s"CACHE lastAddressId = $lastAddressId")

    val wavesBalances        = Map.newBuilder[BigInt, Long]
    val assetBalances        = Map.newBuilder[BigInt, Map[IssuedAsset, Long]]
    val leaseBalances        = Map.newBuilder[BigInt, LeaseBalance]
    val updatedLeaseBalances = Map.newBuilder[Address, LeaseBalance]
    val newPortfolios        = Seq.newBuilder[Address]
    val newBalances          = Map.newBuilder[(Address, Asset), java.lang.Long]

    for ((address, portfolioDiff) <- diff.portfolios) {
      val aid = addressId(address)
      if (portfolioDiff.balance != 0) {
        val wbalance = portfolioDiff.balance + balance(address, Waves)
        wavesBalances += aid            -> wbalance
        newBalances += (address, Waves) -> wbalance
      }

      if (portfolioDiff.lease != LeaseBalance.empty) {
        val lease = leaseBalance(address).combine(portfolioDiff.lease)
        leaseBalances += aid            -> lease
        updatedLeaseBalances += address -> lease
      }

      if (portfolioDiff.assets.nonEmpty) {
        val newAssetBalances = for { (k, v) <- portfolioDiff.assets if v != 0 } yield {
          val abalance = v + balance(address, k)
          newBalances += (address, k) -> abalance
          k                           -> abalance
        }
        if (newAssetBalances.nonEmpty) {
          assetBalances += aid -> newAssetBalances
        }
      }

      newPortfolios += address
    }

    val newFills = for {
      (orderId, fillInfo) <- diff.orderFills
    } yield orderId -> volumeAndFeeCache.get(orderId).combine(fillInfo)

    val transactionList = diff.transactions.toList

    transactionList.foreach {
      case (_, (_, tx, _)) =>
        transactionIds.put(tx.id(), newHeight)
    }

    val addressTransactions: Map[AddressId, List[TransactionId]] =
      transactionList
        .flatMap {
          case (_, (h, tx, addrs)) =>
            transactionIds.put(tx.id(), newHeight) // be careful here!

            addrs.map { addr =>
              val addrId = AddressId(addressId(addr))
              addrId -> TransactionId(tx.id())
            }
        }
        .groupBy(_._1)
        .mapValues(_.map {
          case (_, txId) => txId
        })

    current = (newHeight, current._2 + block.blockScore(), Some(block))

    doAppend(
      block,
      carryFee,
      newAddressIds,
      wavesBalances.result(),
      assetBalances.result(),
      leaseBalances.result(),
      addressTransactions,
      diff.leaseState,
      diff.issuedAssets,
      newFills,
      diff.scripts.map { case (address, s) => addressId(address) -> s },
      diff.assetScripts,
      diff.accountData.map { case (address, data) => addressId(address) -> data },
      diff.aliases.map { case (a, address)        => a                  -> addressId(address) },
      diff.sponsorship,
      totalFee,
      reward,
      diff.scriptResults
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

    for ((address, id)           <- newAddressIds) addressIdCache.put(address, Some(id))
    for ((orderId, volumeAndFee) <- newFills) volumeAndFeeCache.put(orderId, volumeAndFee)
    balancesCache.putAll(newBalances.result().asJava)
    for (address <- newPortfolios.result()) discardPortfolio(address)
    for (id      <- diff.issuedAssets.keySet ++ diff.sponsorship.keySet) assetDescriptionCache.invalidate(id)
    leaseBalanceCache.putAll(updatedLeaseBalances.result().asJava)
    scriptCache.putAll(diff.scripts.asJava)
    assetScriptCache.putAll(diff.assetScripts.asJava)
    blocksTs.put(newHeight, block.timestamp)

    accountDataCache.putAll(newData.asJava)

    forgetBlocks()
  }

  protected def doRollback(targetBlockId: ByteStr): Seq[Block]

  def rollbackTo(targetBlockId: ByteStr): Either[String, Seq[Block]] = {
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
