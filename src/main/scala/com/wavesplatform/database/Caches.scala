package com.wavesplatform.database

import java.util

import cats.syntax.monoid._
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.state._
import com.wavesplatform.transaction.{AssetId, Transaction}
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.metrics.LevelDBStats

import scala.collection.JavaConverters._
import scala.concurrent.duration._

trait Caches extends Blockchain with ScorexLogging {
  import Caches._

  protected def maxCacheSize: Int

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
  override def scoreOf(blockId: ByteStr): Option[BigInt] = {
    val c = current
    if (c._3.exists(_.uniqueId == blockId)) {
      Some(c._2)
    } else {
      loadScoreOf(blockId)
    }
  }

  def loadBlockHeaderAndSize(height: Int): Option[(BlockHeader, Int)]
  override def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)] = {
    val c = current
    if (height == c._1) {
      c._3.map(b => (b, b.bytes().size))
    } else {
      loadBlockHeaderAndSize(height)
    }
  }

  def loadBlockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)]
  override def blockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)] = {
    val c = current
    if (c._3.exists(_.uniqueId == blockId)) {
      c._3.map(b => (b, b.bytes().size))
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

  protected def rememberBlocksInterval: Long

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
    val bts = lastBlock.fold(0L)(_.timestamp) - rememberBlocksInterval
    blocksTs.entrySet().removeIf(_.getValue < bts)
    transactionIds.entrySet().removeIf(_.getValue < oldestBlock)
  }

  private val portfolioCache: LoadingCache[Address, Portfolio] = cache(maxCacheSize, loadPortfolio)
  protected def loadPortfolio(address: Address): Portfolio
  protected def discardPortfolio(address: Address): Unit = portfolioCache.invalidate(address)
  override def portfolio(a: Address): Portfolio          = portfolioCache.get(a)

  private val assetDescriptionCache: LoadingCache[AssetId, Option[AssetDescription]] = cache(maxCacheSize, loadAssetDescription)
  protected def loadAssetDescription(assetId: AssetId): Option[AssetDescription]
  protected def discardAssetDescription(assetId: AssetId): Unit             = assetDescriptionCache.invalidate(assetId)
  override def assetDescription(assetId: AssetId): Option[AssetDescription] = assetDescriptionCache.get(assetId)

  private val volumeAndFeeCache: LoadingCache[ByteStr, VolumeAndFee] = cache(maxCacheSize, loadVolumeAndFee)
  protected def loadVolumeAndFee(orderId: ByteStr): VolumeAndFee
  protected def discardVolumeAndFee(orderId: ByteStr): Unit       = volumeAndFeeCache.invalidate(orderId)
  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee = volumeAndFeeCache.get(orderId)

  private val scriptCache: LoadingCache[Address, Option[Script]] = cache(maxCacheSize, loadScript)
  protected def loadScript(address: Address): Option[Script]
  protected def hasScriptBytes(address: Address): Boolean
  protected def discardScript(address: Address): Unit = scriptCache.invalidate(address)

  override def accountScript(address: Address): Option[Script] = scriptCache.get(address)
  override def hasScript(address: Address): Boolean =
    Option(scriptCache.getIfPresent(address)).flatten.isDefined || hasScriptBytes(address)

  private val assetScriptCache: LoadingCache[AssetId, Option[Script]] = cache(maxCacheSize, loadAssetScript)
  protected def loadAssetScript(asset: AssetId): Option[Script]
  protected def hasAssetScriptBytes(asset: AssetId): Boolean
  protected def discardAssetScript(asset: AssetId): Unit = assetScriptCache.invalidate(asset)

  override def assetScript(asset: AssetId): Option[Script] = assetScriptCache.get(asset)
  override def hasAssetScript(asset: AssetId): Boolean =
    assetScriptCache.getIfPresent(asset) match {
      case null => hasAssetScriptBytes(asset)
      case x    => x.nonEmpty
    }

  private var lastAddressId = loadMaxAddressId()
  protected def loadMaxAddressId(): BigInt

  private val addressIdCache: LoadingCache[Address, Option[BigInt]] = cache(maxCacheSize, loadAddressId)
  protected def loadAddressId(address: Address): Option[BigInt]
  protected def addressId(address: Address): Option[BigInt] = addressIdCache.get(address)

  @volatile
  protected var approvedFeaturesCache: Map[Short, Int] = loadApprovedFeatures()
  protected def loadApprovedFeatures(): Map[Short, Int]
  override def approvedFeatures: Map[Short, Int] = approvedFeaturesCache

  @volatile
  protected var activatedFeaturesCache: Map[Short, Int] = loadActivatedFeatures()
  protected def loadActivatedFeatures(): Map[Short, Int]
  override def activatedFeatures: Map[Short, Int] = activatedFeaturesCache

  protected def doAppend(block: Block,
                         carryFee: Long,
                         addresses: Map[Address, BigInt],
                         wavesBalances: Map[BigInt, Long],
                         assetBalances: Map[BigInt, Map[ByteStr, Long]],
                         leaseBalances: Map[BigInt, LeaseBalance],
                         leaseStates: Map[ByteStr, Boolean],
                         transactions: Map[ByteStr, (Transaction, Set[BigInt])],
                         addressTransactions: Map[BigInt, List[(Int, ByteStr)]],
                         reissuedAssets: Map[ByteStr, AssetInfo],
                         filledQuantity: Map[ByteStr, VolumeAndFee],
                         scripts: Map[BigInt, Option[Script]],
                         assetScripts: Map[ByteStr, Option[Script]],
                         data: Map[BigInt, AccountDataInfo],
                         aliases: Map[Alias, BigInt],
                         sponsorship: Map[AssetId, Sponsorship]): Unit

  override def append(diff: Diff, carryFee: Long, block: Block): Unit = {
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

    val wavesBalances = Map.newBuilder[BigInt, Long]
    val assetBalances = Map.newBuilder[BigInt, Map[ByteStr, Long]]
    val leaseBalances = Map.newBuilder[BigInt, LeaseBalance]
    val newPortfolios = Map.newBuilder[Address, Portfolio]

    for ((address, portfolioDiff) <- diff.portfolios) {
      val newPortfolio = portfolioCache.get(address).combine(portfolioDiff)
      if (portfolioDiff.balance != 0) {
        wavesBalances += addressId(address) -> newPortfolio.balance
      }

      if (portfolioDiff.lease != LeaseBalance.empty) {
        leaseBalances += addressId(address) -> newPortfolio.lease
      }

      if (portfolioDiff.assets.nonEmpty) {
        val newAssetBalances = for { (k, v) <- portfolioDiff.assets if v != 0 } yield k -> newPortfolio.assets(k)
        if (newAssetBalances.nonEmpty) {
          assetBalances += addressId(address) -> newAssetBalances
        }
      }

      newPortfolios += address -> newPortfolio
    }

    val newFills = for {
      (orderId, fillInfo) <- diff.orderFills
    } yield orderId -> volumeAndFeeCache.get(orderId).combine(fillInfo)

    val newTransactions = Map.newBuilder[ByteStr, (Transaction, Set[BigInt])]
    for ((id, (_, tx, addresses)) <- diff.transactions) {
      transactionIds.put(id, newHeight)
      newTransactions += id -> ((tx, addresses.map(addressId)))
    }

    current = (newHeight, (current._2 + block.blockScore()), Some(block))

    doAppend(
      block,
      carryFee,
      newAddressIds,
      wavesBalances.result(),
      assetBalances.result(),
      leaseBalances.result(),
      diff.leaseState,
      newTransactions.result(),
      diff.accountTransactionIds.map({ case (addr, txs) => addressId(addr) -> txs }),
      diff.issuedAssets,
      newFills,
      diff.scripts.map { case (address, s) => addressId(address) -> s },
      diff.assetScripts,
      diff.accountData.map { case (address, data) => addressId(address) -> data },
      diff.aliases.map { case (a, address)        => a                  -> addressId(address) },
      diff.sponsorship
    )

    for ((address, id)           <- newAddressIds) addressIdCache.put(address, Some(id))
    for ((orderId, volumeAndFee) <- newFills) volumeAndFeeCache.put(orderId, volumeAndFee)
    for ((address, portfolio)    <- newPortfolios.result()) portfolioCache.put(address, portfolio)
    for (id                      <- diff.issuedAssets.keySet ++ diff.sponsorship.keySet) assetDescriptionCache.invalidate(id)
    scriptCache.putAll(diff.scripts.asJava)
    assetScriptCache.putAll(diff.assetScripts.asJava)
    blocksTs.put(newHeight, block.timestamp)
    forgetBlocks()
  }

  protected def doRollback(targetBlockId: ByteStr): Seq[Block]

  override def rollbackTo(targetBlockId: ByteStr): Either[String, Seq[Block]] = {
    for {
      height <- heightOf(targetBlockId)
        .toRight(s"No block with signature: $targetBlockId found in blockchain")
      _ <- Either
        .cond(
          height > safeRollbackHeight,
          (),
          s"Rollback is possible only to the block at a height: $safeRollbackHeight"
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
        override def load(key: K) = loader(key)
      })
}
