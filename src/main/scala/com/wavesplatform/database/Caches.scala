package com.wavesplatform.database

import java.util

import cats.syntax.monoid._
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import com.wavesplatform.state._
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.AssetId

import scala.collection.JavaConverters._

trait Caches extends Blockchain {
  import Caches._

  protected def maxCacheSize: Int

  @volatile
  private var heightCache = loadHeight()
  protected def loadHeight(): Int
  override def height: Int = heightCache

  @volatile
  private var scoreCache = loadScore()
  protected def loadScore(): BigInt
  override def score: BigInt = scoreCache

  @volatile
  private var lastBlockCache = loadLastBlock()
  protected def loadLastBlock(): Option[Block]
  override def lastBlock: Option[Block] = lastBlockCache

  private val transactionIds                                       = new util.HashMap[ByteStr, Long]()
  protected def forgetTransaction(id: ByteStr): Unit               = transactionIds.remove(id)
  override def containsTransaction(id: ByteStr): Boolean           = transactionIds.containsKey(id)
  override def learnTransactions(values: Map[ByteStr, Long]): Unit = transactionIds.putAll(values.asJava)
  override def forgetTransactions(pred: (ByteStr, Long) => Boolean): Map[ByteStr, Long] = {
    val removedTransactions = Map.newBuilder[ByteStr, Long]
    val iterator            = transactionIds.entrySet().iterator()
    while (iterator.hasNext) {
      val e = iterator.next()
      if (pred(e.getKey, e.getValue)) {
        removedTransactions += e.getKey -> e.getValue
        iterator.remove()
      }
    }
    removedTransactions.result()
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
                         data: Map[BigInt, AccountDataInfo],
                         aliases: Map[Alias, BigInt],
                         sponsorship: Map[AssetId, Sponsorship]): Unit

  override def append(diff: Diff, block: Block): Unit = {
    heightCache += 1
    scoreCache += block.blockScore()
    lastBlockCache = Some(block)

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
      transactionIds.put(id, tx.timestamp)
      newTransactions += id -> ((tx, addresses.map(addressId)))
    }

    doAppend(
      block,
      newAddressIds,
      wavesBalances.result(),
      assetBalances.result(),
      leaseBalances.result(),
      diff.leaseState,
      newTransactions.result(),
      diff.accountTransactionIds.map({ case (addr, txs) => addressId(addr) -> txs }),
      diff.issuedAssets,
      newFills,
      diff.scripts.map { case (address, s)        => addressId(address) -> s },
      diff.accountData.map { case (address, data) => addressId(address) -> data },
      diff.aliases.map { case (a, address)        => a                  -> addressId(address) },
      diff.sponsorship
    )

    for ((address, id)           <- newAddressIds) addressIdCache.put(address, Some(id))
    for ((orderId, volumeAndFee) <- newFills) volumeAndFeeCache.put(orderId, volumeAndFee)
    for ((address, portfolio)    <- newPortfolios.result()) portfolioCache.put(address, portfolio)
    for (id                      <- diff.issuedAssets.keySet ++ diff.sponsorship.keySet) assetDescriptionCache.invalidate(id)
    scriptCache.putAll(diff.scripts.asJava)
  }

  protected def doRollback(targetBlockId: ByteStr): Seq[Block]

  override def rollbackTo(targetBlockId: ByteStr): Seq[Block] = {
    val discardedBlocks = doRollback(targetBlockId)

    heightCache = loadHeight()
    scoreCache = loadScore()
    lastBlockCache = loadLastBlock()

    activatedFeaturesCache = loadActivatedFeatures()
    approvedFeaturesCache = loadApprovedFeatures()

    discardedBlocks
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
