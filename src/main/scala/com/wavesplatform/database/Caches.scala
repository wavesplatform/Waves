package com.wavesplatform.database

import java.util

import cats.syntax.monoid._
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import com.wavesplatform.state.{
  AccountDataInfo,
  AssetDescription,
  AssetInfo,
  Blockchain,
  ByteStr,
  Diff,
  LeaseBalance,
  Portfolio,
  Sponsorship,
  SponsorshipValue,
  VolumeAndFee
}
import scorex.account.{Address, Alias}
import scorex.block.Block
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.smart.script.Script
import scorex.transaction.{AssetId, Transaction}

import scala.collection.JavaConverters._

trait Caches extends Blockchain {
  import Caches._

  private val MaxSize = 100000

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

  private val transactionIds                             = new util.HashMap[ByteStr, Long]()
  protected def forgetTransaction(id: ByteStr): Unit     = transactionIds.remove(id)
  override def containsTransaction(id: ByteStr): Boolean = transactionIds.containsKey(id)

  protected val portfolioCache: LoadingCache[Address, Portfolio] = cache(MaxSize, loadPortfolio)
  protected def loadPortfolio(address: Address): Portfolio
  override def portfolio(a: Address): Portfolio = portfolioCache.get(a)

  protected val assetInfoCache: LoadingCache[AssetId, Option[AssetInfo]] = cache(MaxSize, loadAssetInfo)
  protected def loadAssetInfo(assetId: AssetId): Option[AssetInfo]

  protected val assetDescriptionCache: LoadingCache[AssetId, Option[AssetDescription]] = cache(MaxSize, loadAssetDescription)
  protected def loadAssetDescription(assetId: AssetId): Option[AssetDescription]
  override def assetDescription(assetId: AssetId): Option[AssetDescription] = assetDescriptionCache.get(assetId) map { descr =>
    sponsorshipCache.get(assetId).fold(descr) {
      case SponsorshipValue(minFee) =>
        descr.copy(sponsorship = minFee)
    }
  }

  protected val volumeAndFeeCache: LoadingCache[ByteStr, VolumeAndFee] = cache(MaxSize, loadVolumeAndFee)
  protected def loadVolumeAndFee(orderId: ByteStr): VolumeAndFee
  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee = volumeAndFeeCache.get(orderId)

  protected val scriptCache: LoadingCache[Address, Option[Script]] = cache(MaxSize, loadScript)
  protected def loadScript(address: Address): Option[Script]
  override def accountScript(address: Address): Option[Script] = scriptCache.get(address)

  private var lastAddressId = loadMaxAddressId()
  protected def loadMaxAddressId(): BigInt

  protected val addressIdCache: LoadingCache[Address, Option[BigInt]] = cache(MaxSize, loadAddressId)
  protected def loadAddressId(address: Address): Option[BigInt]

  @volatile
  protected var approvedFeaturesCache: Map[Short, Int] = loadApprovedFeatures()
  protected def loadApprovedFeatures(): Map[Short, Int]
  override def approvedFeatures: Map[Short, Int] = approvedFeaturesCache

  @volatile
  protected var activatedFeaturesCache: Map[Short, Int] = loadActivatedFeatures()
  protected def loadActivatedFeatures(): Map[Short, Int]
  override def activatedFeatures: Map[Short, Int] = activatedFeaturesCache

  protected val sponsorshipCache: LoadingCache[ByteStr, Option[SponsorshipValue]] = cache(MaxSize, loadSponsorship)
  protected def loadSponsorship(assetId: ByteStr): Option[SponsorshipValue]

  protected def doAppend(block: Block,
                         addresses: Map[Address, BigInt],
                         wavesBalances: Map[BigInt, Long],
                         assetBalances: Map[BigInt, Map[ByteStr, Long]],
                         leaseBalances: Map[BigInt, LeaseBalance],
                         leaseStates: Map[ByteStr, Boolean],
                         transactions: Map[ByteStr, (Transaction, Set[BigInt])],
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
      (address, offset) <- newAddresses.result().toSeq.zipWithIndex
    } yield {
      val nextAddressId = lastAddressId + offset + 1
      addressIdCache.put(address, Some(nextAddressId))
      address -> nextAddressId
    }).toMap

    lastAddressId += newAddressIds.size

    transactionIds.entrySet().removeIf(kv => block.timestamp - kv.getValue > 2 * 60 * 60 * 1000)

    val wavesBalances = Map.newBuilder[BigInt, Long]
    val assetBalances = Map.newBuilder[BigInt, Map[ByteStr, Long]]
    val leaseBalances = Map.newBuilder[BigInt, LeaseBalance]

    for ((address, portfolioDiff) <- diff.portfolios) {
      val newPortfolio = portfolioCache.get(address).combine(portfolioDiff)
      if (portfolioDiff.balance != 0) {
        wavesBalances += addressIdCache.get(address).get -> newPortfolio.balance
      }

      if (portfolioDiff.lease != LeaseBalance.empty) {
        leaseBalances += addressIdCache.get(address).get -> newPortfolio.lease
      }

      if (portfolioDiff.assets.nonEmpty) {
        val newAssetBalances = for { (k, v) <- portfolioDiff.assets if v != 0 } yield k -> newPortfolio.assets(k)
        if (newAssetBalances.nonEmpty) {
          assetBalances += addressIdCache.get(address).get -> newAssetBalances
        }
      }

      portfolioCache.put(address, newPortfolio)
    }

    val newFills = Map.newBuilder[ByteStr, VolumeAndFee]

    for ((orderId, fillInfo) <- diff.orderFills) {
      val newVolumeAndFee = volumeAndFeeCache.get(orderId).combine(fillInfo)
      volumeAndFeeCache.put(orderId, newVolumeAndFee)
      newFills += orderId -> newVolumeAndFee
    }

    val newTransactions = Map.newBuilder[ByteStr, (Transaction, Set[BigInt])]
    for ((id, (_, tx, addresses)) <- diff.transactions) {
      transactionIds.put(id, tx.timestamp)
      newTransactions += id -> ((tx, addresses.map(a => addressIdCache.get(a).get)))
    }

    for ((id, sp: SponsorshipValue) <- diff.sponsorship) {
      sponsorshipCache.put(id, Some(sp))
    }

    for ((id, ai) <- diff.issuedAssets) {
      assetInfoCache.put(id, Some(ai))
      diff.transactions.get(id) match {
        case Some((_, it: IssueTransaction, _)) =>
          assetDescriptionCache.put(
            id,
            Some(
              AssetDescription(
                it.sender,
                it.name,
                it.description,
                it.decimals,
                ai.isReissuable,
                ai.volume,
                it.script,
                diff.sponsorship.get(id) match {
                  case Some(SponsorshipValue(v)) => v
                  case _                         => 0L
                }
              )
            )
          )
        case _ =>
      }
    }

    scriptCache.putAll(diff.scripts.asJava)

    doAppend(
      block,
      newAddressIds,
      wavesBalances.result(),
      assetBalances.result(),
      leaseBalances.result(),
      diff.leaseState,
      newTransactions.result(),
      diff.issuedAssets,
      newFills.result(),
      diff.scripts.map { case (address, s)        => addressIdCache.get(address).get -> s },
      diff.accountData.map { case (address, data) => addressIdCache.get(address).get -> data },
      diff.aliases.map { case (a, address)        => a                               -> addressIdCache.get(address).get },
      diff.sponsorship
    )
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
      .recordStats()
      .build(new CacheLoader[K, V] {
        override def load(key: K) = loader(key)
      })
}
