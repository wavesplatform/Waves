package com.wavesplatform.database

import java.{lang, util}

import cats.data.Ior
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import com.google.common.collect.ArrayListMultimap
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.{Block, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.protobuf.{EthereumTransactionMeta, BlockMeta as PBBlockMeta}
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.settings.DBSettings
import com.wavesplatform.state.*
import com.wavesplatform.state.DiffToStateApplier.PortfolioUpdates
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.{Asset, Transaction}
import com.wavesplatform.utils.ObservedLoadingCache
import monix.reactive.Observer

import scala.collection.immutable.VectorMap
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

abstract class Caches extends Blockchain with Storage {
  import Caches.*

  val dbSettings: DBSettings

  @volatile
  private var current = loadCurrentBlock()

  private def loadCurrentBlock() = {
    val height = loadHeight()
    CurrentBlockInfo(height, loadBlockMeta(height), loadTxs(height))
  }

  protected def loadHeight(): Height
  protected def loadBlockMeta(height: Height): Option[PBBlockMeta]
  protected def loadTxs(height: Height): Seq[Transaction]

  override def height: Int = current.height

  override def score: BigInt = current.score

  override def lastBlock: Option[Block] = current.block

  override def blockHeader(height: Int): Option[SignedBlockHeader] =
    if (current.height == height) current.signedHeader else loadBlockMeta(Height(height)).map(toSignedHeader)

  override def hitSource(height: Int): Option[ByteStr] =
    if (current.height == height) current.hitSource else loadBlockMeta(Height(height)).map(toHitSource)

  def loadHeightOf(blockId: ByteStr): Option[Int]

  override def heightOf(blockId: ByteStr): Option[Int] = if (current.id.contains(blockId)) Some(height) else loadHeightOf(blockId)

  protected val leaseBalanceCache: LoadingCache[Address, CurrentLeaseBalance] =
    cache(dbSettings.maxCacheSize, loadLeaseBalance, keys => loadLeaseBalances(keys.asScala.toSeq).asJava)
  protected def loadLeaseBalance(address: Address): CurrentLeaseBalance
  protected def loadLeaseBalances(addresses: Seq[Address]): Map[Address, CurrentLeaseBalance]
  protected def discardLeaseBalance(address: Address): Unit = leaseBalanceCache.invalidate(address)
  override def leaseBalance(address: Address): LeaseBalance = {
    val currentLeaseBalance = leaseBalanceCache.get(address)
    LeaseBalance(currentLeaseBalance.in, currentLeaseBalance.out)
  }

  override def leaseBalances(addresses: Seq[Address]): Map[Address, LeaseBalance] = {
    leaseBalanceCache
      .getAll(addresses.asJava)
      .asScala
      .view
      .map { case (address, leaseBalance) =>
        address -> LeaseBalance(leaseBalance.in, leaseBalance.out)
      }
      .toMap
  }

  protected val balancesCache: LoadingCache[(Address, Asset), CurrentBalance] =
    cache(dbSettings.maxCacheSize * 16, loadBalance, keys => loadBalances(keys.asScala.toSeq).asJava)
  protected def discardBalance(key: (Address, Asset)): Unit         = balancesCache.invalidate(key)
  override def balance(address: Address, mayBeAssetId: Asset): Long = balancesCache.get(address -> mayBeAssetId).balance

  override def balances(req: Seq[(Address, Asset)]): Map[(Address, Asset), Long] =
    balancesCache
      .getAll(req.asJava)
      .asScala
      .view
      .map { case ((address, asset), balance) =>
        (address, asset) -> balance.balance
      }
      .toMap

  def loadCacheData(addresses: Set[Address], orders: Set[ByteStr]): Unit = {
    addressIdCache.getAll(addresses.asJava)
    balancesCache.getAll(addresses.map(_ -> Waves).asJava)
    leaseBalanceCache.getAll(addresses.asJava)
    volumeAndFeeCache.getAll(orders.asJava)
  }

  override def wavesBalances(addresses: Seq[Address]): Map[Address, Long] =
    balancesCache
      .getAll(addresses.map(_ -> Waves).asJava)
      .asScala
      .view
      .map { case ((address, _), balance) =>
        address -> balance.balance
      }
      .toMap
  protected def loadBalance(req: (Address, Asset)): CurrentBalance
  protected def loadBalances(req: Seq[(Address, Asset)]): Map[(Address, Asset), CurrentBalance]
  protected def loadWavesBalances(req: Seq[(Address, Asset)]): Map[(Address, Asset), CurrentBalance]

  private val assetDescriptionCache: LoadingCache[IssuedAsset, Option[AssetDescription]] = cache(dbSettings.maxCacheSize, loadAssetDescription)
  protected def loadAssetDescription(asset: IssuedAsset): Option[AssetDescription]
  protected def discardAssetDescription(asset: IssuedAsset): Unit             = assetDescriptionCache.invalidate(asset)
  override def assetDescription(asset: IssuedAsset): Option[AssetDescription] = assetDescriptionCache.get(asset)

  private val volumeAndFeeCache: LoadingCache[ByteStr, CurrentVolumeAndFee] =
    cache(dbSettings.maxCacheSize, loadVolumeAndFee, keys => loadVolumesAndFees(keys.asScala.toSeq).asJava)
  protected def loadVolumeAndFee(orderId: ByteStr): CurrentVolumeAndFee
  protected def loadVolumesAndFees(orders: Seq[ByteStr]): Map[ByteStr, CurrentVolumeAndFee]
  protected def discardVolumeAndFee(orderId: ByteStr): Unit = volumeAndFeeCache.invalidate(orderId)
  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee = {
    val curVf = volumeAndFeeCache.get(orderId)
    VolumeAndFee(curVf.volume, curVf.fee)
  }

  private val scriptCache: LoadingCache[Address, Option[AccountScriptInfo]] =
    CacheBuilder
      .newBuilder()
      .maximumWeight(128 << 20)
      .weigher((_: Address, asi: Option[AccountScriptInfo]) => asi.map(_.script.bytes().size).getOrElse(0))
      .recordStats()
      .build(new CacheLoader[Address, Option[AccountScriptInfo]] {
        override def load(key: Address): Option[AccountScriptInfo] = loadScript(key)
        override def loadAll(keys: lang.Iterable[? <: Address]): util.Map[Address, Option[AccountScriptInfo]] =
          new util.HashMap[Address, Option[AccountScriptInfo]]()
      })
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

  private val addressIdCache: LoadingCache[Address, Option[AddressId]] =
    cache(dbSettings.maxCacheSize, loadAddressId, keys => loadAddressIds(keys.asScala.toSeq).asJava)
  protected def loadAddressId(address: Address): Option[AddressId]
  protected def loadAddressIds(addresses: Seq[Address]): Map[Address, Option[AddressId]]

  protected def addressIdWithFallback(address: Address, newAddresses: Map[Address, AddressId]): AddressId =
    newAddresses.getOrElse(address, addressIdCache.get(address).get)

  private val accountDataCache: LoadingCache[(Address, String), CurrentData] = cache(
    dbSettings.maxCacheSize,
    { case (k, v) =>
      loadAccountData(k, v)
    }
  )

  override def accountData(acc: Address, key: String): Option[DataEntry[?]] =
    accountDataCache.get((acc, key)).entry match {
      case _: EmptyDataEntry => None
      case other             => Some(other)
    }

  protected def discardAccountData(addressWithKey: (Address, String)): Unit = accountDataCache.invalidate(addressWithKey)
  protected def loadAccountData(acc: Address, key: String): CurrentData

  private[database] def addressId(address: Address): Option[AddressId] = addressIdCache.get(address)
  private[database] def addressIds(addresses: Seq[Address]): Map[Address, Option[AddressId]] =
    addressIdCache.getAll(addresses.asJava).asScala.toMap

  protected val aliasCache: LoadingCache[Alias, Option[Address]] = cache(dbSettings.maxCacheSize, loadAlias)
  protected def loadAlias(alias: Alias): Option[Address]
  protected def discardAlias(alias: Alias): Unit = aliasCache.invalidate(alias)

  protected val blockHeightCache: LoadingCache[ByteStr, Option[Int]] = cache(dbSettings.maxRollbackDepth + 1000, loadBlockHeight)
  protected def loadBlockHeight(blockId: ByteStr): Option[Int]
  protected def discardBlockHeight(blockId: ByteStr): Unit = blockHeightCache.invalidate(blockId)

  @volatile
  protected var approvedFeaturesCache: Map[Short, Int] = loadApprovedFeatures()
  protected def loadApprovedFeatures(): Map[Short, Int]
  override def approvedFeatures: Map[Short, Int] = approvedFeaturesCache

  @volatile
  protected var activatedFeaturesCache: Map[Short, Int] = loadActivatedFeatures()
  protected def loadActivatedFeatures(): Map[Short, Int]
  override def activatedFeatures: Map[Short, Int] = activatedFeaturesCache

  // noinspection ScalaStyle
  protected def doAppend(
      blockMeta: PBBlockMeta,
      carry: Long,
      newAddresses: Map[Address, AddressId],
      balances: Map[(AddressId, Asset), (CurrentBalance, BalanceNode)],
      leaseBalances: Map[AddressId, (CurrentLeaseBalance, LeaseBalanceNode)],
      addressTransactions: util.Map[AddressId, util.Collection[TransactionId]],
      leaseStates: Map[ByteStr, LeaseDetails],
      issuedAssets: VectorMap[IssuedAsset, NewAssetInfo],
      reissuedAssets: Map[IssuedAsset, Ior[AssetInfo, AssetVolumeInfo]],
      filledQuantity: Map[ByteStr, (CurrentVolumeAndFee, VolumeAndFeeNode)],
      scripts: Map[AddressId, Option[AccountScriptInfo]],
      assetScripts: Map[IssuedAsset, Option[AssetScriptInfo]],
      data: Map[(Address, String), (CurrentData, DataNode)],
      aliases: Map[Alias, AddressId],
      sponsorship: Map[IssuedAsset, Sponsorship],
      scriptResults: Map[ByteStr, InvokeScriptResult],
      transactionMeta: Seq[(TxMeta, Transaction)],
      stateHash: StateHashBuilder.Result,
      ethereumTransactionMeta: Map[ByteStr, EthereumTransactionMeta]
  ): Unit

  override def append(diff: Diff, carryFee: Long, totalFee: Long, reward: Option[Long], hitSource: ByteStr, block: Block): Unit = {
    val newHeight = current.height + 1
    val newScore  = block.blockScore() + current.score
    val newMeta = PBBlockMeta(
      Some(PBBlocks.protobuf(block.header)),
      ByteString.copyFrom(block.signature.arr),
      if (block.header.version >= Block.ProtoBlockVersion) ByteString.copyFrom(block.id().arr) else ByteString.EMPTY,
      newHeight,
      block.bytes().length,
      block.transactionData.size,
      totalFee,
      reward.getOrElse(0),
      if (block.header.version >= Block.ProtoBlockVersion) ByteString.copyFrom(hitSource.arr) else ByteString.EMPTY,
      ByteString.copyFrom(newScore.toByteArray),
      current.meta.fold(settings.genesisSettings.initialBalance)(_.totalWavesAmount) + reward.getOrElse(0L)
    )

    val stateHash = new StateHashBuilder

    val newAddresses = Set.newBuilder[Address]
    newAddresses ++= diff.portfolios.keys.filter(addressIdCache.get(_).isEmpty)
    for (NewTransactionInfo(_, addresses, _, _) <- diff.transactions; address <- addresses if addressIdCache.get(address).isEmpty) {
      newAddresses += address
    }

    val newAddressIds = (for {
      (address, offset) <- newAddresses.result().zipWithIndex
    } yield address -> AddressId(lastAddressId + offset + 1)).toMap

    lastAddressId += newAddressIds.size

    val PortfolioUpdates(updatedBalances, updatedLeaseBalances) = DiffToStateApplier.portfolios(this, diff)

    val leaseBalances = updatedLeaseBalances.map { case (address, lb) =>
      val prevCurrentLeaseBalance = leaseBalanceCache.get(address)
      address ->
        (
          CurrentLeaseBalance(lb.in, lb.out, Height(newHeight), prevCurrentLeaseBalance.height),
          LeaseBalanceNode(lb.in, lb.out, prevCurrentLeaseBalance.height)
        )
    }

    val newFills = for {
      (orderId, fillInfo) <- diff.orderFills
    } yield {
      val prev = volumeAndFeeCache.get(orderId)
      orderId -> (CurrentVolumeAndFee(prev.volume + fillInfo.volume, prev.fee + fillInfo.fee, Height(newHeight), prev.height), VolumeAndFeeNode(
        prev.volume + fillInfo.volume,
        prev.fee + fillInfo.fee,
        prev.height
      ))
    }

    val transactionMeta     = Seq.newBuilder[(TxMeta, Transaction)]
    val addressTransactions = ArrayListMultimap.create[AddressId, TransactionId]()
    for (nti <- diff.transactions) {
      transactionMeta += (TxMeta(Height(newHeight), nti.status, nti.spentComplexity) -> nti.transaction)
      for (addr <- nti.affected) {
        addressTransactions.put(addressIdWithFallback(addr, newAddressIds), TransactionId(nti.transaction.id()))
      }
    }

    current = CurrentBlockInfo(Height(newHeight), Some(newMeta), block.transactionData)

    val updatedBalanceNodes = for {
      (address, assets) <- updatedBalances
      (asset, balance)  <- assets
    } yield {
      asset match {
        case Waves              => stateHash.addWavesBalance(address, balance)
        case asset: IssuedAsset => stateHash.addAssetBalance(address, asset, balance)
      }
      val key                = (address, asset)
      val prevCurrentBalance = balancesCache.get(key)
      key ->
        (CurrentBalance(balance, Height(newHeight), prevCurrentBalance.height), BalanceNode(balance, prevCurrentBalance.height))
    }

    updatedLeaseBalances foreach { case (address, balance) =>
      stateHash.addLeaseBalance(address, balance.in, balance.out)
    }

    val updatedData = for {
      (address, data) <- diff.accountData
      entry           <- data.values
    } yield {
      stateHash.addDataEntry(address, entry)
      val entryKey   = (address, entry.key)
      val prevHeight = accountDataCache.get(entryKey).height
      entryKey -> (CurrentData(entry, Height(newHeight), prevHeight) -> DataNode(entry, prevHeight))
    }

    diff.aliases.foreach { case (alias, address) =>
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

    diff.leaseState.foreach { case (leaseId, details) =>
      stateHash.addLeaseStatus(TransactionId @@ leaseId, details.isActive)
    }

    diff.sponsorship.foreach { case (asset, sponsorship) =>
      stateHash.addSponsorship(
        asset,
        sponsorship match {
          case SponsorshipValue(minFee) => minFee
          case SponsorshipNoInfo        => 0L
        }
      )
    }

    doAppend(
      newMeta,
      carryFee,
      newAddressIds,
      updatedBalanceNodes.map { case ((address, asset), v) => (addressIdWithFallback(address, newAddressIds), asset) -> v },
      leaseBalances.map { case (address, balance) => addressIdWithFallback(address, newAddressIds) -> balance },
      addressTransactions.asMap(),
      diff.leaseState,
      diff.issuedAssets,
      diff.updatedAssets,
      newFills,
      diff.scripts.map { case (address, s) => addressIdWithFallback(address, newAddressIds) -> s },
      diff.assetScripts,
      updatedData,
      diff.aliases.map { case (a, address) => a -> addressIdWithFallback(address, newAddressIds) },
      diff.sponsorship,
      diff.scriptResults,
      transactionMeta.result(),
      stateHash.result(),
      diff.ethereumTransactionMeta
    )

    val assetsToInvalidate =
      diff.issuedAssets.keySet ++
        diff.updatedAssets.keySet ++
        diff.sponsorship.keySet ++
        diff.assetScripts.keySet

    for ((address, id)                       <- newAddressIds) addressIdCache.put(address, Some(id))
    for ((orderId, (volumeAndFee, _))        <- newFills) volumeAndFeeCache.put(orderId, volumeAndFee)
    for (((address, asset), (newBalance, _)) <- updatedBalanceNodes) balancesCache.put((address, asset), newBalance)
    for (id                                  <- assetsToInvalidate) assetDescriptionCache.invalidate(id)
    for ((alias, address)                    <- diff.aliases) aliasCache.put(alias, Some(address))
    leaseBalanceCache.putAll(leaseBalances.view.mapValues(_._1).toMap.asJava)
    scriptCache.putAll(diff.scripts.asJava)
    assetScriptCache.putAll(diff.assetScripts.asJava)
    accountDataCache.putAll(updatedData.view.mapValues(_._1).toMap.asJava)
  }

  protected def doRollback(targetHeight: Int): Seq[(Block, ByteStr)]

  override def rollbackTo(height: Int): Either[String, Seq[(Block, ByteStr)]] = {
    for {
      _ <- Either
        .cond(
          height >= safeRollbackHeight,
          (),
          s"Rollback is possible only to the block at the height: $safeRollbackHeight"
        )
      discardedBlocks = doRollback(height)
    } yield {
      current = loadCurrentBlock()

      activatedFeaturesCache = loadActivatedFeatures()
      approvedFeaturesCache = loadApprovedFeatures()
      discardedBlocks
    }
  }
}

object Caches {
  case class CurrentBlockInfo(height: Height, meta: Option[PBBlockMeta], transactions: Seq[Transaction]) {
    lazy val score: BigInt                           = meta.filterNot(_.totalScore.isEmpty).fold(BigInt(0))(m => BigInt(m.totalScore.toByteArray))
    lazy val block: Option[Block]                    = signedHeader.map(h => Block(h.header, h.signature, transactions))
    lazy val signedHeader: Option[SignedBlockHeader] = meta.map(toSignedHeader)
    lazy val id: Option[ByteStr]                     = meta.map(_.id)
    lazy val hitSource: Option[ByteStr]              = meta.map(toHitSource)
  }

  def toHitSource(m: PBBlockMeta): ByteStr = (if (m.vrf.isEmpty) m.getHeader.generationSignature else m.vrf).toByteStr

  def toSignedHeader(m: PBBlockMeta): SignedBlockHeader = SignedBlockHeader(PBBlocks.vanilla(m.getHeader), m.signature.toByteStr)

  def cache[K <: AnyRef, V <: AnyRef](
      maximumSize: Int,
      loader: K => V,
      batchLoader: lang.Iterable[? <: K] => util.Map[K, V] = { _: lang.Iterable[? <: K] => new util.HashMap[K, V]() }
  ): LoadingCache[K, V] =
    CacheBuilder
      .newBuilder()
      .maximumSize(maximumSize)
      .recordStats()
      .build(new CacheLoader[K, V] {
        override def load(key: K): V                                      = loader(key)
        override def loadAll(keys: lang.Iterable[? <: K]): util.Map[K, V] = batchLoader(keys)
      })

  def observedCache[K <: AnyRef, V <: AnyRef](maximumSize: Int, changed: Observer[K], loader: K => V)(implicit ct: ClassTag[K]): LoadingCache[K, V] =
    new ObservedLoadingCache(cache(maximumSize, loader), changed)
}
