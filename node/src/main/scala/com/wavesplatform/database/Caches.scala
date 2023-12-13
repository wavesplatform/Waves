package com.wavesplatform.database

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import com.google.common.collect.ArrayListMultimap
import com.google.protobuf.ByteString
import com.typesafe.scalalogging.StrictLogging
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.{Block, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.protobuf.BlockMeta as PBBlockMeta
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.settings.DBSettings
import com.wavesplatform.state.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.{Asset, DiscardedBlocks, Transaction}
import com.wavesplatform.utils.ObservedLoadingCache
import monix.reactive.Observer
import org.ehcache.sizeof.SizeOf

import java.{lang, util}
import scala.collection.immutable.VectorMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

abstract class Caches extends Blockchain with Storage with StrictLogging {
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

  private val objectWeigher = SizeOf.newInstance()

  private val scriptCache: LoadingCache[Address, Option[AccountScriptInfo]] =
    CacheBuilder
      .newBuilder()
      .maximumWeight(128 << 20)
      .weigher((_: Address, asi: Option[AccountScriptInfo]) => asi.map(s => objectWeigher.deepSizeOf(s).toInt).getOrElse(0))
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
  protected def loadMultipleEntries(keys: Iterable[(Address, String)]): Map[(Address, String), CurrentData]

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

  // Also contains features those will be activated in the future (activationHeight > currentHeight), because they were approved now or before.
  @volatile
  protected var activatedFeaturesCache: Map[Short, Int] = loadActivatedFeatures()
  protected def loadActivatedFeatures(): Map[Short, Int]
  override def activatedFeatures: Map[Short, Int] = activatedFeaturesCache

  protected def doAppend(
      blockMeta: PBBlockMeta,
      snapshot: StateSnapshot,
      carry: Long,
      computedBlockStateHash: ByteStr,
      newAddresses: Map[Address, AddressId],
      balances: Map[(AddressId, Asset), (CurrentBalance, BalanceNode)],
      leaseBalances: Map[AddressId, (CurrentLeaseBalance, LeaseBalanceNode)],
      filledQuantity: Map[ByteStr, (CurrentVolumeAndFee, VolumeAndFeeNode)],
      data: Map[(Address, String), (CurrentData, DataNode)],
      addressTransactions: util.Map[AddressId, util.Collection[TransactionId]],
      accountScripts: Map[AddressId, Option[AccountScriptInfo]],
      stateHash: StateHashBuilder.Result
  ): Unit

  private var prevTs         = System.currentTimeMillis()
  private var prevComplexity = 0L

  override def append(
      snapshot: StateSnapshot,
      carryFee: Long,
      totalFee: Long,
      reward: Option[Long],
      hitSource: ByteStr,
      computedBlockStateHash: ByteStr,
      block: Block
  ): Unit = {
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
    current = CurrentBlockInfo(Height(newHeight), Some(newMeta), block.transactionData)

    val newAddresses =
      mutable.Set[Address]() ++
        (snapshot.balances.map(_._1._1) ++ snapshot.leaseBalances.keys)
          .filter(addressIdCache.get(_).isEmpty)

    for (address <- snapshot.transactions.flatMap(_._2.affected) if addressIdCache.get(address).isEmpty)
      newAddresses += address

    val newAddressIds = (for {
      (address, offset) <- newAddresses.zipWithIndex
    } yield address -> AddressId(lastAddressId + offset + 1)).toMap

    lastAddressId += newAddressIds.size

    val leaseBalancesWithNodes = snapshot.leaseBalances.flatMap { case (address, lease) =>
      val prevCurrentLeaseBalance = leaseBalanceCache.get(address)
      if (prevCurrentLeaseBalance.in == lease.in && prevCurrentLeaseBalance.out == lease.out)
        Map()
      else
        Map(
          address ->
            (
              CurrentLeaseBalance(lease.in, lease.out, Height(height), prevCurrentLeaseBalance.height),
              LeaseBalanceNode(lease.in, lease.out, prevCurrentLeaseBalance.height)
            )
        )
    }
    val leaseBalances = leaseBalancesWithNodes.map { case (address, (balance, _)) =>
      (address, balance)
    }

    val addressTransactions = ArrayListMultimap.create[AddressId, TransactionId]()
    for ((_, nti) <- snapshot.transactions)
      for (addr <- nti.affected)
        addressTransactions.put(addressIdWithFallback(addr, newAddressIds), TransactionId(nti.transaction.id()))

    val updatedBalanceNodes = for {
      case ((address, asset), amount) <- snapshot.balances
      key         = (address, asset)
      prevBalance = balancesCache.get(key) if prevBalance.balance != amount
    } yield key -> (
      CurrentBalance(amount, Height(height), prevBalance.height),
      BalanceNode(amount, prevBalance.height)
    )

    val newEntries = for {
      (address, entries) <- snapshot.accountData
      (key, entry)       <- entries
    } yield ((address, key), entry)

    val cachedEntries = accountDataCache.getAllPresent(newEntries.keys.asJava).asScala
    val loadedPrevEntries = loadMultipleEntries(newEntries.keys.filterNot(cachedEntries.contains))

    val updatedDataWithNodes = (for {
      (k, currentEntry) <- cachedEntries ++ loadedPrevEntries
      newEntry          <- newEntries.get(k)
    } yield k -> (
      CurrentData(newEntry, Height(height), currentEntry.height),
      DataNode(newEntry, currentEntry.height)
    )).toMap

    val orderFillsWithNodes = for {
      (orderId, VolumeAndFee(volume, fee)) <- snapshot.orderFills
    } yield {
      val prevData = volumeAndFeeCache.get(orderId)
      val current  = CurrentVolumeAndFee(volume, fee, Height(height), prevData.height)
      val node     = VolumeAndFeeNode(volume, fee, prevData.height)
      orderId -> (current, node)
    }

    val stateHash = new StateHashBuilder
    for (((address, asset), (amount, _)) <- updatedBalanceNodes) asset match {
      case Waves              => stateHash.addWavesBalance(address, amount.balance)
      case asset: IssuedAsset => stateHash.addAssetBalance(address, asset, amount.balance)
    }
    for (((address, _), (entry, _)) <- updatedDataWithNodes) stateHash.addDataEntry(address, entry.entry)
    for ((address, lease)           <- leaseBalances) stateHash.addLeaseBalance(address, lease.in, lease.out)
    for ((address, script)          <- snapshot.accountScriptsByAddress) stateHash.addAccountScript(address, script.map(_.script))
    for ((asset, script)            <- snapshot.assetScripts) stateHash.addAssetScript(asset, Some(script.script))
    for ((asset, _)                 <- snapshot.assetStatics) if (!snapshot.assetScripts.contains(asset)) stateHash.addAssetScript(asset, None)
    for (leaseId <- snapshot.newLeases.keys) if (!snapshot.cancelledLeases.contains(leaseId)) stateHash.addLeaseStatus(leaseId, isActive = true)
    for (leaseId <- snapshot.cancelledLeases.keys) stateHash.addLeaseStatus(leaseId, isActive = false)
    for ((assetId, sponsorship) <- snapshot.sponsorships) stateHash.addSponsorship(assetId, sponsorship.minFee)
    for ((alias, address)       <- snapshot.aliases) stateHash.addAlias(address, alias.name)

    prevComplexity += snapshot.scriptsComplexity
    if (System.currentTimeMillis() - prevTs > 60000) {
      logger.info(s"COMP: $prevComplexity")
      prevComplexity = 0
      prevTs = System.currentTimeMillis()
    }

    doAppend(
      newMeta,
      snapshot,
      carryFee,
      computedBlockStateHash,
      newAddressIds,
      VectorMap() ++ updatedBalanceNodes.map { case ((address, asset), v) => (addressIdWithFallback(address, newAddressIds), asset) -> v },
      leaseBalancesWithNodes.map { case (address, balance) => addressIdWithFallback(address, newAddressIds) -> balance },
      orderFillsWithNodes,
      updatedDataWithNodes,
      addressTransactions.asMap(),
      snapshot.accountScriptsByAddress.map { case (address, s) => addressIdWithFallback(address, newAddressIds) -> s },
      stateHash.result()
    )

    val assetsToInvalidate =
      snapshot.assetStatics.keySet ++
        snapshot.assetScripts.keySet ++
        snapshot.assetNamesAndDescriptions.keySet ++
        snapshot.assetVolumes.keySet ++
        snapshot.sponsorships.keySet

    for ((address, id)                       <- newAddressIds) addressIdCache.put(address, Some(id))
    for ((orderId, (volumeAndFee, _))        <- orderFillsWithNodes) volumeAndFeeCache.put(orderId, volumeAndFee)
    for (((address, asset), (newBalance, _)) <- updatedBalanceNodes) balancesCache.put((address, asset), newBalance)
    for (id                                  <- assetsToInvalidate) assetDescriptionCache.invalidate(id)
    for ((alias, address)                    <- snapshot.aliases) aliasCache.put(Alias.create(alias.name).explicitGet(), Some(address))
    leaseBalanceCache.putAll(leaseBalances.asJava)
    scriptCache.putAll(snapshot.accountScriptsByAddress.asJava)
    assetScriptCache.putAll(snapshot.assetScripts.view.mapValues(Some(_)).toMap.asJava)
    accountDataCache.putAll(updatedDataWithNodes.map { case (key, (value, _)) => (key, value) }.asJava)
  }

  protected def doRollback(targetHeight: Int): DiscardedBlocks

  override def rollbackTo(height: Int): Either[String, DiscardedBlocks] = {
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
      batchLoader: lang.Iterable[? <: K] => util.Map[K, V] = { (_: lang.Iterable[? <: K]) => new util.HashMap[K, V]() }
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
