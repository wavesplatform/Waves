package com.wavesplatform.database

import cats.implicits.catsSyntaxNestedBitraverse
import com.google.common.cache.CacheBuilder
import com.google.common.collect.MultimapBuilder
import com.google.common.hash.{BloomFilter, Funnels}
import com.google.common.primitives.Ints
import com.google.common.util.concurrent.MoreExecutors
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.common.WavesBalanceIterator
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.BlockSnapshot
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database
import com.wavesplatform.database.patch.DisableHijackedAliases
import com.wavesplatform.database.protobuf.{StaticAssetInfo, TransactionMeta, BlockMeta as PBBlockMeta}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.protobuf.snapshot.{TransactionStateSnapshot, TransactionStatus as PBStatus}
import com.wavesplatform.protobuf.{ByteStrExt, ByteStringExt, PBSnapshots}
import com.wavesplatform.settings.{BlockchainSettings, DBSettings}
import com.wavesplatform.state.*
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.EthereumTransaction.Transfer
import com.wavesplatform.transaction.TxValidationError.{AliasDoesNotExist, AliasIsDisabled}
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeExpressionTransaction, InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.utils.{LoggerFacade, ScorexLogging}
import io.netty.util.concurrent.DefaultThreadFactory
import org.rocksdb.{RocksDB, Status}
import org.slf4j.LoggerFactory
import sun.nio.ch.Util

import java.nio.ByteBuffer
import java.util
import java.util.concurrent.*
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.Using.Releasable
import scala.util.control.NonFatal

object RocksDBWriter extends ScorexLogging {

  /** {{{
    * ([10, 7, 4], 5, 11) => [10, 7, 4]
    * ([10, 7], 5, 11) => [10, 7, 1]
    * }}}
    */
  private[database] def slice(v: Seq[Int], from: Int, to: Int): Seq[Int] = {
    val (c1, c2) = v.dropWhile(_ > to).partition(_ > from)
    c1 :+ c2.headOption.getOrElse(1)
  }

  implicit class ReadOnlyDBExt(val db: ReadOnlyDB) extends AnyVal {
    def fromHistory[A](historyKey: Key[Seq[Int]], valueKey: Int => Key[A]): Option[A] =
      for {
        lastChange <- db.get(historyKey).headOption
      } yield db.get(valueKey(lastChange))

    def hasInHistory(historyKey: Key[Seq[Int]], v: Int => Key[?]): Boolean =
      db.get(historyKey)
        .headOption
        .exists(h => db.has(v(h)))
  }

  implicit class RWExt(val db: RW) extends AnyVal {
    def fromHistory[A](historyKey: Key[Seq[Int]], valueKey: Int => Key[A]): Option[A] =
      for {
        lastChange <- db.get(historyKey).headOption
      } yield db.get(valueKey(lastChange))
  }

  private def loadHeight(db: RocksDB): Height = db.get(Keys.height)

  private[database] def merge(wbh: Seq[Int], lbh: Seq[Int]): Seq[(Int, Int)] = {

    /** Fixed implementation where {{{([15, 12, 3], [12, 5]) => [(15, 12), (12, 12), (3, 5)]}}}
      */
    @tailrec
    def recMergeFixed(wh: Int, wt: Seq[Int], lh: Int, lt: Seq[Int], buf: ArrayBuffer[(Int, Int)]): ArrayBuffer[(Int, Int)] = {
      buf += wh -> lh
      if (wt.isEmpty && lt.isEmpty) {
        buf
      } else if (wt.isEmpty) {
        recMergeFixed(wh, wt, lt.head, lt.tail, buf)
      } else if (lt.isEmpty) {
        recMergeFixed(wt.head, wt.tail, lh, lt, buf)
      } else {
        if (wh == lh) {
          recMergeFixed(wt.head, wt.tail, lt.head, lt.tail, buf)
        } else if (wh > lh) {
          recMergeFixed(wt.head, wt.tail, lh, lt, buf)
        } else {
          recMergeFixed(wh, wt, lt.head, lt.tail, buf)
        }
      }
    }

    recMergeFixed(wbh.head, wbh.tail, lbh.head, lbh.tail, ArrayBuffer.empty).toSeq
  }

  private implicit val buffersReleaseable: Releasable[collection.IndexedSeq[ByteBuffer]] = _.foreach(Util.releaseTemporaryDirectBuffer)

  def apply(
      rdb: RDB,
      settings: BlockchainSettings,
      dbSettings: DBSettings,
      isLightMode: Boolean,
      bfBlockInsertions: Int = 10000,
      forceCleanupExecutorService: Option[ExecutorService] = None
  ): RocksDBWriter = new RocksDBWriter(
    rdb,
    settings,
    dbSettings,
    isLightMode,
    bfBlockInsertions,
    dbSettings.cleanupInterval match {
      case None => MoreExecutors.newDirectExecutorService() // We don't care if disabled
      case Some(_) =>
        forceCleanupExecutorService.getOrElse {
          new ThreadPoolExecutor(
            1,
            1,
            0,
            TimeUnit.SECONDS,
            new LinkedBlockingQueue[Runnable](1), // Only one task at time
            new DefaultThreadFactory("rocksdb-cleanup", true),
            { (_: Runnable, _: ThreadPoolExecutor) => /* Ignore new jobs, because TPE is busy, we will clean the data next time */ }
          )
        }
    }
  )
}

//noinspection UnstableApiUsage
class RocksDBWriter(
    rdb: RDB,
    val settings: BlockchainSettings,
    val dbSettings: DBSettings,
    isLightMode: Boolean,
    bfBlockInsertions: Int = 10000,
    cleanupExecutorService: ExecutorService
) extends Caches
    with AutoCloseable {
  import rdb.db as writableDB

  private[this] val log = LoggerFacade(LoggerFactory.getLogger(classOf[RocksDBWriter]))

  private[this] var disabledAliases = writableDB.get(Keys.disabledAliases)

  import RocksDBWriter.*

  override def close(): Unit = {
    cleanupExecutorService.shutdownNow()
    if (!cleanupExecutorService.awaitTermination(20, TimeUnit.SECONDS))
      log.warn("Not enough time for a cleanup task, try to increase the limit")
  }

  private[database] def readOnly[A](f: ReadOnlyDB => A): A = writableDB.readOnly(f)

  private[this] def readWrite[A](f: RW => A): A = writableDB.readWrite(f)

  override protected def loadMaxAddressId(): Long = writableDB.get(Keys.lastAddressId).getOrElse(0L)

  override protected def loadAddressId(address: Address): Option[AddressId] =
    writableDB.get(Keys.addressId(address))

  override protected def loadAddressIds(addresses: Seq[Address]): Map[Address, Option[AddressId]] = readOnly { ro =>
    addresses.view.zip(ro.multiGetOpt(addresses.view.map(Keys.addressId).toVector, 8)).toMap
  }

  override protected def loadHeight(): Height = RocksDBWriter.loadHeight(writableDB)

  override def safeRollbackHeight: Int = writableDB.get(Keys.safeRollbackHeight)

  override protected def loadBlockMeta(height: Height): Option[PBBlockMeta] =
    writableDB.get(Keys.blockMetaAt(height))

  override protected def loadTxs(height: Height): Seq[Transaction] =
    loadTransactions(height, rdb).map(_._2)

  override protected def loadScript(address: Address): Option[AccountScriptInfo] = readOnly { db =>
    addressId(address).fold(Option.empty[AccountScriptInfo]) { addressId =>
      db.fromHistory(Keys.addressScriptHistory(addressId), Keys.addressScript(addressId)).flatten
    }
  }

  override protected def hasScriptBytes(address: Address): Boolean = readOnly { db =>
    addressId(address).fold(false) { addressId =>
      db.hasInHistory(Keys.addressScriptHistory(addressId), Keys.addressScript(addressId))
    }
  }

  override protected def loadAssetScript(asset: IssuedAsset): Option[AssetScriptInfo] = readOnly { db =>
    db.fromHistory(Keys.assetScriptHistory(asset), Keys.assetScript(asset)).flatten
  }

  override protected def hasAssetScriptBytes(asset: IssuedAsset): Boolean = readOnly { db =>
    db.fromHistory(Keys.assetScriptHistory(asset), Keys.assetScriptPresent(asset)).flatten.nonEmpty
  }

  override def carryFee(refId: Option[ByteStr]): Long = writableDB.get(Keys.carryFee(height))

  override protected def loadAccountData(address: Address, key: String): CurrentData =
    addressId(address).fold(CurrentData.empty(key)) { addressId =>
      writableDB.get(Keys.data(addressId, key))
    }

  override protected def loadEntryHeights(keys: Iterable[(Address, String)], addressIdOf: Address => AddressId): Map[(Address, String), Height] = {
    val keyBufs  = database.getKeyBuffersFromKeys(keys.map { case (addr, k) => Keys.data(addressIdOf(addr), k) }.toVector)
    val valBufs  = database.getValueBuffers(keys.size, 8)
    val valueBuf = new Array[Byte](8)

    val result = rdb.db
      .multiGetByteBuffers(keyBufs.asJava, valBufs.asJava)
      .asScala
      .view
      .zip(keys)
      .map { case (status, k @ (_, key)) =>
        if (status.status.getCode == Status.Code.Ok) {
          status.value.get(valueBuf)
          k -> readCurrentData(key)(valueBuf).height
        } else k -> Height(0)
      }
      .toMap

    keyBufs.foreach(Util.releaseTemporaryDirectBuffer)
    valBufs.foreach(Util.releaseTemporaryDirectBuffer)

    result
  }

  override def hasData(address: Address): Boolean = {
    writableDB.readOnly { ro =>
      ro.get(Keys.addressId(address)).fold(false) { addressId =>
        ro.prefixExists(KeyTags.Data.prefixBytes ++ addressId.toByteArray)
      }
    }
  }

  protected override def loadBalance(req: (Address, Asset)): CurrentBalance =
    addressId(req._1).fold(CurrentBalance.Unavailable) { addressId =>
      req._2 match {
        case asset @ IssuedAsset(_) =>
          writableDB.get(Keys.assetBalance(addressId, asset))
        case Waves =>
          writableDB.get(Keys.wavesBalance(addressId))
      }
    }

  override protected def loadBalances(req: Seq[(Address, Asset)]): Map[(Address, Asset), CurrentBalance] = readOnly { ro =>
    val addrToId = addressIds(req.map(_._1)).collect { case (address, Some(aid)) =>
      address -> aid
    }

    val reqWithKeys = req.flatMap { case (address, asset) =>
      addrToId.get(address).map { aid =>
        (address, asset) -> (asset match {
          case Waves                    => Keys.wavesBalance(aid)
          case issuedAsset: IssuedAsset => Keys.assetBalance(aid, issuedAsset)
        })
      }
    }

    val addressAssetToBalance = reqWithKeys
      .zip(ro.multiGet(reqWithKeys.view.map(_._2).toVector, 16))
      .collect { case (((address, asset), _), Some(balance)) =>
        (address, asset) -> balance
      }
      .toMap

    req.map { key =>
      key -> addressAssetToBalance.getOrElse(key, CurrentBalance.Unavailable)
    }.toMap
  }

  protected override def loadWavesBalances(req: Seq[(Address, Asset)]): Map[(Address, Asset), CurrentBalance] = readOnly { ro =>
    val addrToId = addressIds(req.map(_._1))
    val addrIds  = addrToId.collect { case (_, Some(aid)) => aid }.toSeq

    val idToBalance = addrIds
      .zip(
        ro.multiGet(
          addrIds.view.map { addrId =>
            Keys.wavesBalance(addrId)
          }.toVector,
          16
        )
      )
      .toMap

    req.map { case (address, asset) =>
      (address, asset) -> addrToId.get(address).flatMap(_.flatMap(idToBalance.get)).flatten.getOrElse(CurrentBalance.Unavailable)
    }.toMap
  }

  private def loadLeaseBalance(db: ReadOnlyDB, addressId: AddressId): CurrentLeaseBalance =
    db.get(Keys.leaseBalance(addressId))

  override protected def loadLeaseBalance(address: Address): CurrentLeaseBalance = readOnly { db =>
    addressId(address).fold(CurrentLeaseBalance.Unavailable)(loadLeaseBalance(db, _))
  }

  override protected def loadLeaseBalances(addresses: Seq[Address]): Map[Address, CurrentLeaseBalance] = readOnly { ro =>
    val addrToId = addressIds(addresses)
    val addrIds  = addrToId.collect { case (_, Some(aid)) => aid }.toSeq

    val idToBalance = addrIds
      .zip(
        ro.multiGet(
          addrIds.view.map { addrId =>
            Keys.leaseBalance(addrId)
          }.toVector,
          24
        )
      )
      .toMap

    addresses.map { address =>
      address -> addrToId.get(address).flatMap(_.flatMap(idToBalance.get)).flatten.getOrElse(CurrentLeaseBalance.Unavailable)
    }.toMap
  }

  override protected def loadAssetDescription(asset: IssuedAsset): Option[AssetDescription] =
    writableDB.withResource(r => database.loadAssetDescription(r, asset))

  override protected def loadVolumeAndFee(orderId: ByteStr): CurrentVolumeAndFee = writableDB.get(Keys.filledVolumeAndFee(orderId))

  override protected def loadVolumesAndFees(orders: Seq[ByteStr]): Map[ByteStr, CurrentVolumeAndFee] = readOnly { ro =>
    orders.view
      .zip(ro.multiGet(orders.view.map(Keys.filledVolumeAndFee).toVector, 24))
      .map { case (id, v) => id -> v.getOrElse(CurrentVolumeAndFee.Unavailable) }
      .toMap
  }

  override protected def loadApprovedFeatures(): Map[Short, Int] =
    writableDB.get(Keys.approvedFeatures)

  override protected def loadActivatedFeatures(): Map[Short, Int] = {
    val stateFeatures = writableDB.get(Keys.activatedFeatures)
    stateFeatures ++ settings.functionalitySettings.preActivatedFeatures
  }

  override def wavesAmount(height: Int): BigInt =
    if (this.isFeatureActivated(BlockchainFeatures.BlockReward, height))
      loadBlockMeta(Height(height)).fold(settings.genesisSettings.initialBalance)(_.totalWavesAmount)
    else settings.genesisSettings.initialBalance

  override def blockReward(height: Int): Option[Long] =
    if (this.isFeatureActivated(BlockchainFeatures.ConsensusImprovements, height) && height == 1) None
    else if (this.isFeatureActivated(BlockchainFeatures.BlockReward, height)) loadBlockMeta(Height(height)).map(_.reward)
    else None

  private def updateHistory(rw: RW, key: Key[Seq[Int]], threshold: Int, kf: Int => Key[?]): Seq[Array[Byte]] =
    updateHistory(rw, rw.get(key), key, threshold, kf)

  private def updateHistory(rw: RW, history: Seq[Int], key: Key[Seq[Int]], threshold: Int, kf: Int => Key[?]): Seq[Array[Byte]] = {
    val (c1, c2) = history.partition(_ >= threshold)
    rw.put(key, (height +: c1) ++ c2.headOption)
    c2.drop(1).map(kf(_).keyBytes)
  }

  private def appendBalances(
      balances: Map[(AddressId, Asset), (CurrentBalance, BalanceNode)],
      assetStatics: Map[IssuedAsset, (AssetStaticInfo, Int)],
      rw: RW
  ): Unit = {
    var changedWavesBalances = List.empty[AddressId]
    val changedAssetBalances = MultimapBuilder.hashKeys().hashSetValues().build[IssuedAsset, java.lang.Long]()
    val updatedNftLists      = MultimapBuilder.hashKeys().linkedHashSetValues().build[java.lang.Long, IssuedAsset]()

    for (((addressId, asset), (currentBalance, balanceNode)) <- balances) {
      asset match {
        case Waves =>
          changedWavesBalances = addressId :: changedWavesBalances
          rw.put(Keys.wavesBalance(addressId), currentBalance)
          rw.put(Keys.wavesBalanceAt(addressId, currentBalance.height), balanceNode)
        case a: IssuedAsset =>
          changedAssetBalances.put(a, addressId.toLong)
          rw.put(Keys.assetBalance(addressId, a), currentBalance)
          rw.put(Keys.assetBalanceAt(addressId, a, currentBalance.height), balanceNode)

          val isNFT = currentBalance.balance > 0 && assetStatics
            .get(a)
            .map(_._1.nft)
            .orElse(assetDescription(a).map(_.nft))
            .getOrElse(false)
          if (currentBalance.prevHeight == Height(0) && isNFT) updatedNftLists.put(addressId.toLong, a)
      }
    }

    for ((addressId, nftIds) <- updatedNftLists.asMap().asScala) {
      val kCount           = Keys.nftCount(AddressId(addressId.toLong))
      val previousNftCount = rw.get(kCount)
      rw.put(kCount, previousNftCount + nftIds.size())
      for ((id, idx) <- nftIds.asScala.zipWithIndex) {
        rw.put(Keys.nftAt(AddressId(addressId.toLong), previousNftCount + idx, id), Some(()))
      }
    }

    rw.put(Keys.changedWavesBalances(height), changedWavesBalances)
    changedAssetBalances.asMap().forEach { (asset, addresses) =>
      rw.put(Keys.changedBalances(height, asset), addresses.asScala.map(id => AddressId(id.toLong)).toSeq)
    }
  }

  private def appendData(newAddresses: Map[Address, AddressId], data: Map[(Address, String), (CurrentData, DataNode)], rw: RW): Unit = {
    val changedKeys = MultimapBuilder.hashKeys().hashSetValues().build[AddressId, String]()

    for (((address, key), (currentData, dataNode)) <- data) {
      val addressId = addressIdWithFallback(address, newAddresses)
      changedKeys.put(addressId, key)

      val kdh = Keys.data(addressId, key)
      rw.put(kdh, currentData)
      rw.put(Keys.dataAt(addressId, key)(height), dataNode)
    }

    changedKeys.asMap().forEach { (addressId, keys) =>
      rw.put(Keys.changedDataKeys(height, addressId), keys.asScala.toSeq)
    }
  }

  // todo: instead of fixed-size block batches, store fixed-time batches
  private val BlockStep  = 200
  private def mkFilter() = BloomFilter.create[Array[Byte]](Funnels.byteArrayFunnel(), BlockStep * bfBlockInsertions, 0.01f)
  private def initFilters(): (BloomFilter[Array[Byte]], BloomFilter[Array[Byte]]) = {
    def loadFilter(heights: Seq[Int]): BloomFilter[Array[Byte]] = {
      val filter = mkFilter()
      heights.filter(_ > 0).foreach { h =>
        loadTransactions(Height(h), rdb).foreach { case (_, tx) => filter.put(tx.id().arr) }
      }
      filter
    }

    val lastFilterStart = (height / BlockStep) * BlockStep + 1
    val prevFilterStart = lastFilterStart - BlockStep
    val (bf0Heights, bf1Heights) = if ((height / BlockStep) % 2 == 0) {
      (lastFilterStart to height, prevFilterStart until lastFilterStart)
    } else {
      (prevFilterStart until lastFilterStart, lastFilterStart to height)
    }
    (loadFilter(bf0Heights), loadFilter(bf1Heights))
  }

  private var (bf0, bf1) = initFilters()

  override def containsTransaction(tx: Transaction): Boolean =
    (bf0.mightContain(tx.id().arr) || bf1.mightContain(tx.id().arr)) && {
      writableDB.get(Keys.transactionMetaById(TransactionId(tx.id()), rdb.txMetaHandle)).isDefined
    }

  override protected def doAppend(
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
  ): Unit = {
    log.trace(s"Persisting block ${blockMeta.id} at height $height")
    readWrite { rw =>
      val expiredKeys = new ArrayBuffer[Array[Byte]]

      rw.put(Keys.height, Height(height))

      val previousSafeRollbackHeight = rw.get(Keys.safeRollbackHeight)
      val newSafeRollbackHeight      = height - dbSettings.maxRollbackDepth

      if (previousSafeRollbackHeight < newSafeRollbackHeight) {
        rw.put(Keys.safeRollbackHeight, newSafeRollbackHeight)
        dbSettings.cleanupInterval.foreach { cleanupInterval =>
          runCleanupTask(newSafeRollbackHeight - 1, cleanupInterval) // -1 because we haven't appended this block
        }
      }

      rw.put(Keys.blockMetaAt(Height(height)), Some(blockMeta))
      rw.put(Keys.heightOf(blockMeta.id), Some(height))
      blockHeightCache.put(blockMeta.id, Some(height))

      blockMeta.header.flatMap(_.challengedHeader.map(_.generator.toAddress())) match {
        case Some(addr) =>
          val key          = Keys.maliciousMinerBanHeights(addr.bytes)
          val savedHeights = rw.get(key)
          rw.put(key, height +: savedHeights)
        case _ => ()
      }

      val lastAddressId = loadMaxAddressId() + newAddresses.size
      rw.put(Keys.lastAddressId, Some(lastAddressId))

      for ((address, id) <- newAddresses) {
        val kaid = Keys.addressId(address)
        rw.put(kaid, Some(id))
        rw.put(Keys.idToAddress(id), address)
      }

      val threshold = newSafeRollbackHeight

      appendBalances(balances, snapshot.assetStatics, rw)
      appendData(newAddresses, data, rw)

      val changedAddresses = (addressTransactions.asScala.keys ++ balances.keys.map(_._1)).toSet
      rw.put(Keys.changedAddresses(height), changedAddresses.toSeq)

      // leases
      for ((addressId, (currentLeaseBalance, leaseBalanceNode)) <- leaseBalances) {
        rw.put(Keys.leaseBalance(addressId), currentLeaseBalance)
        rw.put(Keys.leaseBalanceAt(addressId, currentLeaseBalance.height), leaseBalanceNode)
      }

      for ((orderId, (currentVolumeAndFee, volumeAndFeeNode)) <- filledQuantity) {
        rw.put(Keys.filledVolumeAndFee(orderId), currentVolumeAndFee)
        rw.put(Keys.filledVolumeAndFeeAt(orderId, currentVolumeAndFee.height), volumeAndFeeNode)
      }

      for ((asset, (assetStatic, assetNum)) <- snapshot.assetStatics) {
        val pbAssetStatic = StaticAssetInfo(
          assetStatic.source.toByteString,
          assetStatic.issuer.toByteString,
          assetStatic.decimals,
          assetStatic.nft,
          assetNum,
          height,
          asset.id.toByteString
        )
        rw.put(Keys.assetStaticInfo(asset), Some(pbAssetStatic))
      }

      val updatedAssetSet = snapshot.assetVolumes.keySet ++ snapshot.assetNamesAndDescriptions.keySet
      for (asset <- updatedAssetSet) {
        lazy val dbInfo = rw.fromHistory(Keys.assetDetailsHistory(asset), Keys.assetDetails(asset))
        val volume =
          snapshot.assetVolumes
            .get(asset)
            .map(v => AssetVolumeInfo(v.isReissuable, BigInt(v.volume.toByteArray)))
            .orElse(dbInfo.map(_._2))
        val nameAndDescription =
          snapshot.assetNamesAndDescriptions
            .get(asset)
            .map(nd => AssetInfo(nd.name, nd.description, nd.lastUpdatedAt))
            .orElse(dbInfo.map(_._1))
        (nameAndDescription, volume).bisequence
          .foreach(rw.put(Keys.assetDetails(asset)(height), _))
      }

      for (asset <- snapshot.assetStatics.keySet ++ updatedAssetSet) {
        expiredKeys ++= updateHistory(rw, Keys.assetDetailsHistory(asset), threshold, Keys.assetDetails(asset))
      }

      for ((id, li) <- snapshot.newLeases) {
        rw.put(Keys.leaseDetails(id)(height), Some(LeaseDetails(li, snapshot.cancelledLeases.getOrElse(id, LeaseDetails.Status.Active))))
        expiredKeys ++= updateHistory(rw, Keys.leaseDetailsHistory(id), threshold, Keys.leaseDetails(id))
      }

      for ((id, status) <- snapshot.cancelledLeases if !snapshot.newLeases.contains(id)) {
        leaseDetails(id).foreach { d =>
          rw.put(Keys.leaseDetails(id)(height), Some(d.copy(status = status)))
        }

        expiredKeys ++= updateHistory(rw, Keys.leaseDetailsHistory(id), threshold, Keys.leaseDetails(id))
      }

      for ((addressId, script) <- accountScripts) {
        expiredKeys ++= updateHistory(rw, Keys.addressScriptHistory(addressId), threshold, Keys.addressScript(addressId))
        if (script.isDefined) rw.put(Keys.addressScript(addressId)(height), script)
      }

      for ((asset, script) <- snapshot.assetScripts) {
        expiredKeys ++= updateHistory(rw, Keys.assetScriptHistory(asset), threshold, Keys.assetScript(asset))
        rw.put(Keys.assetScript(asset)(height), Some(script))
      }

      if (height % BlockStep == 1) {
        if ((height / BlockStep) % 2 == 0) {
          bf0 = mkFilter()
        } else {
          bf1 = mkFilter()
        }
      }
      val targetBf = if ((height / BlockStep) % 2 == 0) bf0 else bf1

      val transactionsWithSize =
        snapshot.transactions.zipWithIndex.map { case ((id, txInfo), i) =>
          val tx   = txInfo.transaction
          val num  = TxNum(i.toShort)
          val meta = TxMeta(Height @@ blockMeta.height, txInfo.status, txInfo.spentComplexity)
          val txId = TransactionId(id)

          val size = rw.put(Keys.transactionAt(Height(height), num, rdb.txHandle), Some((meta, tx)))
          rw.put(
            Keys.transactionStateSnapshotAt(Height(height), num, rdb.txSnapshotHandle),
            Some(PBSnapshots.toProtobuf(txInfo.snapshot, txInfo.status))
          )
          rw.put(Keys.transactionMetaById(txId, rdb.txMetaHandle), Some(TransactionMeta(height, num, tx.tpe.id, meta.status.protobuf, 0, size)))
          targetBf.put(id.arr)

          txId -> (num, tx, size)
        }.toMap

      if (dbSettings.storeTransactionsByAddress) {
        val addressTxs = addressTransactions.asScala.toSeq.map { case (aid, txIds) =>
          (aid, txIds, Keys.addressTransactionSeqNr(aid))
        }
        rw.multiGetInts(addressTxs.view.map(_._3).toVector)
          .zip(addressTxs)
          .foreach { case (prevSeqNr, (addressId, txIds, txSeqNrKey)) =>
            val nextSeqNr = prevSeqNr.getOrElse(0) + 1
            val txTypeNumSeq = txIds.asScala.map { txId =>
              val (num, tx, size) = transactionsWithSize(txId)
              (tx.tpe.id.toByte, num, size)
            }.toSeq
            rw.put(Keys.addressTransactionHN(addressId, nextSeqNr), Some((Height(height), txTypeNumSeq.sortBy(-_._2))))
            rw.put(txSeqNrKey, nextSeqNr)
          }
      }

      if (dbSettings.storeLeaseStatesByAddress) {
        val addressIdWithLeaseIds =
          for {
            (leaseId, details) <- snapshot.newLeases.toSeq if !snapshot.cancelledLeases.contains(leaseId)
            address            <- Seq(details.recipientAddress, details.sender.toAddress)
            addressId = this.addressIdWithFallback(address, newAddresses)
          } yield (addressId, leaseId)
        val leaseIdsByAddressId = addressIdWithLeaseIds.groupMap { case (addressId, _) => (addressId, Keys.addressLeaseSeqNr(addressId)) }(_._2).toSeq

        rw.multiGetInts(leaseIdsByAddressId.view.map(_._1._2).toVector)
          .zip(leaseIdsByAddressId)
          .foreach { case (prevSeqNr, ((addressId, leaseSeqKey), leaseIds)) =>
            val nextSeqNr = prevSeqNr.getOrElse(0) + 1
            rw.put(Keys.addressLeaseSeq(addressId, nextSeqNr), Some(leaseIds))
            rw.put(leaseSeqKey, nextSeqNr)
          }
      }

      for ((alias, address) <- snapshot.aliases) {
        val key   = Keys.addressIdOfAlias(alias)
        val value = addressIdWithFallback(address, newAddresses)
        rw.put(key, Some(value))
      }

      for ((assetId, sponsorship) <- snapshot.sponsorships) {
        rw.put(Keys.sponsorship(assetId)(height), sponsorship)
        expiredKeys ++= updateHistory(rw, Keys.sponsorshipHistory(assetId), threshold, Keys.sponsorship(assetId))
      }

      val activationWindowSize = settings.functionalitySettings.activationWindowSize(height)
      if (height % activationWindowSize == 0) {
        val minVotes = settings.functionalitySettings.blocksForFeatureActivation(height)
        val newlyApprovedFeatures = featureVotes(height)
          .filterNot { case (featureId, _) => settings.functionalitySettings.preActivatedFeatures.contains(featureId) }
          .collect {
            case (featureId, voteCount) if voteCount + (if (blockMeta.getHeader.featureVotes.contains(featureId.toInt)) 1 else 0) >= minVotes =>
              featureId -> height
          }

        if (newlyApprovedFeatures.nonEmpty) {
          approvedFeaturesCache = newlyApprovedFeatures ++ approvedFeaturesCache
          rw.put(Keys.approvedFeatures, approvedFeaturesCache)

          val featuresToSave = (newlyApprovedFeatures.view.mapValues(_ + activationWindowSize) ++ activatedFeaturesCache).toMap

          activatedFeaturesCache = featuresToSave ++ settings.functionalitySettings.preActivatedFeatures
          rw.put(Keys.activatedFeatures, featuresToSave)
        }
      }

      rw.put(Keys.issuedAssets(height), snapshot.assetStatics.keySet.toSeq)
      rw.put(Keys.updatedAssets(height), updatedAssetSet.toSeq)
      rw.put(Keys.sponsorshipAssets(height), snapshot.sponsorships.keySet.toSeq)

      rw.put(Keys.carryFee(height), carry)
      expiredKeys += Keys.carryFee(threshold - 1).keyBytes

      rw.put(Keys.blockStateHash(height), computedBlockStateHash)
      expiredKeys += Keys.blockStateHash(threshold - 1).keyBytes

      if (dbSettings.storeInvokeScriptResults) snapshot.scriptResults.foreach { case (txId, result) =>
        val (txHeight, txNum) = transactionsWithSize
          .get(TransactionId @@ txId)
          .map { case (txNum, _, _) => (height, txNum) }
          .orElse(rw.get(Keys.transactionMetaById(TransactionId @@ txId, rdb.txMetaHandle)).map { tm =>
            (tm.height, TxNum(tm.num.toShort))
          })
          .getOrElse(throw new IllegalArgumentException(s"Couldn't find transaction height and num: $txId"))

        try rw.put(Keys.invokeScriptResult(txHeight, txNum), Some(result))
        catch {
          case NonFatal(e) =>
            throw new RuntimeException(s"Error storing invoke script result for $txId: $result", e)
        }
      }

      for ((txId, pbMeta) <- snapshot.ethereumTransactionMeta) {
        val txNum = transactionsWithSize(TransactionId @@ txId)._1
        val key   = Keys.ethereumTransactionMeta(Height(height), txNum)
        rw.put(key, Some(pbMeta))
      }

      expiredKeys.foreach(rw.delete)

      if (DisableHijackedAliases.height == height) {
        disabledAliases = DisableHijackedAliases(rw)
      }

      if (dbSettings.storeStateHashes) {
        val prevStateHash =
          if (height == 1) ByteStr.empty
          else
            rw.get(Keys.stateHash(height - 1))
              .fold(
                throw new IllegalStateException(
                  s"Couldn't load state hash for ${height - 1}. Please rebuild the state or disable db.store-state-hashes"
                )
              )(_.totalHash)

        val newStateHash = stateHash.createStateHash(prevStateHash)
        rw.put(Keys.stateHash(height), Some(newStateHash))
      }
    }
    log.trace(s"Finished persisting block ${blockMeta.id} at height $height")
  }

  @volatile private var lastCleanupHeight = writableDB.get(Keys.lastCleanupHeight)
  private def runCleanupTask(newLastSafeHeightForDeletion: Int, cleanupInterval: Int): Unit =
    if (lastCleanupHeight + cleanupInterval < newLastSafeHeightForDeletion) {
      cleanupExecutorService.submit(new Runnable {
        override def run(): Unit = {
          val firstDirtyHeight  = Height(lastCleanupHeight + 1)
          val toHeightExclusive = Height(firstDirtyHeight + cleanupInterval)
          val startTs           = System.nanoTime()

          rdb.db.withOptions { (ro, wo) =>
            rdb.db.readWriteWithOptions(ro, wo.setLowPri(true)) { rw =>
              batchCleanupWavesBalances(
                fromInclusive = firstDirtyHeight,
                toExclusive = toHeightExclusive,
                rw = rw
              )

              batchCleanupAssetBalances(
                fromInclusive = firstDirtyHeight,
                toExclusive = toHeightExclusive,
                rw = rw
              )

              batchCleanupAccountData(
                fromInclusive = firstDirtyHeight,
                toExclusive = toHeightExclusive,
                rw = rw
              )

              lastCleanupHeight = Height(toHeightExclusive - 1)
              rw.put(Keys.lastCleanupHeight, lastCleanupHeight)
            }
          }

          log.debug(s"Cleanup in [$firstDirtyHeight; $toHeightExclusive) took ${(System.nanoTime() - startTs) / 1_000_000}ms")
        }
      })
    }

  private def batchCleanupWavesBalances(fromInclusive: Height, toExclusive: Height, rw: RW): Unit = {
    val lastUpdateAt = mutable.LongMap.empty[Height]

    val updateAt     = new ArrayBuffer[(AddressId, Height)]() // AddressId -> First height of update in this range
    val updateAtKeys = new ArrayBuffer[Key[BalanceNode]]()

    val changedKeyPrefix = KeyTags.ChangedWavesBalances.prefixBytes
    val changedFromKey   = Keys.changedWavesBalances(fromInclusive) // Doesn't matter, we need this only to parse
    rw.iterateOverWithSeek(changedKeyPrefix, changedFromKey.keyBytes) { e =>
      val currHeight = Height(Ints.fromByteArray(e.getKey.drop(changedKeyPrefix.length)))
      val continue   = currHeight < toExclusive
      if (continue)
        changedFromKey.parse(e.getValue).foreach { addressId =>
          lastUpdateAt.updateWith(addressId) { orig =>
            if (orig.isEmpty) {
              updateAt.addOne(addressId -> currHeight)
              updateAtKeys.addOne(Keys.wavesBalanceAt(addressId, currHeight))
            }
            Some(currHeight)
          }
        }
      continue
    }

    rw.multiGet(updateAtKeys, BalanceNode.SizeInBytes)
      .view
      .zip(updateAt)
      .foreach { case (prevBalanceNode, (addressId, firstHeight)) =>
        // We have changes on: previous period = 1000, 1200, 1900, current period = 2000, 2500.
        // Removed on a previous period: 1100, 1200. We need to remove on a current period: 1900, 2000.
        // We doesn't know about 1900, so we should delete all keys from 1.
        // But there is an issue in RocksDB: https://github.com/facebook/rocksdb/issues/11407 that leads to stopped writes.
        // So we need to issue non-overlapping delete ranges and we have to read changes on 2000 to know 1900.
        // Also note: memtable_max_range_deletions doesn't have any effect.
        // TODO Use deleteRange(1, height) after RocksDB's team solves the overlapping deleteRange issue.
        val firstDeleteHeight = prevBalanceNode.fold(firstHeight) { x =>
          if (x.prevHeight == 0) firstHeight // There is no previous record
          else x.prevHeight
        }

        val lastDeleteHeight = lastUpdateAt(addressId)
        if (firstDeleteHeight != lastDeleteHeight)
          rw.deleteRange(
            Keys.wavesBalanceAt(addressId, firstDeleteHeight),
            Keys.wavesBalanceAt(addressId, lastDeleteHeight) // Deletes exclusively
          )
      }

    rw.deleteRange(Keys.changedWavesBalances(fromInclusive), Keys.changedWavesBalances(toExclusive))
  }

  private def batchCleanupAssetBalances(fromInclusive: Height, toExclusive: Height, rw: RW): Unit = {
    val lastUpdateAt = mutable.AnyRefMap.empty[(AddressId, IssuedAsset), Height]

    val updateAt     = new ArrayBuffer[(AddressId, IssuedAsset, Height)]() // First height of update in this range
    val updateAtKeys = new ArrayBuffer[Key[BalanceNode]]()

    val changedKeyPrefix = KeyTags.ChangedAssetBalances.prefixBytes
    val changedKey       = Keys.changedBalances(Int.MaxValue, IssuedAsset(ByteStr.empty))
    rw.iterateOverWithSeek(changedKeyPrefix, Keys.changedBalancesAtPrefix(fromInclusive)) { e =>
      val currHeight = Height(Ints.fromByteArray(e.getKey.drop(changedKeyPrefix.length)))
      val continue   = currHeight < toExclusive
      if (continue) {
        val asset = IssuedAsset(ByteStr(e.getKey.takeRight(AssetIdLength)))
        changedKey.parse(e.getValue).foreach { addressId =>
          lastUpdateAt.updateWith((addressId, asset)) { orig =>
            if (orig.isEmpty) {
              updateAt.addOne((addressId, asset, currHeight))
              updateAtKeys.addOne(Keys.assetBalanceAt(addressId, asset, currHeight))
            }
            Some(currHeight)
          }
        }
      }
      continue
    }

    rw.multiGet(updateAtKeys, BalanceNode.SizeInBytes)
      .view
      .zip(updateAt)
      .foreach { case (prevBalanceNode, (addressId, asset, firstHeight)) =>
        val firstDeleteHeight = prevBalanceNode.fold(firstHeight) { x =>
          if (x.prevHeight == 0) firstHeight
          else x.prevHeight
        }

        val lastDeleteHeight = lastUpdateAt((addressId, asset))
        if (firstDeleteHeight != lastDeleteHeight)
          rw.deleteRange(
            Keys.assetBalanceAt(addressId, asset, firstDeleteHeight),
            Keys.assetBalanceAt(addressId, asset, lastDeleteHeight)
          )
      }

    rw.deleteRange(Keys.changedBalancesAtPrefix(fromInclusive), Keys.changedBalancesAtPrefix(toExclusive))
  }

  private def batchCleanupAccountData(fromInclusive: Height, toExclusive: Height, rw: RW): Unit = {
    val changedDataAddresses = mutable.Set.empty[AddressId]
    val lastUpdateAt         = mutable.AnyRefMap.empty[(AddressId, String), Height]

    val updateAt     = new ArrayBuffer[(AddressId, String, Height)]() // First height of update in this range
    val updateAtKeys = new ArrayBuffer[Key[DataNode]]()

    val changedAddressesPrefix  = KeyTags.ChangedAddresses.prefixBytes
    val changedAddressesFromKey = Keys.changedAddresses(fromInclusive)
    rw.iterateOverWithSeek(changedAddressesPrefix, changedAddressesFromKey.keyBytes) { e =>
      val currHeight = Height(Ints.fromByteArray(e.getKey.drop(changedAddressesPrefix.length)))
      val continue   = currHeight < toExclusive
      if (continue)
        changedAddressesFromKey.parse(e.getValue).foreach { addressId =>
          val changedDataKeys = rw.get(Keys.changedDataKeys(currHeight, addressId))
          if (changedDataKeys.nonEmpty) {
            changedDataAddresses.addOne(addressId)
            changedDataKeys.foreach { accountDataKey =>
              lastUpdateAt.updateWith((addressId, accountDataKey)) { orig =>
                if (orig.isEmpty) {
                  updateAt.addOne((addressId, accountDataKey, currHeight))
                  updateAtKeys.addOne(Keys.dataAt(addressId, accountDataKey)(currHeight))
                }
                Some(currHeight)
              }
            }
          }
        }

      continue
    }

    val valueBuff = new Array[Byte](Ints.BYTES) // height of DataNode
    Using.resources(
      database.getKeyBuffersFromKeys(updateAtKeys),
      database.getValueBuffers(updateAtKeys.size, valueBuff.length)
    ) { (keyBuffs, valBuffs) =>
      rdb.db
        .multiGetByteBuffers(keyBuffs.asJava, valBuffs.asJava)
        .asScala
        .view
        .zip(updateAt)
        .foreach { case (status, (addressId, accountDataKey, firstHeight)) =>
          val firstDeleteHeight = if (status.status.getCode == Status.Code.Ok) {
            status.value.get(valueBuff)
            val r = readDataNode(accountDataKey)(valueBuff).prevHeight
            if (r == 0) firstHeight else r
          } else firstHeight

          val lastDeleteHeight = lastUpdateAt((addressId, accountDataKey))
          if (firstDeleteHeight != lastDeleteHeight)
            rw.deleteRange(
              Keys.dataAt(addressId, accountDataKey)(firstDeleteHeight),
              Keys.dataAt(addressId, accountDataKey)(lastDeleteHeight)
            )
        }
    }

    rw.deleteRange(Keys.changedAddresses(fromInclusive), Keys.changedAddresses(toExclusive))
    changedDataAddresses.foreach { addressId =>
      rw.deleteRange(Keys.changedDataKeys(fromInclusive, addressId), Keys.changedDataKeys(toExclusive, addressId))
    }
  }

  override protected def doRollback(targetHeight: Int): DiscardedBlocks = {
    val targetBlockId = readOnly(_.get(Keys.blockMetaAt(Height @@ targetHeight)))
      .map(_.id)
      .getOrElse(throw new IllegalArgumentException(s"No block at height $targetHeight"))

    log.debug(s"Rolling back to block $targetBlockId at $targetHeight")

    val discardedBlocks: DiscardedBlocks =
      for (currentHeightInt <- height until targetHeight by -1; currentHeight = Height(currentHeightInt)) yield {
        val balancesToInvalidate     = Seq.newBuilder[(Address, Asset)]
        val ordersToInvalidate       = Seq.newBuilder[ByteStr]
        val scriptsToDiscard         = Seq.newBuilder[Address]
        val assetScriptsToDiscard    = Seq.newBuilder[IssuedAsset]
        val accountDataToInvalidate  = Seq.newBuilder[(Address, String)]
        val aliasesToInvalidate      = Seq.newBuilder[Alias]
        val blockHeightsToInvalidate = Seq.newBuilder[ByteStr]

        val discardedBlock = readWrite { rw =>
          rw.put(Keys.height, Height(currentHeight - 1))

          val discardedMeta = rw
            .get(Keys.blockMetaAt(currentHeight))
            .getOrElse(throw new IllegalArgumentException(s"No block at height $currentHeight"))

          log.trace(s"Removing block ${discardedMeta.id} at $currentHeight")

          val changedAddresses = for {
            addressId <- rw.get(Keys.changedAddresses(currentHeight))
          } yield addressId -> rw.get(Keys.idToAddress(addressId))

          rw.iterateOver(KeyTags.ChangedAssetBalances.prefixBytes ++ KeyHelpers.h(currentHeight)) { e =>
            val assetId = IssuedAsset(ByteStr(e.getKey.takeRight(AssetIdLength)))
            for ((addressId, address) <- changedAddresses) {
              balancesToInvalidate += address -> assetId
              rollbackBalanceHistory(rw, Keys.assetBalance(addressId, assetId), Keys.assetBalanceAt(addressId, assetId, _), currentHeight)
            }
          }

          for ((addressId, address) <- changedAddresses) {
            for (k <- rw.get(Keys.changedDataKeys(currentHeight, addressId))) {
              log.trace(s"Discarding $k for $address at $currentHeight")
              accountDataToInvalidate += (address -> k)

              rw.delete(Keys.dataAt(addressId, k)(currentHeight))
              rollbackDataHistory(rw, Keys.data(addressId, k), Keys.dataAt(addressId, k)(_), currentHeight)
            }
            rw.delete(Keys.changedDataKeys(currentHeight, addressId))

            balancesToInvalidate += (address -> Waves)
            rollbackBalanceHistory(rw, Keys.wavesBalance(addressId), Keys.wavesBalanceAt(addressId, _), currentHeight)

            rollbackLeaseBalance(rw, addressId, currentHeight)

            balanceAtHeightCache.invalidate((currentHeight, addressId))
            leaseBalanceAtHeightCache.invalidate((currentHeight, addressId))
            discardLeaseBalance(address)

            if (dbSettings.storeTransactionsByAddress) {
              val kTxSeqNr = Keys.addressTransactionSeqNr(addressId)
              val txSeqNr  = rw.get(kTxSeqNr)
              val kTxHNSeq = Keys.addressTransactionHN(addressId, txSeqNr)

              rw.get(kTxHNSeq).collect { case (`currentHeight`, _) =>
                rw.delete(kTxHNSeq)
                rw.put(kTxSeqNr, (txSeqNr - 1).max(0))
              }
            }

            if (dbSettings.storeLeaseStatesByAddress) {
              val leaseSeqNrKey = Keys.addressLeaseSeqNr(addressId)
              val leaseSeqNr    = rw.get(leaseSeqNrKey)
              val leaseSeqKey   = Keys.addressLeaseSeq(addressId, leaseSeqNr)
              rw.get(leaseSeqKey)
                .flatMap(_.headOption)
                .flatMap(leaseDetails)
                .filter(_.height == currentHeight)
                .foreach { _ =>
                  rw.delete(leaseSeqKey)
                  rw.put(leaseSeqNrKey, (leaseSeqNr - 1).max(0))
                }
            }
          }

          writableDB
            .withResource(loadLeaseIds(_, currentHeight, currentHeight, includeCancelled = true))
            .foreach(rollbackLeaseStatus(rw, _, currentHeight))

          rollbackAssetsInfo(rw, currentHeight)

          val blockTxs = loadTransactions(currentHeight, rdb)
          blockTxs.view.zipWithIndex.foreach { case ((_, tx), idx) =>
            val num = TxNum(idx.toShort)
            (tx: @unchecked) match {
              case _: GenesisTransaction                                                       => // genesis transaction can not be rolled back
              case _: PaymentTransaction | _: TransferTransaction | _: MassTransferTransaction =>
              // balances already restored

              case _: IssueTransaction | _: UpdateAssetInfoTransaction | _: ReissueTransaction | _: BurnTransaction | _: SponsorFeeTransaction =>
              // asset info already restored

              case _: LeaseTransaction | _: LeaseCancelTransaction =>
              // leases already restored

              case tx: SetScriptTransaction =>
                val address = tx.sender.toAddress
                scriptsToDiscard += address
                for (addressId <- addressId(address)) {
                  rw.delete(Keys.addressScript(addressId)(currentHeight))
                  rw.filterHistory(Keys.addressScriptHistory(addressId), currentHeight)
                }

              case tx: SetAssetScriptTransaction =>
                val asset = tx.asset
                assetScriptsToDiscard += asset
                rw.delete(Keys.assetScript(asset)(currentHeight))
                rw.filterHistory(Keys.assetScriptHistory(asset), currentHeight)

              case _: DataTransaction => // see changed data keys removal
              case _: InvokeScriptTransaction | _: InvokeExpressionTransaction =>
                rw.delete(Keys.invokeScriptResult(currentHeight, num))

              case tx: CreateAliasTransaction =>
                rw.delete(Keys.addressIdOfAlias(tx.alias))
                aliasesToInvalidate += tx.alias
              case tx: ExchangeTransaction =>
                ordersToInvalidate += rollbackOrderFill(rw, tx.buyOrder.id(), currentHeight)
                ordersToInvalidate += rollbackOrderFill(rw, tx.sellOrder.id(), currentHeight)
              case _: EthereumTransaction =>
                rw.delete(Keys.ethereumTransactionMeta(currentHeight, num))
            }

            if (tx.tpe != TransactionType.Genesis) {
              rw.delete(Keys.transactionAt(currentHeight, num, rdb.txHandle))
              rw.delete(Keys.transactionMetaById(TransactionId(tx.id()), rdb.txMetaHandle))
            }
            rw.delete(Keys.transactionStateSnapshotAt(currentHeight, num, rdb.txSnapshotHandle))
          }

          discardedMeta.header.flatMap(_.challengedHeader.map(_.generator.toAddress())) match {
            case Some(addr) =>
              val key        = Keys.maliciousMinerBanHeights(addr.bytes)
              val banHeights = rw.get(key)
              if (banHeights.size > 1) rw.put(key, banHeights.tail) else rw.delete(key)
            case _ => ()
          }

          rw.delete(Keys.blockMetaAt(currentHeight))
          rw.delete(Keys.changedAddresses(currentHeight))
          rw.delete(Keys.changedWavesBalances(currentHeight))
          rw.delete(Keys.heightOf(discardedMeta.id))
          blockHeightsToInvalidate.addOne(discardedMeta.id)
          rw.delete(Keys.carryFee(currentHeight))
          rw.delete(Keys.blockStateHash(currentHeight))
          rw.delete(Keys.stateHash(currentHeight))

          if (DisableHijackedAliases.height == currentHeight) {
            disabledAliases = DisableHijackedAliases.revert(rw)
          }

          val disapprovedFeatures = approvedFeaturesCache.collect { case (id, approvalHeight) if approvalHeight > targetHeight => id }
          if (disapprovedFeatures.nonEmpty) {
            approvedFeaturesCache --= disapprovedFeatures
            rw.put(Keys.approvedFeatures, approvedFeaturesCache)

            activatedFeaturesCache --= disapprovedFeatures // We won't activate them in the future
            rw.put(Keys.activatedFeatures, activatedFeaturesCache)
          }

          val block = createBlock(
            PBBlocks.vanilla(
              discardedMeta.header.getOrElse(throw new IllegalArgumentException(s"Block header is missing at height ${currentHeight.toInt}"))
            ),
            ByteStr(discardedMeta.signature.toByteArray),
            blockTxs.map(_._2)
          ).explicitGet()

          val snapshot = if (isLightMode) {
            Some(BlockSnapshot(block.id(), loadTxStateSnapshotsWithStatus(currentHeight, rdb, block.transactionData)))
          } else None

          (block, Caches.toHitSource(discardedMeta), snapshot)
        }

        balancesToInvalidate.result().foreach(discardBalance)
        ordersToInvalidate.result().foreach(discardVolumeAndFee)
        scriptsToDiscard.result().foreach(discardScript)
        assetScriptsToDiscard.result().foreach(discardAssetScript)
        accountDataToInvalidate.result().foreach(discardAccountData)
        aliasesToInvalidate.result().foreach(discardAlias)
        blockHeightsToInvalidate.result().foreach(discardBlockHeight)
        discardedBlock
      }

    log.debug(s"Rollback to block $targetBlockId at $targetHeight completed")
    discardedBlocks.reverse
  }

  private def rollbackDataHistory(rw: RW, currentDataKey: Key[CurrentData], dataNodeKey: Height => Key[DataNode], currentHeight: Height): Unit = {
    val currentData = rw.get(currentDataKey)
    if (currentData.height == currentHeight) {
      val prevDataNode = rw.get(dataNodeKey(currentData.prevHeight))
      rw.delete(dataNodeKey(currentHeight))
      prevDataNode.entry match {
        case _: EmptyDataEntry => rw.delete(currentDataKey)
        case _                 => rw.put(currentDataKey, CurrentData(prevDataNode.entry, currentData.prevHeight, prevDataNode.prevHeight))
      }
    }
  }

  private def rollbackBalanceHistory(rw: RW, curBalanceKey: Key[CurrentBalance], balanceNodeKey: Height => Key[BalanceNode], height: Height): Unit = {
    val balance = rw.get(curBalanceKey)
    if (balance.height == height) {
      val prevBalanceNode = rw.get(balanceNodeKey(balance.prevHeight))
      rw.delete(balanceNodeKey(height))
      rw.put(curBalanceKey, CurrentBalance(prevBalanceNode.balance, balance.prevHeight, prevBalanceNode.prevHeight))
    }
  }

  private def rollbackAssetsInfo(rw: RW, currentHeight: Int): Unit = {
    val issuedKey      = Keys.issuedAssets(currentHeight)
    val updatedKey     = Keys.updatedAssets(currentHeight)
    val sponsorshipKey = Keys.sponsorshipAssets(currentHeight)

    val issued      = rw.get(issuedKey)
    val updated     = rw.get(updatedKey)
    val sponsorship = rw.get(sponsorshipKey)

    rw.delete(issuedKey)
    rw.delete(updatedKey)
    rw.delete(sponsorshipKey)

    issued.foreach { asset =>
      rw.delete(Keys.assetStaticInfo(asset))
    }

    (issued ++ updated).foreach { asset =>
      rw.delete(Keys.assetDetails(asset)(currentHeight))
      rw.filterHistory(Keys.assetDetailsHistory(asset), currentHeight)
      discardAssetDescription(asset)
    }

    sponsorship.foreach { asset =>
      rw.delete(Keys.sponsorship(asset)(currentHeight))
      rw.filterHistory(Keys.sponsorshipHistory(asset), currentHeight)
      discardAssetDescription(asset)
    }
  }

  private def rollbackOrderFill(rw: RW, orderId: ByteStr, height: Height): ByteStr = {
    val curVfKey = Keys.filledVolumeAndFee(orderId)
    val vf       = rw.get(curVfKey)
    if (vf.height == height) {
      val vfNodeKey  = Keys.filledVolumeAndFeeAt(orderId, _)
      val prevVfNode = rw.get(vfNodeKey(vf.prevHeight))
      rw.delete(vfNodeKey(height))
      rw.put(curVfKey, CurrentVolumeAndFee(prevVfNode.volume, prevVfNode.fee, vf.prevHeight, prevVfNode.prevHeight))
    }
    orderId
  }

  private def rollbackLeaseBalance(rw: RW, addressId: AddressId, height: Height): Unit = {
    val curLbKey = Keys.leaseBalance(addressId)
    val lb       = rw.get(curLbKey)
    if (lb.height == height) {
      val lbNodeKey  = Keys.leaseBalanceAt(addressId, _)
      val prevLbNode = rw.get(lbNodeKey(lb.prevHeight))
      rw.delete(lbNodeKey(height))
      rw.put(curLbKey, CurrentLeaseBalance(prevLbNode.in, prevLbNode.out, lb.prevHeight, prevLbNode.prevHeight))
    }
  }

  private def rollbackLeaseStatus(rw: RW, leaseId: ByteStr, currentHeight: Int): Unit = {
    rw.delete(Keys.leaseDetails(leaseId)(currentHeight))
    rw.filterHistory(Keys.leaseDetailsHistory(leaseId), currentHeight)
  }

  override def transferById(id: ByteStr): Option[(Int, TransferTransactionLike)] = readOnly { db =>
    for {
      tm <- db.get(Keys.transactionMetaById(TransactionId @@ id, rdb.txMetaHandle))
      if tm.`type` == TransferTransaction.typeId || tm.`type` == TransactionType.Ethereum.id
      tx <- db
        .get(Keys.transactionAt(Height(tm.height), TxNum(tm.num.toShort), rdb.txHandle))
        .collect {
          case (tm, t: TransferTransaction) if tm.status == TxMeta.Status.Succeeded => t
          case (_, e @ EthereumTransaction(transfer: Transfer, _, _, _)) if tm.status == PBStatus.SUCCEEDED =>
            val asset = transfer.tokenAddress.fold[Asset](Waves)(resolveERC20Address(_).get)
            e.toTransferLike(TxPositiveAmount.unsafeFrom(transfer.amount), transfer.recipient, asset)
        }
    } yield (height, tx)
  }

  override def transactionInfo(id: ByteStr): Option[(TxMeta, Transaction)] = readOnly(transactionInfo(id, _))

  override def transactionInfos(ids: Seq[ByteStr]): Seq[Option[(TxMeta, Transaction)]] = readOnly { db =>
    val tms = db.multiGetOpt(ids.view.map(id => Keys.transactionMetaById(TransactionId(id), rdb.txMetaHandle)).toVector, 36)
    val (keys, sizes) = tms.view
      .map {
        case Some(tm) => Keys.transactionAt(Height(tm.height), TxNum(tm.num.toShort), rdb.txHandle) -> tm.size
        case None     => Keys.transactionAt(Height(0), TxNum(0.toShort), rdb.txHandle)              -> 0
      }
      .toVector
      .unzip

    db.multiGetOpt(keys, sizes)
  }

  protected def transactionInfo(id: ByteStr, db: ReadOnlyDB): Option[(TxMeta, Transaction)] =
    for {
      tm        <- db.get(Keys.transactionMetaById(TransactionId(id), rdb.txMetaHandle))
      (txm, tx) <- db.get(Keys.transactionAt(Height(tm.height), TxNum(tm.num.toShort), rdb.txHandle))
    } yield (txm, tx)

  override def transactionMeta(id: ByteStr): Option[TxMeta] = {
    writableDB.get(Keys.transactionMetaById(TransactionId(id), rdb.txMetaHandle)).map { tm =>
      TxMeta(Height(tm.height), TxMeta.Status.fromProtobuf(tm.status), tm.spentComplexity)
    }
  }

  def transactionSnapshot(id: ByteStr): Option[TransactionStateSnapshot] = readOnly { db =>
    for {
      meta     <- db.get(Keys.transactionMetaById(TransactionId(id), rdb.txMetaHandle))
      snapshot <- db.get(Keys.transactionStateSnapshotAt(Height(meta.height), TxNum(meta.num.toShort), rdb.txSnapshotHandle))
    } yield snapshot
  }

  override def resolveAlias(alias: Alias): Either[ValidationError, Address] =
    if (disabledAliases.contains(alias)) Left(AliasIsDisabled(alias))
    else aliasCache.get(alias).toRight(AliasDoesNotExist(alias))

  override protected def loadAlias(alias: Alias): Option[Address] = readOnly { db =>
    db.get(Keys.addressIdOfAlias(alias))
      .map(addressId => db.get(Keys.idToAddress(addressId)))
  }

  override protected def loadBlockHeight(blockId: BlockId): Option[Int] = readOnly(_.get(Keys.heightOf(blockId)))

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = readOnly { db =>
    for {
      h       <- db.get(Keys.leaseDetailsHistory(leaseId)).headOption
      details <- db.get(Keys.leaseDetails(leaseId)(h))
    } yield details
  }

  // These two caches are used exclusively for balance snapshots. They are not used for portfolios, because there aren't
  // as many miners, so snapshots will rarely be evicted due to overflows.

  private val balanceAtHeightCache = CacheBuilder
    .newBuilder()
    .maximumSize(100000)
    .recordStats()
    .build[(Int, AddressId), BalanceNode]()

  private val leaseBalanceAtHeightCache = CacheBuilder
    .newBuilder()
    .maximumSize(100000)
    .recordStats()
    .build[(Int, AddressId), LeaseBalanceNode]()

  override def balanceAtHeight(address: Address, height: Int, assetId: Asset = Waves): Option[(Int, Long)] = readOnly { db =>
    @tailrec
    def getBalanceAtHeight(h: Height, key: Height => Key[BalanceNode]): (Int, Long) = {
      val balance = db.get(key(h))
      if (h <= height) {
        h -> balance.balance
      } else {
        getBalanceAtHeight(balance.prevHeight, key)
      }
    }

    db.get(Keys.addressId(address)).map { aid =>
      val (balance, balanceNodeKey) =
        assetId match {
          case Waves                  => (db.get(Keys.wavesBalance(aid)), Keys.wavesBalanceAt(aid, _))
          case asset @ IssuedAsset(_) => (db.get(Keys.assetBalance(aid, asset)), Keys.assetBalanceAt(aid, asset, _))
        }

      if (balance.height > height) {
        getBalanceAtHeight(balance.prevHeight, balanceNodeKey)
      } else {
        balance.height -> balance.balance
      }
    }
  }

  override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] = readOnly { db =>
    addressId(address).fold(Seq(BalanceSnapshot(1, 0, 0, 0))) { addressId =>
      val toHeight = to.flatMap(this.heightOf).getOrElse(this.height)

      val lastBalance      = balancesCache.get((address, Asset.Waves))
      val lastLeaseBalance = leaseBalanceCache.get(address)

      @tailrec
      def collectBalanceHistory(acc: Vector[Int], hh: Int): Seq[Int] =
        if (hh < from || hh <= 0)
          acc :+ hh
        else {
          val bn     = balanceAtHeightCache.get((hh, addressId), () => db.get(Keys.wavesBalanceAt(addressId, Height(hh))))
          val newAcc = if (hh > toHeight) acc else acc :+ hh
          collectBalanceHistory(newAcc, bn.prevHeight)
        }

      @tailrec
      def collectLeaseBalanceHistory(acc: Vector[Int], hh: Int): Seq[Int] =
        if (hh < from || hh <= 0)
          acc :+ hh
        else {
          val lbn    = leaseBalanceAtHeightCache.get((hh, addressId), () => db.get(Keys.leaseBalanceAt(addressId, Height(hh))))
          val newAcc = if (hh > toHeight) acc else acc :+ hh
          collectLeaseBalanceHistory(newAcc, lbn.prevHeight)
        }

      val wbh = slice(collectBalanceHistory(Vector.empty, lastBalance.height), from, toHeight)
      val lbh = slice(collectLeaseBalanceHistory(Vector.empty, lastLeaseBalance.height), from, toHeight)
      for {
        (wh, lh) <- merge(wbh, lbh)
        wb = balanceAtHeightCache.get((wh, addressId), () => db.get(Keys.wavesBalanceAt(addressId, Height(wh))))
        lb = leaseBalanceAtHeightCache.get((lh, addressId), () => db.get(Keys.leaseBalanceAt(addressId, Height(lh))))
      } yield {
        val height = wh.max(lh)
        BalanceSnapshot(height, wb.balance, lb.in, lb.out)
      }
    }
  }

  override def loadHeightOf(blockId: ByteStr): Option[Int] = blockHeightCache.get(blockId)

  override def featureVotes(height: Int): Map[Short, Int] = readOnly { db =>
    settings.functionalitySettings
      .activationWindow(height)
      .flatMap { h =>
        val height = Height(h)
        db.get(Keys.blockMetaAt(height))
          .flatMap(_.header)
          .fold(Seq.empty[Short])(_.featureVotes.map(_.toShort))
      }
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .toMap
  }

  override def blockRewardVotes(height: Int): Seq[Long] = readOnly { db =>
    activatedFeatures.get(BlockchainFeatures.BlockReward.id) match {
      case Some(activatedAt) if activatedAt <= height =>
        val modifyTerm = activatedFeatures.get(BlockchainFeatures.CappedReward.id).exists(_ <= height)
        settings.rewardsSettings
          .votingWindow(activatedAt, height, modifyTerm)
          .flatMap { h =>
            db.get(Keys.blockMetaAt(Height(h)))
              .flatMap(_.header)
              .map(_.rewardVote)
          }
      case _ => Seq()
    }
  }

  def loadStateHash(height: Int): Option[StateHash] = readOnly { db =>
    db.get(Keys.stateHash(height))
  }

  // TODO: maybe add length constraint
  def loadBalanceHistory(address: Address): Seq[(Int, Long)] = writableDB.withResource { dbResource =>
    dbResource.get(Keys.addressId(address)).fold(Seq.empty[(Int, Long)]) { aid =>
      new WavesBalanceIterator(aid, dbResource).asScala.toSeq
    }
  }

  override def effectiveBalanceBanHeights(address: Address): Seq[Int] =
    readOnly(_.get(Keys.maliciousMinerBanHeights(address.bytes)))

  override def resolveERC20Address(address: ERC20Address): Option[IssuedAsset] =
    readOnly(_.get(Keys.assetStaticInfo(address)).map(assetInfo => IssuedAsset(assetInfo.id.toByteStr)))

  override def lastStateHash(refId: Option[ByteStr]): ByteStr = {
    readOnly(_.get(Keys.blockStateHash(height)))
  }
}
