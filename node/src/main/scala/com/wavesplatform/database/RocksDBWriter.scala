package com.wavesplatform.database

import java.util
import cats.data.Ior
import cats.syntax.option.*
import cats.syntax.semigroup.*
import com.google.common.cache.CacheBuilder
import com.google.common.collect.MultimapBuilder
import com.google.common.primitives.Ints
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database
import com.wavesplatform.database.patch.DisableHijackedAliases
import com.wavesplatform.database.protobuf.{EthereumTransactionMeta, TransactionMeta, BlockMeta as PBBlockMeta}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.protobuf.transaction.PBAmounts
import com.wavesplatform.settings.{BlockchainSettings, DBSettings, WavesSettings}
import com.wavesplatform.state.*
import com.wavesplatform.state.reader.LeaseDetails
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
import monix.reactive.Observer
import org.rocksdb.RocksDB
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*
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

  private[database] def closest(v: Seq[Int], h: Int): Option[Int] = {
    v.dropWhile(_ > h).headOption // Should we use binary search?
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

  def apply(db: RocksDB, spendableBalanceChanged: Observer[(Address, Asset)], settings: WavesSettings): RocksDBWriter & AutoCloseable = {
    val expectedHeight = loadHeight(db)
    def load(name: String, key: KeyTags.KeyTag): Option[BloomFilterImpl] = {
      if (settings.dbSettings.useBloomFilter)
        Some(BloomFilter.loadOrPopulate(db, settings.dbSettings.directory, name, expectedHeight, key, 100000000))
      else
        None
    }

    val _orderFilter        = load("orders", KeyTags.FilledVolumeAndFeeHistory)
    val _dataKeyFilter      = load("account-data", KeyTags.DataHistory)
    val _addressesKeyFilter = load("addresses", KeyTags.AddressId)
    new RocksDBWriter(db, spendableBalanceChanged, 200_000_000, settings.blockchainSettings, settings.dbSettings) with AutoCloseable {

      override val dataKeyFilter: BloomFilter = _dataKeyFilter.getOrElse(BloomFilter.AlwaysEmpty)
      override val addressFilter: BloomFilter = _addressesKeyFilter.getOrElse(BloomFilter.AlwaysEmpty)

      override def close(): Unit = {
        log.debug("Shutting down LevelDBWriter")
        val lastHeight = RocksDBWriter.loadHeight(db)
        _orderFilter.foreach(_.save(lastHeight))
        _dataKeyFilter.foreach(_.save(lastHeight))
        _addressesKeyFilter.foreach(_.save(lastHeight))
      }
    }
  }

  def readOnly(db: RocksDB, settings: WavesSettings): RocksDBWriter = {
    val expectedHeight = loadHeight(db)
    def loadFilter(filterName: String) =
      if (settings.dbSettings.useBloomFilter)
        BloomFilter
          .tryLoad(db, filterName, settings.dbSettings.directory, expectedHeight)
          .fold(_ => BloomFilter.AlwaysEmpty, gf => new Wrapper(gf))
      else
        BloomFilter.AlwaysEmpty

    new RocksDBWriter(db, Observer.stopped, 200_000_000, settings.blockchainSettings, settings.dbSettings) {
      override val dataKeyFilter: BloomFilter = loadFilter("account-data")
      override val addressFilter: BloomFilter = loadFilter("addresses")
    }
  }
}

//noinspection UnstableApiUsage
abstract class RocksDBWriter private[database] (
    writableDB: RocksDB,
    spendableBalanceChanged: Observer[(Address, Asset)],
    txFilterSize: Int,
    val settings: BlockchainSettings,
    val dbSettings: DBSettings
) extends Caches(spendableBalanceChanged, txFilterSize) {

  private[this] val log = LoggerFacade(LoggerFactory.getLogger(classOf[RocksDBWriter]))

//  val txdb = new TXDB(new File(dbSettings.directory).getCanonicalPath + "/../transactions")

  def dataKeyFilter: BloomFilter
  def addressFilter: BloomFilter

  private[this] var disabledAliases = writableDB.get(Keys.disabledAliases)

  import RocksDBWriter.*

  private[database] def readOnly[A](f: ReadOnlyDB => A): A = writableDB.readOnly(f)

  private[this] def readWrite[A](f: RW => A): A = writableDB.readWrite(f)

  private def loadWithFilter[A, R](filter: BloomFilter, key: Key[A])(f: (ReadOnlyDB, A) => Option[R]): Option[R] =
    if (filter.mightContain(key.suffix)) readOnly { ro =>
      f(ro, ro.get(key))
    }
    else None

  override protected def loadMaxAddressId(): Long = readOnly(db => db.get(Keys.lastAddressId).getOrElse(0L))

  override protected def loadAddressId(address: Address): Option[AddressId] = loadWithFilter(addressFilter, Keys.addressId(address)) { (_, id) => id }

  override protected def loadAddressIds(addresses: Seq[Address]): Map[Address, Option[AddressId]] = readOnly { ro =>
    addresses.view.zip(ro.multiGetOpt(addresses.map(Keys.addressId), 8)).toMap
  }

  override protected def loadHeight(): Height = RocksDBWriter.loadHeight(writableDB)

  override def safeRollbackHeight: Int = readOnly(_.get(Keys.safeRollbackHeight))

  override protected def loadBlockMeta(height: Height): Option[PBBlockMeta] = readOnly { db =>
    db.get(Keys.blockMetaAt(height))
  }

  override protected def loadTxs(height: Height): Seq[Transaction] = readOnly { db =>
    loadTransactions(height, db).map(_._2)
  }

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

  override def carryFee: Long = readOnly(_.get(Keys.carryFee(height)))

  override protected def loadAccountData(address: Address, key: String): Option[DataEntry[?]] =
    loadWithFilter(dataKeyFilter, Keys.dataMetaHistory(address, key)) { (ro, history) =>
      for {
        aid    <- addressId(address)
        (h, _) <- history.headOption
        e      <- ro.get(Keys.data(aid, key)(h))
      } yield e
    }

  override def hasData(address: Address): Boolean = {
    writableDB.readOnly { ro =>
      ro.get(Keys.addressId(address)).fold(false) { addressId =>
        ro.prefixExists(KeyTags.ChangedDataKeys.prefixBytes ++ addressId.toByteArray)
      }
    }
  }

  protected override def loadBalance(req: (Address, Asset)): CurrentBalance =
    addressId(req._1).fold(CurrentBalance.Unavailable) { addressId =>
      req._2 match {
        case asset @ IssuedAsset(_) =>
          writableDB.readOnly(_.get(Keys.assetBalance(addressId, asset)))
        case Waves =>
          writableDB.readOnly(_.get(Keys.wavesBalance(addressId)))
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
      .zip(ro.multiGet(reqWithKeys.map(_._2), 16))
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
          addrIds.map { addrId =>
            Keys.wavesBalance(addrId)
          },
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
          addrIds.map { addrId =>
            Keys.leaseBalance(addrId)
          },
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

  override protected def loadVolumeAndFee(orderId: ByteStr): CurrentVolumeAndFee = readOnly { ro =>
    ro.get(Keys.filledVolumeAndFee(orderId))
  }

  override protected def loadApprovedFeatures(): Map[Short, Int] = {
    readOnly(_.get(Keys.approvedFeatures))
  }

  override protected def loadActivatedFeatures(): Map[Short, Int] = {
    val stateFeatures = readOnly(_.get(Keys.activatedFeatures))
    stateFeatures ++ settings.functionalitySettings.preActivatedFeatures
  }

  override def wavesAmount(height: Int): BigInt =
    if (this.isFeatureActivated(BlockchainFeatures.BlockReward, height))
      loadBlockMeta(Height(height)).fold(settings.genesisSettings.initialBalance)(_.totalWavesAmount)
    else settings.genesisSettings.initialBalance

  override def blockReward(height: Int): Option[Long] =
    if (this.isFeatureActivated(BlockchainFeatures.BlockReward, height)) loadBlockMeta(Height(height)).map(_.reward)
    else None

  private def updateHistory(rw: RW, key: Key[Seq[Int]], threshold: Int, kf: Int => Key[?]): Seq[Array[Byte]] =
    updateHistory(rw, rw.get(key), key, threshold, kf)

  private def updateHistory(rw: RW, history: Seq[Int], key: Key[Seq[Int]], threshold: Int, kf: Int => Key[?]): Seq[Array[Byte]] = {
    val (c1, c2) = history.partition(_ >= threshold)
    rw.put(key, (height +: c1) ++ c2.headOption)
    c2.drop(1).map(kf(_).keyBytes)
  }

  private def updateMetaHistory(rw: RW, key: Key[Seq[(Int, Int)]], threshold: Int, kf: Int => Key[?], size: Int): Seq[Array[Byte]] =
    updateMetaHistory(rw, rw.get(key), key, threshold, kf, size)

  private def updateMetaHistory(
      rw: RW,
      history: Seq[(Int, Int)],
      key: Key[Seq[(Int, Int)]],
      threshold: Int,
      kf: Int => Key[?],
      size: Int
  ): Seq[Array[Byte]] = {
    val (c1, c2) = history.partition(_._1 >= threshold)
    rw.put(key, ((height, size) +: c1) ++ c2.headOption)
    c2.drop(1).map { case (h, _) => kf(h).keyBytes }
  }

  private def appendBalances(
      balances: Map[(AddressId, Asset), (CurrentBalance, BalanceNode)],
      issuedAssets: Map[IssuedAsset, NewAssetInfo],
      rw: RW
  ): Unit = {
    val changedAssetBalances = MultimapBuilder.hashKeys().hashSetValues().build[IssuedAsset, java.lang.Long]()
    val updatedNftLists      = MultimapBuilder.hashKeys().linkedHashSetValues().build[java.lang.Long, IssuedAsset]()

    for (((addressId, asset), (currentBalance, balanceNode)) <- balances) {
      asset match {
        case Waves =>
          rw.put(Keys.wavesBalance(addressId), currentBalance)
          rw.put(Keys.wavesBalanceAt(addressId, currentBalance.height), balanceNode)
        case a: IssuedAsset =>
          changedAssetBalances.put(a, addressId.toLong)
          rw.put(Keys.assetBalance(addressId, a), currentBalance)
          rw.put(Keys.assetBalanceAt(addressId, a, currentBalance.height), balanceNode)

          val isNFT = currentBalance.balance > 0 && issuedAssets
            .get(a)
            .map(_.static.nft)
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

    changedAssetBalances.asMap().forEach { (asset, addresses) =>
      rw.put(Keys.changedBalances(height, asset), addresses.asScala.map(id => AddressId(id.toLong)).toSeq)
    }
  }

  // noinspection ScalaStyle
  override protected def doAppend(
      blockMeta: PBBlockMeta,
      carry: Long,
      newAddresses: Map[Address, AddressId],
      balances: Map[(AddressId, Asset), (CurrentBalance, BalanceNode)],
      leaseBalances: Map[AddressId, (CurrentLeaseBalance, LeaseBalanceNode)],
      addressTransactions: util.Map[AddressId, util.Collection[TransactionId]],
      leaseStates: Map[ByteStr, LeaseDetails],
      issuedAssets: Map[IssuedAsset, NewAssetInfo],
      updatedAssets: Map[IssuedAsset, Ior[AssetInfo, AssetVolumeInfo]],
      filledQuantity: Map[ByteStr, (CurrentVolumeAndFee, VolumeAndFeeNode)],
      scripts: Map[AddressId, Option[AccountScriptInfo]],
      assetScripts: Map[IssuedAsset, Option[AssetScriptInfo]],
      data: Map[Address, AccountDataInfo],
      aliases: Map[Alias, AddressId],
      sponsorship: Map[IssuedAsset, Sponsorship],
      scriptResults: Map[ByteStr, InvokeScriptResult],
      transactionMeta: Seq[(TxMeta, Transaction)],
      stateHash: StateHashBuilder.Result,
      ethereumTransactionMeta: Map[ByteStr, EthereumTransactionMeta]
  ): Unit = {
    log.trace(s"Persisting block ${blockMeta.id} at height $height")
    readWrite { rw =>
      val expiredKeys = new ArrayBuffer[Array[Byte]]

      rw.put(Keys.height, Height(height))

      val previousSafeRollbackHeight = rw.get(Keys.safeRollbackHeight)
      val newSafeRollbackHeight      = height - dbSettings.maxRollbackDepth

      if (previousSafeRollbackHeight < newSafeRollbackHeight) {
        rw.put(Keys.safeRollbackHeight, newSafeRollbackHeight)
      }

      val transactions: Map[TransactionId, (TxMeta, Transaction, TxNum)] = transactionMeta.zipWithIndex.map { case ((tm, tx), idx) =>
        TransactionId(tx.id()) -> ((tm, tx, TxNum(idx.toShort)))
      }.toMap

      rw.put(Keys.blockMetaAt(Height(height)), Some(blockMeta))
      rw.put(Keys.heightOf(blockMeta.id), Some(height))
      blockHeightCache.put(blockMeta.id, Some(height))

      val lastAddressId = loadMaxAddressId() + newAddresses.size

      rw.put(Keys.lastAddressId, Some(lastAddressId))

      for ((address, id) <- newAddresses) {
        val kaid = Keys.addressId(address)
        rw.put(kaid, Some(id))
        rw.put(Keys.idToAddress(id), address)
        addressFilter.put(kaid.suffix)
      }

      val threshold = newSafeRollbackHeight

      appendBalances(balances, issuedAssets, rw)

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

      for ((asset, NewAssetInfo(staticInfo, info, volumeInfo)) <- issuedAssets) {
        rw.put(Keys.assetStaticInfo(asset), staticInfo.some)
        rw.put(Keys.assetDetails(asset)(height), (info, volumeInfo))
      }

      for ((asset, infoToUpdate) <- updatedAssets) {
        rw.fromHistory(Keys.assetDetailsHistory(asset), Keys.assetDetails(asset))
          .orElse(issuedAssets.get(asset).map { case NewAssetInfo(_, i, vi) => (i, vi) })
          .foreach { case (info, vol) =>
            val updInfo = infoToUpdate.left
              .fold(info)(identity)

            val updVol = infoToUpdate.right
              .fold(vol)(_ |+| vol)

            rw.put(Keys.assetDetails(asset)(height), (updInfo, updVol))
          }
      }

      for (asset <- issuedAssets.keySet ++ updatedAssets.keySet) {
        expiredKeys ++= updateHistory(rw, Keys.assetDetailsHistory(asset), threshold, Keys.assetDetails(asset))
      }

      for ((leaseId, details) <- leaseStates) {
        rw.put(Keys.leaseDetails(leaseId)(height), Some(Right(details)))
        expiredKeys ++= updateHistory(rw, Keys.leaseDetailsHistory(leaseId), threshold, Keys.leaseDetails(leaseId))
      }

      for ((addressId, script) <- scripts) {
        expiredKeys ++= updateHistory(rw, Keys.addressScriptHistory(addressId), threshold, Keys.addressScript(addressId))
        if (script.isDefined) rw.put(Keys.addressScript(addressId)(height), script)
      }

      for ((asset, script) <- assetScripts) {
        expiredKeys ++= updateHistory(rw, Keys.assetScriptHistory(asset), threshold, Keys.assetScript(asset))
        if (script.isDefined) rw.put(Keys.assetScript(asset)(height), script)
      }

      for ((address, addressData) <- data) {
        val addressId = addressIdWithFallback(address, newAddresses)
        rw.put(Keys.changedDataKeys(height, addressId), addressData.data.keys.toSeq)

        for ((key, value) <- addressData.data) {
          val kdh  = Keys.dataMetaHistory(address, key)
          val size = rw.put(Keys.data(addressId, key)(height), Some(value))
          dataKeyFilter.put(kdh.suffix)
          expiredKeys ++= updateMetaHistory(rw, kdh, threshold, Keys.data(addressId, key), size)
        }
      }

      val txSizes = transactions.map { case (id, (txm, tx, num)) =>
        val size = rw.put(Keys.transactionAt(Height(height), num), Some((txm, tx)))
        rw.put(Keys.transactionMetaById(id), Some(TransactionMeta(height, num, tx.tpe.id, !txm.succeeded, 0, size)))
        id -> size
      }

      if (dbSettings.storeTransactionsByAddress) {
        val addressTxs = addressTransactions.asScala.toSeq.map { case (aid, txIds) =>
          (aid, txIds, Keys.addressTransactionSeqNr(aid))
        }
        rw.multiGetInts(addressTxs.map(_._3))
          .zip(addressTxs)
          .foreach { case (prevSeqNr, (addressId, txIds, txSeqNrKey)) =>
            val nextSeqNr = prevSeqNr.getOrElse(0) + 1
            val txTypeNumSeq = txIds.asScala.map { txId =>
              val (_, tx, num) = transactions(txId)
              val size         = txSizes(txId)
              (tx.tpe.id.toByte, num, size)
            }.toSeq
            rw.put(Keys.addressTransactionHN(addressId, nextSeqNr), Some((Height(height), txTypeNumSeq.sortBy(-_._2))))
            rw.put(txSeqNrKey, nextSeqNr)
          }
      }

      for ((alias, addressId) <- aliases) {
        rw.put(Keys.addressIdOfAlias(alias), Some(addressId))
      }

      val activationWindowSize = settings.functionalitySettings.activationWindowSize(height)
      if (height % activationWindowSize == 0) {
        val minVotes = settings.functionalitySettings.blocksForFeatureActivation(height)
        val newlyApprovedFeatures = featureVotes(height)
          .filterNot { case (featureId, _) => settings.functionalitySettings.preActivatedFeatures.contains(featureId) }
          .collect {
            case (featureId, voteCount) if voteCount + (if (blockMeta.getHeader.featureVotes.contains(featureId)) 1 else 0) >= minVotes =>
              featureId -> height
          }

        if (newlyApprovedFeatures.nonEmpty) {
          approvedFeaturesCache = newlyApprovedFeatures ++ rw.get(Keys.approvedFeatures)
          rw.put(Keys.approvedFeatures, approvedFeaturesCache)

          val featuresToSave = (newlyApprovedFeatures.view.mapValues(_ + activationWindowSize) ++ rw.get(Keys.activatedFeatures)).toMap

          activatedFeaturesCache = featuresToSave ++ settings.functionalitySettings.preActivatedFeatures
          rw.put(Keys.activatedFeatures, featuresToSave)
        }
      }

      for (case (asset, sp: SponsorshipValue) <- sponsorship) {
        rw.put(Keys.sponsorship(asset)(height), sp)
        expiredKeys ++= updateHistory(rw, Keys.sponsorshipHistory(asset), threshold, Keys.sponsorship(asset))
      }

      rw.put(Keys.issuedAssets(height), issuedAssets.keySet.toSeq)
      rw.put(Keys.updatedAssets(height), updatedAssets.keySet.toSeq)
      rw.put(Keys.sponsorshipAssets(height), sponsorship.keySet.toSeq)

      rw.put(Keys.carryFee(height), carry)
      expiredKeys += Keys.carryFee(threshold - 1).keyBytes

      if (dbSettings.storeInvokeScriptResults) scriptResults.foreach { case (txId, result) =>
        val (txHeight, txNum) = transactions
          .get(TransactionId(txId))
          .map { case (_, _, txNum) => (height, txNum) }
          .orElse(rw.get(Keys.transactionMetaById(TransactionId(txId))).map { tm =>
            (tm.height, TxNum(tm.num.toShort))
          })
          .getOrElse(throw new IllegalArgumentException(s"Couldn't find transaction height and num: $txId"))

        try rw.put(Keys.invokeScriptResult(txHeight, txNum), Some(result))
        catch {
          case NonFatal(e) =>
            throw new RuntimeException(s"Error storing invoke script result for $txId: $result", e)
        }
      }

      for ((id, meta) <- ethereumTransactionMeta) {
        rw.put(Keys.ethereumTransactionMeta(Height(height), transactions(TransactionId(id))._3), Some(meta))
      }

      expiredKeys.foreach(rw.delete(_, "expired-keys"))

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

  override protected def doRollback(targetHeight: Int): Seq[(Block, ByteStr)] = {
    val targetBlockId = readOnly(_.get(Keys.blockMetaAt(Height @@ targetHeight)))
      .map(_.id)
      .getOrElse(throw new IllegalArgumentException(s"No block at height $targetHeight"))

    log.debug(s"Rolling back to block $targetBlockId at $targetHeight")

    val discardedBlocks: Seq[(Block, ByteStr)] =
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

          rw.iterateOver(KeyTags.ChangedAssetBalances.prefixBytes ++ Ints.toByteArray(currentHeight)) { e =>
            val assetId = IssuedAsset(ByteStr(e.getKey.takeRight(32)))
            for ((addressId, address) <- changedAddresses) {
              balancesToInvalidate += address -> assetId
              rollbackBalanceHistory(rw, Keys.assetBalance(addressId, assetId), Keys.assetBalanceAt(addressId, assetId, _), currentHeight)
            }
          }

          for ((addressId, address) <- changedAddresses) {
            for (k <- rw.get(Keys.changedDataKeys(currentHeight, addressId))) {
              log.trace(s"Discarding $k for $address at $currentHeight")
              accountDataToInvalidate += (address -> k)
              rw.delete(Keys.data(addressId, k)(currentHeight))
              rw.filterMetaHistory(Keys.dataMetaHistory(address, k), currentHeight)
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
          }

          writableDB
            .withResource(loadLeaseIds(_, currentHeight, currentHeight, includeCancelled = true))
            .foreach(rollbackLeaseStatus(rw, _, currentHeight))

          rollbackAssetsInfo(rw, currentHeight)

          val blockTxs = loadTransactions(currentHeight, rw)
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
              rw.delete(Keys.transactionAt(currentHeight, num))
              rw.delete(Keys.transactionMetaById(TransactionId(tx.id())))
            }
          }

          rw.delete(Keys.blockMetaAt(currentHeight))
          rw.delete(Keys.changedAddresses(currentHeight))
          rw.delete(Keys.heightOf(discardedMeta.id))
          blockHeightsToInvalidate.addOne(discardedMeta.id)
          rw.delete(Keys.carryFee(currentHeight))
          rw.delete(Keys.blockTransactionsFee(currentHeight))
          rw.delete(Keys.stateHash(currentHeight))

          if (DisableHijackedAliases.height == currentHeight) {
            disabledAliases = DisableHijackedAliases.revert(rw)
          }

          val block = createBlock(
            PBBlocks.vanilla(
              discardedMeta.header.getOrElse(throw new IllegalArgumentException(s"Block header is missing at height ${currentHeight.toInt}"))
            ),
            ByteStr(discardedMeta.signature.toByteArray),
            blockTxs.map(_._2)
          ).explicitGet()

          (block, Caches.toHitSource(discardedMeta))
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
      tm <- db.get(Keys.transactionMetaById(TransactionId @@ id))
      if tm.`type` == TransferTransaction.typeId || tm.`type` == TransactionType.Ethereum.id
      tx <- db
        .get(Keys.transactionAt(Height(tm.height), TxNum(tm.num.toShort)))
        .collect {
          case (tm, t: TransferTransaction) if tm.succeeded => t
          case (m, e @ EthereumTransaction(_: Transfer, _, _, _)) if m.succeeded =>
            val meta     = db.get(Keys.ethereumTransactionMeta(m.height, TxNum(tm.num.toShort))).get
            val transfer = meta.payload.transfer.get
            val tAmount  = transfer.amount.get
            val asset    = PBAmounts.toVanillaAssetId(tAmount.assetId)
            e.toTransferLike(TxPositiveAmount.unsafeFrom(tAmount.amount), Address(transfer.publicKeyHash.toByteArray), asset)
        }
      //      tx = txdb.load(id) match {
//        case t: TransferTransaction if !tm.failed => t
//        case e @ EthereumTransaction(_: Transfer, _, _, _) if !tm.failed =>
//          val meta     = db.get(Keys.ethereumTransactionMeta(Height(tm.height), TxNum(tm.num.toShort))).get
//          val transfer = meta.payload.transfer.get
//          val tAmount  = transfer.amount.get
//          val asset    = PBAmounts.toVanillaAssetId(tAmount.assetId)
//          e.toTransferLike(TxPositiveAmount.unsafeFrom(tAmount.amount), Address(transfer.publicKeyHash.toByteArray), asset)
//      }
    } yield (height, tx)
  }

  override def transactionInfo(id: ByteStr): Option[(TxMeta, Transaction)] = readOnly(transactionInfo(id, _))

  override def transactionInfos(ids: Seq[ByteStr]): Seq[Option[(TxMeta, Transaction)]] = readOnly { db =>
    val tms = db.multiGetOpt(ids.map(id => Keys.transactionMetaById(TransactionId(id))), 36)
    val (keys, sizes) = tms.map {
      case Some(tm) => Keys.transactionAt(Height(tm.height), TxNum(tm.num.toShort)) -> tm.size
      case None     => Keys.transactionAt(Height(0), TxNum(0.toShort))              -> 0
    }.unzip

    db.multiGetOpt(keys, sizes)
  }

  protected def transactionInfo(id: ByteStr, db: ReadOnlyDB): Option[(TxMeta, Transaction)] =
    for {
      tm        <- db.get(Keys.transactionMetaById(TransactionId(id)))
      (txm, tx) <- db.get(Keys.transactionAt(Height(tm.height), TxNum(tm.num.toShort)))
    } yield (txm, tx)
//    transactionMeta(id).map(tm => tm -> txdb.load(id))

  override def transactionMeta(id: ByteStr): Option[TxMeta] = readOnly { db =>
    db.get(Keys.transactionMetaById(TransactionId(id))).map { tm =>
      TxMeta(Height(tm.height), !tm.failed, tm.spentComplexity)
    }
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
      h             <- db.get(Keys.leaseDetailsHistory(leaseId)).headOption
      detailsOrFlag <- db.get(Keys.leaseDetails(leaseId)(h))
      details <- detailsOrFlag.fold(
        isActive =>
          transactionInfo(leaseId, db).collect { case (txm, lt: LeaseTransaction) =>
            LeaseDetails(
              lt.sender,
              lt.recipient,
              lt.amount.value,
              if (isActive) LeaseDetails.Status.Active
              else LeaseDetails.Status.Cancelled(h, None),
              leaseId,
              txm.height
            )
          },
        Some(_)
      )
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
        if (hh < from)
          acc :+ hh
        else {
          val bn     = balanceAtHeightCache.get((hh, addressId), () => db.get(Keys.wavesBalanceAt(addressId, Height(hh))))
          val newAcc = if (hh > toHeight) acc else acc :+ hh
          collectBalanceHistory(newAcc, bn.prevHeight)
        }

      @tailrec
      def collectLeaseBalanceHistory(acc: Vector[Int], hh: Int): Seq[Int] =
        if (hh < from)
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
      } yield BalanceSnapshot(wh.max(lh), wb.balance, lb.in, lb.out)
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
        settings.rewardsSettings
          .votingWindow(activatedAt, height)
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

  override def resolveERC20Address(address: ERC20Address): Option[IssuedAsset] =
    readOnly(_.get(Keys.assetStaticInfo(address)).map(assetInfo => IssuedAsset(assetInfo.id)))
}
