package com.wavesplatform.database

import java.nio.ByteBuffer

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.jdk.CollectionConverters._
import scala.util.Try
import scala.util.control.NonFatal

import cats.data.Ior
import cats.implicits._
import com.google.common.cache.CacheBuilder
import com.google.common.collect.MultimapBuilder
import com.google.common.primitives.{Ints, Shorts}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.block.{Block, SignedBlockHeader}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.database
import com.wavesplatform.database.patch.DisableHijackedAliases
import com.wavesplatform.database.protobuf.TransactionMeta
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.settings.{BlockchainSettings, DBSettings, WavesSettings}
import com.wavesplatform.state.{TxNum, _}
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.{AliasDoesNotExist, AliasIsDisabled}
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.{LoggerFacade, ScorexLogging}
import monix.reactive.Observer
import org.iq80.leveldb.DB
import org.slf4j.LoggerFactory

object LevelDBWriter extends ScorexLogging {

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

    def hasInHistory(historyKey: Key[Seq[Int]], v: Int => Key[_]): Boolean =
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

  private def loadHeight(db: DB): Int = db.get(Keys.height)

  private[database] def merge(wbh: Seq[Int], lbh: Seq[Int]): Seq[(Int, Int)] = {

    /**
      * Fixed implementation where
      *  {{{([15, 12, 3], [12, 5]) => [(15, 12), (12, 12), (3, 5)]}}}
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

  def apply(db: DB, spendableBalanceChanged: Observer[(Address, Asset)], settings: WavesSettings): LevelDBWriter with AutoCloseable = {
    val expectedHeight = loadHeight(db)
    def load(name: String, key: KeyTags.KeyTag): Option[BloomFilterImpl] = {
      if (settings.dbSettings.useBloomFilter)
        Some(BloomFilter.loadOrPopulate(db, settings.dbSettings.directory, name, expectedHeight, key, 100000000))
      else
        None
    }

    val _orderFilter        = load("orders", KeyTags.FilledVolumeAndFeeHistory)
    val _dataKeyFilter      = load("account-data", KeyTags.DataHistory)
    val _wavesBalanceFilter = load("waves-balances", KeyTags.WavesBalanceHistory)
    val _assetBalanceFilter = load("asset-balances", KeyTags.AssetBalanceHistory)
    new LevelDBWriter(db, spendableBalanceChanged, settings.blockchainSettings, settings.dbSettings) with AutoCloseable {

      override val orderFilter: BloomFilter        = _orderFilter.getOrElse(BloomFilter.AlwaysEmpty)
      override val dataKeyFilter: BloomFilter      = _dataKeyFilter.getOrElse(BloomFilter.AlwaysEmpty)
      override val wavesBalanceFilter: BloomFilter = _wavesBalanceFilter.getOrElse(BloomFilter.AlwaysEmpty)
      override val assetBalanceFilter: BloomFilter = _assetBalanceFilter.getOrElse(BloomFilter.AlwaysEmpty)

      override def close(): Unit = {
        log.debug("Shutting down LevelDBWriter")
        val lastHeight = LevelDBWriter.loadHeight(db)
        _orderFilter.foreach(_.save(lastHeight))
        _dataKeyFilter.foreach(_.save(lastHeight))
        _wavesBalanceFilter.foreach(_.save(lastHeight))
        _assetBalanceFilter.foreach(_.save(lastHeight))
      }
    }
  }

  def readOnly(db: DB, settings: WavesSettings): LevelDBWriter = {
    val expectedHeight = loadHeight(db)
    def loadFilter(filterName: String) =
      if (settings.dbSettings.useBloomFilter)
        BloomFilter
          .tryLoad(db, filterName, settings.dbSettings.directory, expectedHeight)
          .fold(_ => BloomFilter.AlwaysEmpty, gf => new Wrapper(gf))
      else
        BloomFilter.AlwaysEmpty

    new LevelDBWriter(db, Observer.stopped, settings.blockchainSettings, settings.dbSettings) {
      override val orderFilter: BloomFilter        = loadFilter("orders")
      override val dataKeyFilter: BloomFilter      = loadFilter("account-data")
      override val wavesBalanceFilter: BloomFilter = loadFilter("waves-balances")
      override val assetBalanceFilter: BloomFilter = loadFilter("asset-balances")
    }
  }
}

//noinspection UnstableApiUsage
abstract class LevelDBWriter private[database] (
    writableDB: DB,
    spendableBalanceChanged: Observer[(Address, Asset)],
    val settings: BlockchainSettings,
    val dbSettings: DBSettings
) extends Caches(spendableBalanceChanged) {

  private[this] val log = LoggerFacade(LoggerFactory.getLogger(classOf[LevelDBWriter]))

  def orderFilter: BloomFilter
  def dataKeyFilter: BloomFilter
  def wavesBalanceFilter: BloomFilter
  def assetBalanceFilter: BloomFilter

  private[this] var disabledAliases = writableDB.get(Keys.disabledAliases)

  private[this] val balanceSnapshotMaxRollbackDepth: Int = dbSettings.maxRollbackDepth + 1000
  import LevelDBWriter._

  private[database] def readOnly[A](f: ReadOnlyDB => A): A = writableDB.readOnly(f)

  private[this] def readWrite[A](f: RW => A): A = writableDB.readWrite(f)

  private def loadWithFilter[A, R](filter: BloomFilter, key: Key[A])(f: (ReadOnlyDB, A) => Option[R]): Option[R] =
    if (filter.mightContain(key.suffix)) readOnly { ro =>
      f(ro, ro.get(key))
    } else None

  override protected def loadMaxAddressId(): Long = readOnly(db => db.get(Keys.lastAddressId).getOrElse(0L))

  override protected def loadAddressId(address: Address): Option[AddressId] = readOnly(db => db.get(Keys.addressId(address)))

  override protected def loadHeight(): Int = LevelDBWriter.loadHeight(writableDB)

  override def safeRollbackHeight: Int = readOnly(_.get(Keys.safeRollbackHeight))

  override protected def loadScore(): BigInt = readOnly(db => db.get(Keys.score(db.get(Keys.height))))

  override protected def loadLastBlock(): Option[Block] = readOnly { db =>
    loadBlock(Height(db.get(Keys.height)), db)
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

  override protected def loadAccountData(address: Address, key: String): Option[DataEntry[_]] =
    loadWithFilter(dataKeyFilter, Keys.dataHistory(address, key)) { (ro, history) =>
      for {
        aid <- addressId(address)
        h   <- history.headOption
        e   <- ro.get(Keys.data(aid, key)(h))
      } yield e
    }

  override def hasData(address: Address): Boolean = {
    writableDB.readOnly { ro =>
      ro.get(Keys.addressId(address)).fold(false) { addressId =>
        ro.prefixExists(KeyTags.ChangedDataKeys.prefixBytes ++ addressId.toByteArray)
      }
    }
  }

  protected override def loadBalance(req: (Address, Asset)): Long =
    addressId(req._1).fold(0L) { addressId =>
      req._2 match {
        case asset @ IssuedAsset(_) =>
          val kabh = Keys.assetBalanceHistory(addressId, asset)
          if (assetBalanceFilter.mightContain(kabh.suffix))
            writableDB.readOnly(_.fromHistory(kabh, Keys.assetBalance(addressId, asset))).getOrElse(0L)
          else 0L
        case Waves =>
          val kwbh = Keys.wavesBalanceHistory(addressId)
          if (wavesBalanceFilter.mightContain(kwbh.suffix))
            writableDB.readOnly(_.fromHistory(kwbh, Keys.wavesBalance(addressId))).getOrElse(0L)
          else 0L
      }
    }

  private def loadLeaseBalance(db: ReadOnlyDB, addressId: AddressId): LeaseBalance =
    db.fromHistory(Keys.leaseBalanceHistory(addressId), Keys.leaseBalance(addressId)).getOrElse(LeaseBalance.empty)

  override protected def loadLeaseBalance(address: Address): LeaseBalance = readOnly { db =>
    addressId(address).fold(LeaseBalance.empty)(loadLeaseBalance(db, _))
  }

  private[database] def loadLposPortfolio(db: ReadOnlyDB, addressId: AddressId) = Portfolio(
    db.fromHistory(Keys.wavesBalanceHistory(addressId), Keys.wavesBalance(addressId)).getOrElse(0L),
    loadLeaseBalance(db, addressId),
    Map.empty
  )

  override protected def loadAssetDescription(asset: IssuedAsset): Option[AssetDescription] =
    writableDB.withResource(r => database.loadAssetDescription(r, asset))

  override protected def loadVolumeAndFee(orderId: ByteStr): VolumeAndFee =
    loadWithFilter(orderFilter, Keys.filledVolumeAndFeeHistory(orderId)) { (ro, history) =>
      history.headOption.map(h => ro.get(Keys.filledVolumeAndFee(orderId)(h)))
    }.orEmpty

  override protected def loadApprovedFeatures(): Map[Short, Int] = {
    readOnly(_.get(Keys.approvedFeatures))
  }

  override protected def loadActivatedFeatures(): Map[Short, Int] = {
    val stateFeatures = readOnly(_.get(Keys.activatedFeatures))
    stateFeatures ++ settings.functionalitySettings.preActivatedFeatures
  }

  override def wavesAmount(height: Int): BigInt = readOnly { db =>
    val factHeight = height.min(this.height)
    if (db.has(Keys.wavesAmount(factHeight))) db.get(Keys.wavesAmount(factHeight))
    else settings.genesisSettings.initialBalance
  }

  override def blockReward(height: Int): Option[Long] =
    readOnly(_.db.get(Keys.blockReward(height)))

  private def updateHistory(rw: RW, key: Key[Seq[Int]], threshold: Int, kf: Int => Key[_]): Seq[Array[Byte]] =
    updateHistory(rw, rw.get(key), key, threshold, kf)

  private def updateHistory(rw: RW, history: Seq[Int], key: Key[Seq[Int]], threshold: Int, kf: Int => Key[_]): Seq[Array[Byte]] = {
    val (c1, c2) = history.partition(_ >= threshold)
    rw.put(key, (height +: c1) ++ c2.headOption)
    c2.drop(1).map(kf(_).keyBytes)
  }

  private def appendBalances(
      balances: Map[AddressId, Map[Asset, Long]],
      issuedAssets: Map[IssuedAsset, NewAssetInfo],
      rw: RW,
      threshold: Int,
      balanceThreshold: Int
  ): Unit = {
    val changedAssetBalances = MultimapBuilder.hashKeys().hashSetValues().build[IssuedAsset, java.lang.Long]()
    val updatedNftLists      = MultimapBuilder.hashKeys().hashSetValues().build[java.lang.Long, IssuedAsset]()

    for ((addressId, updatedBalances) <- balances) {
      for ((asset, balance) <- updatedBalances) {
        asset match {
          case Waves =>
            rw.put(Keys.wavesBalance(addressId)(height), balance)
            val kwbh = Keys.wavesBalanceHistory(addressId)
            if (wavesBalanceFilter.mightContain(kwbh.suffix))
              updateHistory(rw, kwbh, balanceThreshold, Keys.wavesBalance(addressId)).foreach(rw.delete)
            else {
              rw.put(kwbh, Seq(height))
              wavesBalanceFilter.put(kwbh.suffix)
            }
          case a: IssuedAsset =>
            changedAssetBalances.put(a, addressId.toLong)
            rw.put(Keys.assetBalance(addressId, a)(height), balance)
            val kabh = Keys.assetBalanceHistory(addressId, a)
            val isNFT = balance > 0 && issuedAssets
              .get(a)
              .map(_.static.nft)
              .orElse(assetDescription(a).map(_.nft))
              .getOrElse(false)
            if (assetBalanceFilter.mightContain(kabh.suffix)) {
              if (rw.get(kabh).isEmpty && isNFT) updatedNftLists.put(addressId.toLong, a)
              updateHistory(rw, kabh, threshold, Keys.assetBalance(addressId, a)).foreach(rw.delete)
            } else {
              rw.put(kabh, Seq(height))
              assetBalanceFilter.put(kabh.suffix)
              if (isNFT) updatedNftLists.put(addressId.toLong, a)
            }
        }
      }
    }

    for ((addressId, nftIds) <- updatedNftLists.asMap().asScala) {
      val kCount           = Keys.nftCount(AddressId(addressId.toLong))
      val previousNftCount = rw.get(kCount)
      rw.put(kCount, previousNftCount + nftIds.size())
      for ((id, idx) <- nftIds.asScala.zipWithIndex) {
        rw.put(Keys.nftAt(AddressId(addressId.toLong), idx, id), Some(()))
      }
    }

    changedAssetBalances.asMap().forEach { (asset, addresses) =>
      rw.put(Keys.changedBalances(height, asset), addresses.asScala.map(id => AddressId(id.toLong)).toSeq)
    }
  }

  //noinspection ScalaStyle
  override protected def doAppend(
      block: Block,
      carry: Long,
      newAddresses: Map[Address, AddressId],
      balances: Map[AddressId, Map[Asset, Long]],
      leaseBalances: Map[AddressId, LeaseBalance],
      addressTransactions: Map[AddressId, Seq[TransactionId]],
      leaseStates: Map[ByteStr, LeaseDetails],
      issuedAssets: Map[IssuedAsset, NewAssetInfo],
      updatedAssets: Map[IssuedAsset, Ior[AssetInfo, AssetVolumeInfo]],
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
      stateHash: StateHashBuilder.Result
  ): Unit = {
    log.trace(s"Persisting block ${block.id()} at height $height")
    readWrite { rw =>
      val expiredKeys = new ArrayBuffer[Array[Byte]]

      rw.put(Keys.height, height)

      val previousSafeRollbackHeight = rw.get(Keys.safeRollbackHeight)
      val newSafeRollbackHeight      = height - dbSettings.maxRollbackDepth

      if (previousSafeRollbackHeight < newSafeRollbackHeight) {
        rw.put(Keys.safeRollbackHeight, newSafeRollbackHeight)
      }

      val transactions: Map[TransactionId, (Transaction, TxNum, Boolean)] =
        block.transactionData.zipWithIndex.map { in =>
          val (tx, idx) = in
          val k         = TransactionId(tx.id())
          val v         = (tx, TxNum(idx.toShort), !failedTransactionIds.contains(tx.id()))
          k -> v
        }.toMap

      rw.put(
        Keys.blockMetaAt(Height(height)),
        Some(
          BlockMeta.fromBlock(block, height, totalFee, reward, if (block.header.version >= Block.ProtoBlockVersion) Some(hitSource) else None)
        )
      )
      rw.put(Keys.heightOf(block.id()), Some(height))

      val lastAddressId = loadMaxAddressId() + newAddresses.size

      rw.put(Keys.lastAddressId, Some(lastAddressId))
      rw.put(Keys.score(height), rw.get(Keys.score(height - 1)) + block.blockScore())

      for ((address, id) <- newAddresses) {
        rw.put(Keys.addressId(address), Some(id))
        rw.put(Keys.idToAddress(id), address)
      }

      val threshold        = newSafeRollbackHeight
      val balanceThreshold = height - balanceSnapshotMaxRollbackDepth

      appendBalances(balances, issuedAssets, rw, threshold, balanceThreshold)

      val changedAddresses = (addressTransactions.keys ++ balances.keys).toSet
      rw.put(Keys.changedAddresses(height), changedAddresses.toSeq)

      // leases
      for ((addressId, leaseBalance) <- leaseBalances) {
        rw.put(Keys.leaseBalance(addressId)(height), leaseBalance)
        expiredKeys ++= updateHistory(rw, Keys.leaseBalanceHistory(addressId), balanceThreshold, Keys.leaseBalance(addressId))
      }

      for ((orderId, volumeAndFee) <- filledQuantity) {
        orderFilter.put(orderId.arr)
        rw.put(Keys.filledVolumeAndFee(orderId)(height), volumeAndFee)
        expiredKeys ++= updateHistory(rw, Keys.filledVolumeAndFeeHistory(orderId), threshold, Keys.filledVolumeAndFee(orderId))
      }

      for ((asset, NewAssetInfo(staticInfo, info, volumeInfo)) <- issuedAssets) {
        rw.put(Keys.assetStaticInfo(asset), staticInfo.some)
        rw.put(Keys.assetDetails(asset)(height), (info, volumeInfo))
      }

      for ((asset, infoToUpdate) <- updatedAssets) {
        rw.fromHistory(Keys.assetDetailsHistory(asset), Keys.assetDetails(asset))
          .orElse(issuedAssets.get(asset).map { case NewAssetInfo(_, i, vi) => (i, vi) })
          .foreach {
            case (info, vol) =>
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
          val kdh = Keys.dataHistory(address, key)
          rw.put(Keys.data(addressId, key)(height), Some(value))
          dataKeyFilter.put(kdh.suffix)
          expiredKeys ++= updateHistory(rw, kdh, threshold, Keys.data(addressId, key))
        }
      }

      if (dbSettings.storeTransactionsByAddress) for ((addressId, txIds) <- addressTransactions) {
        val kk        = Keys.addressTransactionSeqNr(addressId)
        val nextSeqNr = rw.get(kk) + 1
        val txTypeNumSeq = txIds.map { txId =>
          val (tx, num, _) = transactions(txId)
          (tx.typeId, num)
        }
        rw.put(Keys.addressTransactionHN(addressId, nextSeqNr), Some((Height(height), txTypeNumSeq.sortBy(-_._2))))
        rw.put(kk, nextSeqNr)
      }

      for ((alias, addressId) <- aliases) {
        rw.put(Keys.addressIdOfAlias(alias), Some(addressId))
      }

      for ((id, (tx, num, succeeded)) <- transactions) {
        rw.put(Keys.transactionAt(Height(height), num), Some((tx, succeeded)))
        rw.put(Keys.transactionMetaById(id), Some(TransactionMeta(height, num, tx.typeId, !succeeded)))
      }

      val activationWindowSize = settings.functionalitySettings.activationWindowSize(height)
      if (height % activationWindowSize == 0) {
        val minVotes = settings.functionalitySettings.blocksForFeatureActivation(height)
        val newlyApprovedFeatures = featureVotes(height)
          .filterNot { case (featureId, _) => settings.functionalitySettings.preActivatedFeatures.contains(featureId) }
          .collect {
            case (featureId, voteCount) if voteCount + (if (block.header.featureVotes.contains(featureId)) 1 else 0) >= minVotes =>
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

      reward.foreach { lastReward =>
        rw.put(Keys.blockReward(height), Some(lastReward))
        rw.put(Keys.wavesAmount(height), wavesAmount(height - 1) + lastReward)
      }

      for ((asset, sp: SponsorshipValue) <- sponsorship) {
        rw.put(Keys.sponsorship(asset)(height), sp)
        expiredKeys ++= updateHistory(rw, Keys.sponsorshipHistory(asset), threshold, Keys.sponsorship(asset))
      }

      rw.put(Keys.issuedAssets(height), issuedAssets.keySet.toSeq)
      rw.put(Keys.updatedAssets(height), updatedAssets.keySet.toSeq)
      rw.put(Keys.sponsorshipAssets(height), sponsorship.keySet.toSeq)

      rw.put(Keys.carryFee(height), carry)
      expiredKeys += Keys.carryFee(threshold - 1).keyBytes

      rw.put(Keys.blockTransactionsFee(height), totalFee)

      if (dbSettings.storeInvokeScriptResults) scriptResults.foreach {
        case (txId, result) =>
          val (txHeight, txNum) = transactions
            .get(TransactionId(txId))
            .map { case (_, txNum, _) => (height, txNum) }
            .orElse(rw.get(Keys.transactionMetaById(TransactionId(txId))).map {
              case TransactionMeta(height, txNum, _, _) => (height, TxNum(txNum.toShort))
            })
            .getOrElse(throw new IllegalArgumentException(s"Couldn't find transaction height and num: $txId"))

          try rw.put(Keys.invokeScriptResult(txHeight, txNum), Some(result))
          catch {
            case NonFatal(e) =>
              throw new RuntimeException(s"Error storing invoke script result for $txId: $result", e)
          }
      }

      expiredKeys.foreach(rw.delete(_, "expired-keys"))

      if (DisableHijackedAliases.height == height) {
        disabledAliases = DisableHijackedAliases(rw)
      }

      rw.put(Keys.hitSource(height), Some(hitSource))

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

    log.trace(s"Finished persisting block ${block.id()} at height $height")
  }

  override protected def doRollback(targetHeight: Int): Seq[(Block, ByteStr)] = {
    val targetBlockId = readOnly(_.get(Keys.blockMetaAt(Height @@ targetHeight)))
      .map(_.id)
      .getOrElse(throw new IllegalArgumentException(s"No block at height $targetHeight"))

    log.debug(s"Rolling back to block $targetBlockId at $targetHeight")

    val discardedBlocks: Seq[(Block, ByteStr)] = for (currentHeight <- height until targetHeight by -1) yield {
      val balancesToInvalidate    = Seq.newBuilder[(Address, Asset)]
      val ordersToInvalidate      = Seq.newBuilder[ByteStr]
      val scriptsToDiscard        = Seq.newBuilder[Address]
      val assetScriptsToDiscard   = Seq.newBuilder[IssuedAsset]
      val accountDataToInvalidate = Seq.newBuilder[(Address, String)]

      val h = Height(currentHeight)

      val discardedBlock = readWrite { rw =>
        rw.put(Keys.height, currentHeight - 1)

        val discardedMeta = rw
          .get(Keys.blockMetaAt(h))
          .getOrElse(throw new IllegalArgumentException(s"No block at height $currentHeight"))

        log.trace(s"Removing block ${discardedMeta.id} at $currentHeight")
        rw.delete(Keys.blockMetaAt(h))

        val changedAddresses = for {
          addressId <- rw.get(Keys.changedAddresses(currentHeight))
        } yield addressId -> rw.get(Keys.idToAddress(addressId))

        rw.iterateOver(KeyTags.ChangedAssetBalances.prefixBytes ++ Ints.toByteArray(h)) { e =>
          val assetId = IssuedAsset(ByteStr(e.getKey.takeRight(32)))
          for ((addressId, address) <- changedAddresses) {
            val kabh    = Keys.assetBalanceHistory(addressId, assetId)
            val history = rw.get(kabh)
            if (history.nonEmpty && history.head == currentHeight) {
              log.trace(s"Discarding ${assetId.id} balance for $address at $currentHeight")
              balancesToInvalidate += address -> assetId
              rw.delete(Keys.assetBalance(addressId, assetId)(history.head))
              rw.put(kabh.keyBytes, writeIntSeq(history.tail))
            }
          }
        }

        for ((addressId, address) <- changedAddresses) {
          for (k <- rw.get(Keys.changedDataKeys(currentHeight, addressId))) {
            log.trace(s"Discarding $k for $address at $currentHeight")
            accountDataToInvalidate += (address -> k)
            rw.delete(Keys.data(addressId, k)(currentHeight))
            rw.filterHistory(Keys.dataHistory(address, k), currentHeight)
          }
          rw.delete(Keys.changedDataKeys(currentHeight, addressId))

          balancesToInvalidate += (address -> Waves)
          rw.delete(Keys.wavesBalance(addressId)(currentHeight))
          rw.filterHistory(Keys.wavesBalanceHistory(addressId), currentHeight)

          rw.delete(Keys.leaseBalance(addressId)(currentHeight))
          rw.filterHistory(Keys.leaseBalanceHistory(addressId), currentHeight)

          balanceAtHeightCache.invalidate((currentHeight, addressId))
          leaseBalanceAtHeightCache.invalidate((currentHeight, addressId))
          discardLeaseBalance(address)

          if (dbSettings.storeTransactionsByAddress) {
            val kTxSeqNr = Keys.addressTransactionSeqNr(addressId)
            val txSeqNr  = rw.get(kTxSeqNr)
            val kTxHNSeq = Keys.addressTransactionHN(addressId, txSeqNr)

            rw.get(kTxHNSeq)
              .filter(_._1 == Height(currentHeight))
              .foreach { _ =>
                rw.delete(kTxHNSeq)
                rw.put(kTxSeqNr, (txSeqNr - 1).max(0))
              }
          }
        }

        writableDB
          .withResource(loadLeaseIds(_, currentHeight, currentHeight, includeCancelled = true))
          .foreach(rollbackLeaseStatus(rw, _, currentHeight))

        rollbackAssetsInfo(rw, currentHeight)

        val transactions = transactionsAtHeight(h)

        transactions.foreach {
          case (num, tx) =>
            forgetTransaction(tx.id())
            tx match {
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

              case _: InvokeScriptTransaction =>
                val k = Keys.invokeScriptResult(h, num)
                rw.delete(k)

              case tx: CreateAliasTransaction => rw.delete(Keys.addressIdOfAlias(tx.alias))
              case tx: ExchangeTransaction =>
                ordersToInvalidate += rollbackOrderFill(rw, tx.buyOrder.id(), currentHeight)
                ordersToInvalidate += rollbackOrderFill(rw, tx.sellOrder.id(), currentHeight)
            }

            if (tx.typeId != GenesisTransaction.typeId) {
              rw.delete(Keys.transactionAt(h, num))
              rw.delete(Keys.transactionMetaById(TransactionId(tx.id())))
            }
        }

        rw.delete(Keys.blockMetaAt(h))
        rw.delete(Keys.heightOf(discardedMeta.id))
        rw.delete(Keys.carryFee(currentHeight))
        rw.delete(Keys.blockTransactionsFee(currentHeight))
        rw.delete(Keys.blockReward(currentHeight))
        rw.delete(Keys.wavesAmount(currentHeight))
        rw.delete(Keys.stateHash(currentHeight))

        if (DisableHijackedAliases.height == currentHeight) {
          disabledAliases = DisableHijackedAliases.revert(rw)
        }

        val hitSource = rw.get(Keys.hitSource(currentHeight)).get
        val block     = createBlock(discardedMeta.header, discardedMeta.signature, transactions.map(_._2)).explicitGet()

        (block, hitSource)
      }

      balancesToInvalidate.result().foreach(discardBalance)
      ordersToInvalidate.result().foreach(discardVolumeAndFee)
      scriptsToDiscard.result().foreach(discardScript)
      assetScriptsToDiscard.result().foreach(discardAssetScript)
      accountDataToInvalidate.result().foreach(discardAccountData)
      discardedBlock
    }

    log.debug(s"Rollback to block $targetBlockId at $targetHeight completed")
    discardedBlocks.reverse
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

  private def rollbackOrderFill(rw: RW, orderId: ByteStr, currentHeight: Int): ByteStr = {
    rw.delete(Keys.filledVolumeAndFee(orderId)(currentHeight))
    rw.filterHistory(Keys.filledVolumeAndFeeHistory(orderId), currentHeight)
    orderId
  }

  private def rollbackLeaseStatus(rw: RW, leaseId: ByteStr, currentHeight: Int): Unit = {
    rw.delete(Keys.leaseDetails(leaseId)(currentHeight))
    rw.filterHistory(Keys.leaseDetailsHistory(leaseId), currentHeight)
  }

  override def transferById(id: ByteStr): Option[(Int, TransferTransaction)] = readOnly { db =>
    for {
      TransactionMeta(height, num, TransferTransaction.typeId, _) <- db.get(Keys.transactionMetaById(TransactionId @@ id))
      tx                                                          <- db.get(Keys.transactionAt(Height(height), TxNum(num.toShort))).collect { case (t: TransferTransaction, true) => t }
    } yield (height, tx)
  }

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction, Boolean)] = readOnly(transactionInfo(id, _))

  protected def transactionInfo(id: ByteStr, db: ReadOnlyDB): Option[(Int, Transaction, Boolean)] = {
    val txId = TransactionId(id)
    for {
      TransactionMeta(height, num, _, failed) <- db.get(Keys.transactionMetaById(txId))
      (tx, _)                                 <- db.get(Keys.transactionAt(Height(height), TxNum(num.toShort)))
    } yield (height, tx, !failed)
  }

  override def transactionMeta(id: ByteStr): Option[(Int, Boolean)] = readOnly { db =>
    db.get(Keys.transactionMetaById(TransactionId(id))).map { case TransactionMeta(height, _, _, failed) => (height, !failed) }
  }

  override def resolveAlias(alias: Alias): Either[ValidationError, Address] = readOnly { db =>
    if (disabledAliases.contains(alias)) Left(AliasIsDisabled(alias))
    else
      db.get(Keys.addressIdOfAlias(alias))
        .map(addressId => db.get(Keys.idToAddress(addressId)))
        .toRight(AliasDoesNotExist(alias))
  }

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = readOnly { db =>
    for {
      h             <- db.get(Keys.leaseDetailsHistory(leaseId)).headOption
      detailsOrFlag <- db.get(Keys.leaseDetails(leaseId)(h))
      details <- detailsOrFlag.fold(
        isActive =>
          transactionInfo(leaseId, db).collect {
            case (leaseHeight, lt: LeaseTransaction, _) =>
              LeaseDetails(lt.sender, lt.recipient, lt.amount, if (isActive) LeaseDetails.Status.Active
                else LeaseDetails.Status.Cancelled(h, None), leaseId, leaseHeight)
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
    .build[(Int, AddressId), java.lang.Long]()

  private val leaseBalanceAtHeightCache = CacheBuilder
    .newBuilder()
    .maximumSize(100000)
    .recordStats()
    .build[(Int, AddressId), LeaseBalance]()

  override def balanceAtHeight(address: Address, height: Int, assetId: Asset = Waves): Option[(Int, Long)] = readOnly { db =>
    db.get(Keys.addressId(address)).flatMap { addressId =>
      assetId match {
        case Waves =>
          closest(db.get(Keys.wavesBalanceHistory(addressId)), height).map { wh =>
            val b: Long = db.get(Keys.wavesBalance(addressId)(wh))
            (wh, b)
          }
        case asset @ IssuedAsset(_) =>
          closest(db.get(Keys.assetBalanceHistory(addressId, asset)), height).map { wh =>
            val b: Long = db.get(Keys.assetBalance(addressId, asset)(wh))
            (wh, b)
          }
      }
    }
  }

  override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] = readOnly { db =>
    db.get(Keys.addressId(address)).fold(Seq(BalanceSnapshot(1, 0, 0, 0))) { addressId =>
      val toHeigth = to.flatMap(this.heightOf).getOrElse(this.height)
      val wbh      = slice(db.get(Keys.wavesBalanceHistory(addressId)), from, toHeigth)
      val lbh      = slice(db.get(Keys.leaseBalanceHistory(addressId)), from, toHeigth)
      for {
        (wh, lh) <- merge(wbh, lbh)
        wb = balanceAtHeightCache.get((wh, addressId), () => db.get(Keys.wavesBalance(addressId)(wh)))
        lb = leaseBalanceAtHeightCache.get((lh, addressId), () => db.get(Keys.leaseBalance(addressId)(lh)))
      } yield BalanceSnapshot(wh.max(lh), wb, lb.in, lb.out)
    }
  }

  def loadScoreOf(blockId: ByteStr): Option[BigInt] = {
    readOnly(db => db.get(Keys.heightOf(blockId)).map(h => db.get(Keys.score(h))))
  }

  override def loadBlockInfo(height: Int): Option[SignedBlockHeader] = {
    writableDB.get(Keys.blockMetaAt(Height(height))).map(_.toSignedHeader)
  }

  def loadBlockInfo(height: Int, db: ReadOnlyDB): Option[SignedBlockHeader] = {
    db.get(Keys.blockMetaAt(Height(height))).map(_.toSignedHeader)
  }

  override def loadHeightOf(blockId: ByteStr): Option[Int] = {
    readOnly(_.get(Keys.heightOf(blockId)))
  }

  override def featureVotes(height: Int): Map[Short, Int] = readOnly { db =>
    settings.functionalitySettings
      .activationWindow(height)
      .flatMap { h =>
        val height = Height(h)
        db.get(Keys.blockMetaAt(height))
          .map(_.header.featureVotes)
          .getOrElse(Seq.empty)
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
              .map(_.header.rewardVote)
          }
      case _ => Seq()
    }
  }

  override def hitSource(height: Int): Option[ByteStr] = readOnly { db =>
    db.get(Keys.hitSource(height))
      .filter(_.arr.length == Block.HitSourceLength)
  }

  def loadStateHash(height: Int): Option[StateHash] = readOnly { db =>
    db.get(Keys.stateHash(height))
  }

  private def transactionsAtHeight(h: Height): List[(TxNum, Transaction)] = readOnly { db =>
    import com.wavesplatform.protobuf.transaction.PBSignedTransaction

    val txs = new ListBuffer[(TxNum, Transaction)]()

    val prefix = ByteBuffer
      .allocate(6)
      .put(KeyTags.NthTransactionInfoAtHeight.prefixBytes)
      .putInt(h)
      .array()

    db.iterateOver(prefix) { entry =>
      val k = entry.getKey

      for {
        idx <- Try(Shorts.fromByteArray(k.slice(6, 8)))
        tx = readTransactionBytes(entry.getValue) match {
          case (_, Left(legacyBytes)) => TransactionParsers.parseBytes(legacyBytes).get
          case (_, Right(newBytes))   => PBTransactions.vanilla(PBSignedTransaction.parseFrom(newBytes)).explicitGet()
        }
      } txs.append((TxNum(idx), tx))
    }

    txs.toList
  }
}
