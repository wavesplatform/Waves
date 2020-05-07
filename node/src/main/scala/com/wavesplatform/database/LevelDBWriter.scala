package com.wavesplatform.database

import java.nio.ByteBuffer

import cats.data.Ior
import cats.implicits._
import com.google.common.cache.CacheBuilder
import com.google.common.primitives.Shorts
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.database
import com.wavesplatform.database.patch.DisableHijackedAliases
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.settings.{BlockchainSettings, Constants, DBSettings, WavesSettings}
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{TxNum, _}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.{AliasDoesNotExist, AliasIsDisabled}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.ScorexLogging
import monix.reactive.Observer
import org.iq80.leveldb.DB

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Try
import scala.util.control.NonFatal

object LevelDBWriter extends ScorexLogging {

  private def loadLeaseStatus(db: ReadOnlyDB, leaseId: ByteStr): Boolean =
    db.get(Keys.leaseStatusHistory(leaseId)).headOption.fold(false)(h => db.get(Keys.leaseStatus(leaseId)(h)))

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
    v.takeWhile(_ <= h).lastOption // Should we use binary search?
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

    recMergeFixed(wbh.head, wbh.tail, lbh.head, lbh.tail, ArrayBuffer.empty)
  }

  def apply(db: DB, spendableBalanceChanged: Observer[(Address, Asset)], settings: WavesSettings): LevelDBWriter with AutoCloseable = {
    val expectedHeight = loadHeight(db)
    def load(name: String, key: KeyTags.KeyTag): BloomFilterImpl =
      BloomFilter.loadOrPopulate(db, settings.dbSettings.directory, name, expectedHeight, key, 100)

    val _orderFilter        = load("orders", KeyTags.FilledVolumeAndFeeHistory)
    val _dataKeyFilter      = load("account-data", KeyTags.DataHistory)
    val _wavesBalanceFilter = load("waves-balances", KeyTags.WavesBalanceHistory)
    val _assetBalanceFilter = load("asset-balances", KeyTags.AssetBalanceHistory)
    new LevelDBWriter(db, spendableBalanceChanged, settings.blockchainSettings, settings.dbSettings) with AutoCloseable {

      override val orderFilter: BloomFilter        = _orderFilter
      override val dataKeyFilter: BloomFilter      = _dataKeyFilter
      override val wavesBalanceFilter: BloomFilter = _wavesBalanceFilter
      override val assetBalanceFilter: BloomFilter = _assetBalanceFilter

      override def close(): Unit = {
        log.debug("Shutting down LevelDBWriter")
        val lastHeight = LevelDBWriter.loadHeight(db)
        _orderFilter.save(lastHeight)
        _dataKeyFilter.save(lastHeight)
        _wavesBalanceFilter.save(lastHeight)
        _assetBalanceFilter.save(lastHeight)
      }
    }
  }

  def readOnly(db: DB, settings: WavesSettings): LevelDBWriter = {
    val expectedHeight = loadHeight(db)
    def loadFilter(filterName: String) =
      BloomFilter
        .tryLoad(db, filterName, settings.dbSettings.directory, expectedHeight)
        .fold(_ => BloomFilter.AlwaysEmpty, gf => new Wrapper(gf))

    new LevelDBWriter(db, Observer.stopped, settings.blockchainSettings, settings.dbSettings) {
      override val orderFilter: BloomFilter        = loadFilter("orders")
      override val dataKeyFilter: BloomFilter      = loadFilter("account-data")
      override val wavesBalanceFilter: BloomFilter = loadFilter("waves-balances")
      override val assetBalanceFilter: BloomFilter = loadFilter("asset-balances")
    }
  }
}

abstract class LevelDBWriter private[database] (
    writableDB: DB,
    spendableBalanceChanged: Observer[(Address, Asset)],
    val settings: BlockchainSettings,
    val dbSettings: DBSettings
) extends Caches(spendableBalanceChanged)
    with ScorexLogging {

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

  override protected def safeRollbackHeight: Int = readOnly(_.get(Keys.safeRollbackHeight))

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

  override protected def loadAssetScript(asset: IssuedAsset): Option[(Script, Long)] = readOnly { db =>
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

  private def loadWavesBalanceHistory(db: ReadOnlyDB, addressId: AddressId): Seq[Int] = {
    val kwbh = Keys.wavesBalanceHistory(addressId)
    if (wavesBalanceFilter.mightContain(kwbh.suffix)) db.get(kwbh) else Seq.empty
  }

  private def loadAssetBalanceHistory(db: ReadOnlyDB, addressId: AddressId, assetId: IssuedAsset): Seq[Int] = {
    val kabh = Keys.assetBalanceHistory(addressId, assetId)
    if (assetBalanceFilter.mightContain(kabh.suffix)) db.get(kabh) else Seq.empty
  }

  protected override def loadBalance(req: (Address, Asset)): Long = readOnly { db =>
    addressId(req._1).fold(0L) { addressId =>
      req._2 match {
        case asset @ IssuedAsset(_) =>
          loadAssetBalanceHistory(db, addressId, asset).headOption.map(h => db.get(Keys.assetBalance(addressId, asset)(h))).getOrElse(0L)
        case Waves =>
          loadWavesBalanceHistory(db, addressId).headOption.map(h => db.get(Keys.wavesBalance(addressId)(h))).getOrElse(0L)
      }
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
    if (db.has(Keys.wavesAmount(height))) db.get(Keys.wavesAmount(height))
    else BigInt(Constants.UnitsInWave * Constants.TotalWaves)
  }

  override def blockReward(height: Int): Option[Long] =
    readOnly(_.db.get(Keys.blockReward(height)))

  private def updateHistory(rw: RW, key: Key[Seq[Int]], threshold: Int, kf: Int => Key[_]): Seq[Array[Byte]] =
    updateHistory(rw, rw.get(key), key, threshold, kf)

  private def updateHistory(rw: RW, history: Seq[Int], key: Key[Seq[Int]], threshold: Int, kf: Int => Key[_]): Seq[Array[Byte]] = {
    val (c1, c2) = history.partition(_ > threshold)
    rw.put(key, (height +: c1) ++ c2.headOption)
    c2.drop(1).map(kf(_).keyBytes)
  }

  private def appendBalances(
      balances: Map[AddressId, Map[Asset, Long]],
      issuedAssets: Map[IssuedAsset, (AssetStaticInfo, AssetInfo, AssetVolumeInfo)],
      rw: RW,
      threshold: Int,
      balanceThreshold: Int
  ): Unit = {
    // balances
    val updatedNftLists = mutable.LongMap.empty[Seq[IssuedAsset]].withDefaultValue(Seq.empty)

    for ((addressId, updatedBalances) <- balances) {
      for ((asset, balance) <- updatedBalances) {
        asset match {
          case Waves =>
            val kwbh = Keys.wavesBalanceHistory(addressId)
            val wbh  = loadWavesBalanceHistory(rw, addressId)
            if (wbh.isEmpty) wavesBalanceFilter.put(kwbh.suffix)
            rw.put(Keys.wavesBalance(addressId)(height), balance)
            updateHistory(rw, wbh, kwbh, balanceThreshold, Keys.wavesBalance(addressId)).foreach(rw.delete)
          case a: IssuedAsset =>
            rw.put(Keys.assetBalance(addressId, a)(height), balance)
            val kBalanceHistory = Keys.assetBalanceHistory(addressId, a)
            val balanceHistory  = loadAssetBalanceHistory(rw, addressId, a)
            if (balanceHistory.isEmpty) assetBalanceFilter.put(kBalanceHistory.suffix)
            updateHistory(rw, balanceHistory, kBalanceHistory, threshold, Keys.assetBalance(addressId, a)).foreach(rw.delete)
            if (balance > 0 && balanceHistory.isEmpty && issuedAssets
                  .get(a)
                  .map(_._1.nft)
                  .orElse(assetDescription(a).map(_.nft))
                  .getOrElse(false)) {
              updatedNftLists += addressId.toLong -> (a +: updatedNftLists(addressId.toLong))
            }
        }
      }
    }

    for ((addressIdL, nftIds) <- updatedNftLists) {
      val addressId        = AddressId(addressIdL)
      val kCount           = Keys.nftCount(addressId)
      val previousNftCount = rw.get(kCount)
      rw.put(kCount, previousNftCount + nftIds.length)
      for ((id, idx) <- nftIds.zipWithIndex) {
        rw.put(Keys.nftAt(addressId, idx, id), Some(()))
      }
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
      leaseStates: Map[ByteStr, Boolean],
      issuedAssets: Map[IssuedAsset, (AssetStaticInfo, AssetInfo, AssetVolumeInfo)],
      updatedAssets: Map[IssuedAsset, Ior[AssetInfo, AssetVolumeInfo]],
      filledQuantity: Map[ByteStr, VolumeAndFee],
      scripts: Map[AddressId, Option[AccountScriptInfo]],
      assetScripts: Map[IssuedAsset, Option[(Script, Long)]],
      data: Map[Address, AccountDataInfo],
      aliases: Map[Alias, AddressId],
      sponsorship: Map[IssuedAsset, Sponsorship],
      totalFee: Long,
      reward: Option[Long],
      hitSource: ByteStr,
      scriptResults: Map[ByteStr, InvokeScriptResult],
      failedTransactionIds: Set[ByteStr]
  ): Unit = {
    log.trace(s"Persisting block ${block.id()} at height $height")
    readWrite { rw =>
      val expiredKeys = new ArrayBuffer[Array[Byte]]

      rw.put(Keys.height, height)

      val previousSafeRollbackHeight = rw.get(Keys.safeRollbackHeight)

      if (previousSafeRollbackHeight < (height - dbSettings.maxRollbackDepth)) {
        rw.put(Keys.safeRollbackHeight, height - dbSettings.maxRollbackDepth)
      }

      val transactions: Map[TransactionId, (Transaction, TxNum)] =
        block.transactionData.zipWithIndex.map { in =>
          val (tx, idx) = in
          val k         = TransactionId(tx.id())
          val v         = (tx, TxNum(idx.toShort))
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

      val threshold        = height - dbSettings.maxRollbackDepth
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

      for ((asset, (staticInfo, info, volumeInfo)) <- issuedAssets) {
        rw.put(Keys.assetStaticInfo(asset), staticInfo.some)
        rw.put(Keys.assetDetails(asset)(height), (info, volumeInfo))
      }

      for ((asset, infoToUpdate) <- updatedAssets) {
        rw.fromHistory(Keys.assetDetailsHistory(asset), Keys.assetDetails(asset))
          .orElse(issuedAssets.get(asset).map { case (_, i, vi) => (i, vi) })
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

      for ((leaseId, state) <- leaseStates) {
        rw.put(Keys.leaseStatus(leaseId)(height), state)
        expiredKeys ++= updateHistory(rw, Keys.leaseStatusHistory(leaseId), threshold, Keys.leaseStatus(leaseId))
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
          val (tx, num) = transactions(txId)

          (tx.typeId, num)
        }
        rw.put(Keys.addressTransactionHN(addressId, nextSeqNr), Some((Height(height), txTypeNumSeq.sortBy(-_._2))))
        rw.put(kk, nextSeqNr)
      }

      for ((alias, addressId) <- aliases) {
        rw.put(Keys.addressIdOfAlias(alias), Some(addressId))
      }

      for ((id, (tx, num)) <- transactions) {
        rw.put(Keys.transactionAt(Height(height), num), Some((tx, !failedTransactionIds.contains(id))))
        rw.put(Keys.transactionHNById(id), Some((Height(height), num)))
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

          val featuresToSave = newlyApprovedFeatures.mapValues(_ + activationWindowSize) ++ rw.get(Keys.activatedFeatures)

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

      rw.put(Keys.carryFee(height), carry)
      expiredKeys += Keys.carryFee(threshold - 1).keyBytes

      rw.put(Keys.blockTransactionsFee(height), totalFee)

      if (dbSettings.storeInvokeScriptResults) scriptResults.foreach {
        case (txId, result) =>
          val (txHeight, txNum) = transactions
            .get(TransactionId(txId))
            .map { case (_, txNum) => (height, txNum) }
            .orElse(rw.get(Keys.transactionHNById(TransactionId(txId))))
            .getOrElse(throw new IllegalArgumentException(s"Couldn't find transaction height and num: $txId"))

          try rw.put(Keys.invokeScriptResult(txHeight, txNum), Some(result))
          catch {
            case NonFatal(e) =>
              throw new RuntimeException(s"Error storing invoke script result for $txId: $result", e)
          }
      }

      expiredKeys.foreach(rw.delete(_, "expired-keys"))

      if (activatedFeatures.get(BlockchainFeatures.DataTransaction.id).contains(height)) {
        disabledAliases = DisableHijackedAliases(rw)
      }

      rw.put(Keys.hitSource(height), Some(hitSource))
    }

    log.trace(s"Finished persisting block ${block.id()} at height $height")
  }

  override protected def doRollback(targetBlockId: ByteStr): Seq[(Block, ByteStr)] = {
    readOnly(_.get(Keys.heightOf(targetBlockId))).fold(Seq.empty[(Block, ByteStr)]) { targetHeight =>
      log.debug(s"Rolling back to block $targetBlockId at $targetHeight")

      val discardedBlocks: Seq[(Block, ByteStr)] = for (currentHeight <- height until targetHeight by -1) yield {
        val balancesToInvalidate     = Seq.newBuilder[(Address, Asset)]
        val assetDetailsToInvalidate = Seq.newBuilder[IssuedAsset]
        val ordersToInvalidate       = Seq.newBuilder[ByteStr]
        val scriptsToDiscard         = Seq.newBuilder[Address]
        val assetScriptsToDiscard    = Seq.newBuilder[IssuedAsset]
        val accountDataToInvalidate  = Seq.newBuilder[(Address, String)]

        val h = Height(currentHeight)

        val discardedBlock = readWrite { rw =>
          log.trace(s"Rolling back to ${currentHeight - 1}")
          rw.put(Keys.height, currentHeight - 1)

          val discardedMeta = rw
            .get(Keys.blockMetaAt(h))
            .getOrElse(throw new IllegalArgumentException(s"No block at height $currentHeight"))

          rw.delete(Keys.blockMetaAt(h))

          for (addressId <- rw.get(Keys.changedAddresses(currentHeight))) {
            val address = rw.get(Keys.idToAddress(addressId))

            rw.iterateOver(KeyTags.AssetBalanceHistory.prefixBytes ++ addressId.toByteArray) { e =>
              val assetId = IssuedAsset(ByteStr(e.getKey.takeRight(32)))
              val history = readIntSeq(e.getValue)
              if (history.nonEmpty && history.head == currentHeight) {
                balancesToInvalidate += address -> assetId
                rw.delete(Keys.assetBalance(addressId, assetId)(history.head))
                rw.put(e.getKey, writeIntSeq(history.tail))
              }
            }

            for (k <- rw.get(Keys.changedDataKeys(currentHeight, addressId))) {
              log.trace(s"Discarding $k for $address at $currentHeight")
              accountDataToInvalidate += (address -> k)
              rw.delete(Keys.data(addressId, k)(currentHeight))
              rw.filterHistory(Keys.dataHistory(address, k), currentHeight)
            }

            balancesToInvalidate += (address -> Waves)
            rw.delete(Keys.wavesBalance(addressId)(currentHeight))
            rw.filterHistory(Keys.wavesBalanceHistory(addressId), currentHeight)

            rw.delete(Keys.leaseBalance(addressId)(currentHeight))
            rw.filterHistory(Keys.leaseBalanceHistory(addressId), currentHeight)

            log.trace(s"Discarding portfolio for $address")

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

          val transactions = transactionsAtHeight(h)

          transactions.foreach {
            case (num, tx) =>
              forgetTransaction(tx.id())
              tx match {
                case _: GenesisTransaction                                                       => // genesis transaction can not be rolled back
                case _: PaymentTransaction | _: TransferTransaction | _: MassTransferTransaction =>
                // balances already restored

                case tx: IssueTransaction =>
                  rw.delete(Keys.assetStaticInfo(IssuedAsset(tx.id())))
                  assetDetailsToInvalidate += rollbackAssetInfo(rw, IssuedAsset(tx.id()), currentHeight)
                case tx: UpdateAssetInfoTransaction =>
                  assetDetailsToInvalidate += rollbackAssetInfo(rw, tx.assetId, currentHeight)
                case tx: ReissueTransaction =>
                  assetDetailsToInvalidate += rollbackAssetInfo(rw, tx.asset, currentHeight)
                case tx: BurnTransaction =>
                  assetDetailsToInvalidate += rollbackAssetInfo(rw, tx.asset, currentHeight)
                case tx: SponsorFeeTransaction =>
                  assetDetailsToInvalidate += rollbackSponsorship(rw, tx.asset, currentHeight)
                case tx: LeaseTransaction =>
                  rollbackLeaseStatus(rw, tx.id(), currentHeight)
                case tx: LeaseCancelTransaction =>
                  rollbackLeaseStatus(rw, tx.leaseId, currentHeight)

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
                rw.delete(Keys.transactionHNById(TransactionId(tx.id())))
              }
          }

          rw.delete(Keys.blockMetaAt(h))
          rw.delete(Keys.heightOf(discardedMeta.id))
          rw.delete(Keys.carryFee(currentHeight))
          rw.delete(Keys.blockTransactionsFee(currentHeight))
          rw.delete(Keys.blockReward(currentHeight))
          rw.delete(Keys.wavesAmount(currentHeight))

          if (activatedFeatures.get(BlockchainFeatures.DataTransaction.id).contains(currentHeight)) {
            DisableHijackedAliases.revert(rw)
            disabledAliases = Set.empty
          }

          val hitSource = rw.get(Keys.hitSource(currentHeight)).get
          val block     = createBlock(discardedMeta.header, discardedMeta.signature, transactions.map(_._2)).explicitGet()

          (block, hitSource)
        }

        balancesToInvalidate.result().foreach(discardBalance)
        assetDetailsToInvalidate.result().foreach(discardAssetDescription)
        ordersToInvalidate.result().foreach(discardVolumeAndFee)
        scriptsToDiscard.result().foreach(discardScript)
        assetScriptsToDiscard.result().foreach(discardAssetScript)
        accountDataToInvalidate.result().foreach(discardAccountData)
        discardedBlock
      }

      log.debug(s"Rollback to block $targetBlockId at $targetHeight completed")

      discardedBlocks.reverse
    }
  }

  private def rollbackAssetInfo(rw: RW, asset: IssuedAsset, currentHeight: Int): IssuedAsset = {
    rw.delete(Keys.assetDetails(asset)(currentHeight))
    rw.filterHistory(Keys.assetDetailsHistory(asset), currentHeight)
    asset
  }

  private def rollbackOrderFill(rw: RW, orderId: ByteStr, currentHeight: Int): ByteStr = {
    rw.delete(Keys.filledVolumeAndFee(orderId)(currentHeight))
    rw.filterHistory(Keys.filledVolumeAndFeeHistory(orderId), currentHeight)
    orderId
  }

  private def rollbackLeaseStatus(rw: RW, leaseId: ByteStr, currentHeight: Int): Unit = {
    rw.delete(Keys.leaseStatus(leaseId)(currentHeight))
    rw.filterHistory(Keys.leaseStatusHistory(leaseId), currentHeight)
  }

  private def rollbackSponsorship(rw: RW, asset: IssuedAsset, currentHeight: Int): IssuedAsset = {
    rw.delete(Keys.sponsorship(asset)(currentHeight))
    rw.filterHistory(Keys.sponsorshipHistory(asset), currentHeight)
    asset
  }

  override def transferById(id: ByteStr): Option[(Int, TransferTransaction)] = readOnly { db =>
    for {
      (height, num) <- db.get(Keys.transactionHNById(TransactionId @@ id))
      tx  <- db.get(Keys.transferTransactionAt(height, num))
    } yield (height, tx)
  }

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction, Boolean)] = readOnly(transactionInfo(id, _))

  protected def transactionInfo(id: ByteStr, db: ReadOnlyDB): Option[(Int, Transaction, Boolean)] = {
    val txId = TransactionId(id)
    for {
      (height, num) <- db.get(Keys.transactionHNById(txId))
      (tx, status)  <- db.get(Keys.transactionAt(height, num))
    } yield (height, tx, status)
  }

  override def transactionHeight(id: ByteStr): Option[Int] = readOnly { db =>
    db.get(Keys.transactionHNById(TransactionId(id))).map(_._1)
  }

  override def resolveAlias(alias: Alias): Either[ValidationError, Address] = readOnly { db =>
    if (disabledAliases.contains(alias)) Left(AliasIsDisabled(alias))
    else
      db.get(Keys.addressIdOfAlias(alias))
        .map(addressId => db.get(Keys.idToAddress(addressId)))
        .toRight(AliasDoesNotExist(alias))
  }

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = readOnly { db =>
    transactionInfo(leaseId, db) match {
      case Some((h, lt: LeaseTransaction, true)) =>
        Some(LeaseDetails(lt.sender, lt.recipient, h, lt.amount, loadLeaseStatus(db, leaseId)))
      case _ => None
    }
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

  override def balanceOnlySnapshots(address: Address, height: Int, assetId: Asset = Waves): Option[(Int, Long)] = readOnly { db =>
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

  override def collectActiveLeases(filter: LeaseTransaction => Boolean): Seq[LeaseTransaction] = readOnly { db =>
    val activeLeaseIds = mutable.Set.empty[ByteStr]
    db.iterateOver(KeyTags.LeaseStatus) { e =>
      val leaseId = e.getKey.slice(2, e.getKey.length - 4)
      if (e.getValue.headOption.contains(1.toByte)) {
        activeLeaseIds += ByteStr(leaseId)
      } else {
        activeLeaseIds -= ByteStr(leaseId)
      }
    }

    val activeLeaseTransactions = for {
      leaseId <- activeLeaseIds
      (h, n)  <- db.get(Keys.transactionHNById(TransactionId(leaseId)))
      tx      <- db.get(Keys.transactionAt(h, n)).collect { case (lt: LeaseTransaction, true) if filter(lt) => lt }
    } yield tx

    activeLeaseTransactions.toSeq
  }

  override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] = readOnly { db =>
    val b = Map.newBuilder[Address, A]
    for (id <- 1L to db.get(Keys.lastAddressId).getOrElse(0L)) {
      val addressId = AddressId(id)
      val address   = db.get(Keys.idToAddress(addressId))
      pf.runWith(b += address -> _)(address -> loadLposPortfolio(db, addressId))
    }
    b.result()
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
          .map(_.header.featureVotes.toSeq)
          .getOrElse(Seq.empty)
      }
      .groupBy(identity)
      .mapValues(_.size)
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
