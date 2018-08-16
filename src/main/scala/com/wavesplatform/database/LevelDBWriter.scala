package com.wavesplatform.database

import cats.kernel.Monoid
import com.google.common.cache.CacheBuilder
import com.wavesplatform.database.patch.DisableHijackedAliases
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.state.reader.LeaseDetails
import org.iq80.leveldb.{DB, ReadOptions}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.transaction.Transaction.Type
import com.wavesplatform.transaction.ValidationError.{AliasDoesNotExist, AliasIsDisabled}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.transfer._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}

object LevelDBWriter {
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

  /** {{{([15, 12, 3], [12, 5]) => [(15, 12), (12, 12), (3, 12), (3, 5)]}}}
    *
    * @param wbh WAVES balance history
    * @param lbh Lease balance history
    */
  private[database] def merge(wbh: Seq[Int], lbh: Seq[Int]): Seq[(Int, Int)] = {
    @tailrec
    def recMerge(wh: Int, wt: Seq[Int], lh: Int, lt: Seq[Int], buf: ArrayBuffer[(Int, Int)]): ArrayBuffer[(Int, Int)] = {
      buf += wh -> lh
      if (wt.isEmpty && lt.isEmpty) {
        buf
      } else if (wt.isEmpty) {
        recMerge(wh, wt, lt.head, lt.tail, buf)
      } else if (lt.isEmpty) {
        recMerge(wt.head, wt.tail, lh, lt, buf)
      } else {
        if (wh >= lh) {
          recMerge(wt.head, wt.tail, lh, lt, buf)
        } else {
          recMerge(wh, wt, lt.head, lt.tail, buf)
        }
      }
    }

    recMerge(wbh.head, wbh.tail, lbh.head, lbh.tail, ArrayBuffer.empty)
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
}

class LevelDBWriter(writableDB: DB, fs: FunctionalitySettings, val maxCacheSize: Int = 100000) extends Caches with ScorexLogging {

  import LevelDBWriter._

  private def readOnly[A](f: ReadOnlyDB => A): A = {
    val s = writableDB.getSnapshot
    try f(new ReadOnlyDB(writableDB, new ReadOptions().snapshot(s)))
    finally s.close()
  }

  private def readWrite[A](f: RW => A): A = {
    val rw = new RW(writableDB)
    try f(rw)
    finally rw.close()
  }

  override protected def loadMaxAddressId(): BigInt = readOnly(db => db.get(Keys.lastAddressId).getOrElse(BigInt(0)))

  override protected def loadAddressId(address: Address): Option[BigInt] = readOnly(db => db.get(Keys.addressId(address)))

  override protected def loadHeight(): Int = readOnly(_.get(Keys.height))

  override protected def loadScore(): BigInt = readOnly(db => db.get(Keys.score(db.get(Keys.height))))

  override protected def loadLastBlock(): Option[Block] = readOnly(db => db.get(Keys.blockAt(db.get(Keys.height))))

  override protected def loadScript(address: Address): Option[Script] = readOnly { db =>
    addressId(address).fold(Option.empty[Script]) { addressId =>
      db.fromHistory(Keys.addressScriptHistory(addressId), Keys.addressScript(addressId)).flatten
    }
  }

  override protected def hasScriptBytes(address: Address): Boolean = readOnly { db =>
    addressId(address).fold(false) { addressId =>
      db.hasInHistory(Keys.addressScriptHistory(addressId), Keys.addressScript(addressId))
    }
  }

  override def accountData(address: Address): AccountDataInfo = readOnly { db =>
    AccountDataInfo((for {
      addressId <- addressId(address).toSeq
      keyChunkCount = db.get(Keys.dataKeyChunkCount(addressId))
      chunkNo <- Range(0, keyChunkCount)
      key     <- db.get(Keys.dataKeyChunk(addressId, chunkNo))
      value   <- accountData(address, key)
    } yield key -> value).toMap)
  }

  override def accountData(address: Address, key: String): Option[DataEntry[_]] = readOnly { db =>
    addressId(address).fold(Option.empty[DataEntry[_]]) { addressId =>
      db.fromHistory(Keys.dataHistory(addressId, key), Keys.data(addressId, key)).flatten
    }
  }

  override def balance(address: Address, mayBeAssetId: Option[AssetId]): Long = readOnly { db =>
    addressId(address).fold(0L) { addressId =>
      mayBeAssetId match {
        case Some(assetId) => db.fromHistory(Keys.assetBalanceHistory(addressId, assetId), Keys.assetBalance(addressId, assetId)).getOrElse(0L)
        case None          => db.fromHistory(Keys.wavesBalanceHistory(addressId), Keys.wavesBalance(addressId)).getOrElse(0L)
      }
    }
  }

  private def loadLposPortfolio(db: ReadOnlyDB, addressId: BigInt) = Portfolio(
    db.fromHistory(Keys.wavesBalanceHistory(addressId), Keys.wavesBalance(addressId)).getOrElse(0L),
    db.fromHistory(Keys.leaseBalanceHistory(addressId), Keys.leaseBalance(addressId)).getOrElse(LeaseBalance.empty),
    Map.empty
  )

  private def loadPortfolio(db: ReadOnlyDB, addressId: BigInt) = loadLposPortfolio(db, addressId).copy(
    assets = (for {
      assetId <- db.get(Keys.assetList(addressId))
    } yield assetId -> db.fromHistory(Keys.assetBalanceHistory(addressId, assetId), Keys.assetBalance(addressId, assetId)).getOrElse(0L)).toMap
  )

  override protected def loadPortfolio(address: Address): Portfolio = readOnly { db =>
    addressId(address).fold(Portfolio.empty)(loadPortfolio(db, _))
  }

  override protected def loadAssetDescription(assetId: ByteStr): Option[AssetDescription] = readOnly { db =>
    db.get(Keys.transactionInfo(assetId)) match {
      case Some((_, i: IssueTransaction)) =>
        val ai          = db.fromHistory(Keys.assetInfoHistory(assetId), Keys.assetInfo(assetId)).getOrElse(AssetInfo(i.reissuable, i.quantity, i.script))
        val sponsorship = db.fromHistory(Keys.sponsorshipHistory(assetId), Keys.sponsorship(assetId)).fold(0L)(_.minFee)
        Some(AssetDescription(i.sender, i.name, i.description, i.decimals, ai.isReissuable, ai.volume, ai.script, sponsorship))
      case _ => None
    }
  }

  override protected def loadVolumeAndFee(orderId: ByteStr): VolumeAndFee = readOnly { db =>
    db.fromHistory(Keys.filledVolumeAndFeeHistory(orderId), Keys.filledVolumeAndFee(orderId)).getOrElse(VolumeAndFee.empty)
  }

  override protected def loadApprovedFeatures(): Map[Short, Int] = readOnly(_.get(Keys.approvedFeatures))

  override protected def loadActivatedFeatures(): Map[Short, Int] = fs.preActivatedFeatures ++ readOnly(_.get(Keys.activatedFeatures))

  private def updateHistory(rw: RW, key: Key[Seq[Int]], threshold: Int, kf: Int => Key[_]): Seq[Array[Byte]] =
    updateHistory(rw, rw.get(key), key, threshold, kf)

  private def updateHistory(rw: RW, history: Seq[Int], key: Key[Seq[Int]], threshold: Int, kf: Int => Key[_]): Seq[Array[Byte]] = {
    val (c1, c2) = history.partition(_ > threshold)
    rw.put(key, (height +: c1) ++ c2.headOption)
    c2.drop(1).map(kf(_).keyBytes)
  }

  override protected def doAppend(block: Block,
                                  newAddresses: Map[Address, BigInt],
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
                                  sponsorship: Map[AssetId, Sponsorship]): Unit = readWrite { rw =>
    val expiredKeys = new ArrayBuffer[Array[Byte]]

    rw.put(Keys.height, height)
    rw.put(Keys.blockAt(height), Some(block))
    rw.put(Keys.heightOf(block.uniqueId), Some(height))
    rw.put(Keys.lastAddressId, Some(loadMaxAddressId() + newAddresses.size))
    rw.put(Keys.score(height), rw.get(Keys.score(height - 1)) + block.blockScore())

    for ((address, id) <- newAddresses) {
      rw.put(Keys.addressId(address), Some(id))
      rw.put(Keys.idToAddress(id), address)
    }

    val threshold = height - 2000

    val newAddressesForWaves = ArrayBuffer.empty[BigInt]
    val updatedBalanceAddresses = for ((addressId, balance) <- wavesBalances) yield {
      val kwbh = Keys.wavesBalanceHistory(addressId)
      val wbh  = rw.get(kwbh)
      if (wbh.isEmpty) {
        newAddressesForWaves += addressId
      }
      rw.put(Keys.wavesBalance(addressId)(height), balance)
      expiredKeys ++= updateHistory(rw, wbh, kwbh, threshold, Keys.wavesBalance(addressId))
      addressId
    }

    val changedAddresses = (addressTransactions.keys ++ updatedBalanceAddresses)

    if (newAddressesForWaves.nonEmpty) {
      val newSeqNr = rw.get(Keys.addressesForWavesSeqNr) + 1
      rw.put(Keys.addressesForWavesSeqNr, newSeqNr)
      rw.put(Keys.addressesForWaves(newSeqNr), newAddressesForWaves)
    }

    for ((addressId, leaseBalance) <- leaseBalances) {
      rw.put(Keys.leaseBalance(addressId)(height), leaseBalance)
      expiredKeys ++= updateHistory(rw, Keys.leaseBalanceHistory(addressId), threshold, Keys.leaseBalance(addressId))
    }

    val newAddressesForAsset = mutable.AnyRefMap.empty[ByteStr, Set[BigInt]]
    for ((addressId, assets) <- assetBalances) {
      val prevAssets = rw.get(Keys.assetList(addressId))
      val newAssets  = assets.keySet.diff(prevAssets)
      for (assetId <- newAssets) {
        newAddressesForAsset += assetId -> (newAddressesForAsset.getOrElse(assetId, Set.empty) + addressId)
      }
      rw.put(Keys.assetList(addressId), prevAssets ++ assets.keySet)
      for ((assetId, balance) <- assets) {
        rw.put(Keys.assetBalance(addressId, assetId)(height), balance)
        expiredKeys ++= updateHistory(rw, Keys.assetBalanceHistory(addressId, assetId), threshold, Keys.assetBalance(addressId, assetId))
      }
    }

    for ((assetId, newAddressIds) <- newAddressesForAsset) {
      val seqNrKey  = Keys.addressesForAssetSeqNr(assetId)
      val nextSeqNr = rw.get(seqNrKey) + 1
      val key       = Keys.addressesForAsset(assetId, nextSeqNr)

      rw.put(seqNrKey, nextSeqNr)
      rw.put(key, newAddressIds.toSeq)
    }

    rw.put(Keys.changedAddresses(height), changedAddresses.toSeq)

    for ((orderId, volumeAndFee) <- filledQuantity) {
      rw.put(Keys.filledVolumeAndFee(orderId)(height), volumeAndFee)
      expiredKeys ++= updateHistory(rw, Keys.filledVolumeAndFeeHistory(orderId), threshold, Keys.filledVolumeAndFee(orderId))
    }

    for ((assetId, assetInfo) <- reissuedAssets) {
      val combinedAssetInfo = rw.fromHistory(Keys.assetInfoHistory(assetId), Keys.assetInfo(assetId)).fold(assetInfo) { p =>
        Monoid.combine(p, assetInfo)
      }
      rw.put(Keys.assetInfo(assetId)(height), combinedAssetInfo)
      expiredKeys ++= updateHistory(rw, Keys.assetInfoHistory(assetId), threshold, Keys.assetInfo(assetId))
    }

    for ((leaseId, state) <- leaseStates) {
      rw.put(Keys.leaseStatus(leaseId)(height), state)
      expiredKeys ++= updateHistory(rw, Keys.leaseStatusHistory(leaseId), threshold, Keys.leaseStatus(leaseId))
    }

    for ((addressId, script) <- scripts) {
      expiredKeys ++= updateHistory(rw, Keys.addressScriptHistory(addressId), threshold, Keys.addressScript(addressId))
      script.foreach(s => rw.put(Keys.addressScript(addressId)(height), Some(s)))
    }

    for ((addressId, addressData) <- data) {
      val newKeys = (
        for {
          (key, value) <- addressData.data
          kdh   = Keys.dataHistory(addressId, key)
          isNew = rw.get(kdh).isEmpty
          _     = rw.put(Keys.data(addressId, key)(height), Some(value))
          _     = expiredKeys ++= updateHistory(rw, kdh, threshold, Keys.data(addressId, key))
          if isNew
        } yield key
      ).toSeq
      if (newKeys.nonEmpty) {
        val chunkCountKey = Keys.dataKeyChunkCount(addressId)
        val chunkCount    = rw.get(chunkCountKey)
        rw.put(Keys.dataKeyChunk(addressId, chunkCount), newKeys)
        rw.put(chunkCountKey, chunkCount + 1)
      }
    }

    for ((addressId, txs) <- addressTransactions) {
      val kk        = Keys.addressTransactionSeqNr(addressId)
      val nextSeqNr = rw.get(kk) + 1
      rw.put(Keys.addressTransactionIds(addressId, nextSeqNr), txs)
      rw.put(kk, nextSeqNr)
    }

    for ((alias, addressId) <- aliases) {
      rw.put(Keys.addressIdOfAlias(alias), Some(addressId))
    }

    for ((id, (tx, _)) <- transactions) {
      rw.put(Keys.transactionInfo(id), Some((height, tx)))
    }

    val activationWindowSize = fs.activationWindowSize(height)
    if (height % activationWindowSize == 0) {
      val minVotes = fs.blocksForFeatureActivation(height)
      val newlyApprovedFeatures = featureVotes(height).collect {
        case (featureId, voteCount) if voteCount + (if (block.featureVotes(featureId)) 1 else 0) >= minVotes => featureId -> height
      }

      if (newlyApprovedFeatures.nonEmpty) {
        approvedFeaturesCache = newlyApprovedFeatures ++ rw.get(Keys.approvedFeatures)
        rw.put(Keys.approvedFeatures, approvedFeaturesCache)

        val featuresToSave = newlyApprovedFeatures.mapValues(_ + activationWindowSize) ++ rw.get(Keys.activatedFeatures)

        activatedFeaturesCache = featuresToSave ++ fs.preActivatedFeatures
        rw.put(Keys.activatedFeatures, featuresToSave)
      }
    }

    for ((assetId, sp: SponsorshipValue) <- sponsorship) {
      rw.put(Keys.sponsorship(assetId)(height), sp)
      expiredKeys ++= updateHistory(rw, Keys.sponsorshipHistory(assetId), threshold, Keys.sponsorship(assetId))
    }

    rw.put(Keys.transactionIdsAtHeight(height), transactions.keys.toSeq)

    expiredKeys.foreach(rw.delete)

    if (activatedFeatures.get(BlockchainFeatures.DataTransaction.id).contains(height)) {
      DisableHijackedAliases(rw)
    }
  }

  override protected def doRollback(targetBlockId: ByteStr): Seq[Block] = {
    readOnly(_.get(Keys.heightOf(targetBlockId))).fold(Seq.empty[Block]) { targetHeight =>
      log.debug(s"Rolling back to block $targetBlockId at $targetHeight")

      val discardedBlocks: Seq[Block] = for (currentHeight <- height until targetHeight by -1) yield {
        val portfoliosToInvalidate = Seq.newBuilder[Address]
        val assetInfoToInvalidate  = Seq.newBuilder[ByteStr]
        val ordersToInvalidate     = Seq.newBuilder[ByteStr]
        val scriptsToDiscard       = Seq.newBuilder[Address]

        val discardedBlock = readWrite { rw =>
          log.trace(s"Rolling back to ${currentHeight - 1}")
          rw.put(Keys.height, currentHeight - 1)

          for (addressId <- rw.get(Keys.changedAddresses(currentHeight))) {
            val address = rw.get(Keys.idToAddress(addressId))

            for (assetId <- rw.get(Keys.assetList(addressId))) {
              rw.delete(Keys.assetBalance(addressId, assetId)(currentHeight))
              rw.filterHistory(Keys.assetBalanceHistory(addressId, assetId), currentHeight)
            }

            rw.delete(Keys.wavesBalance(addressId)(currentHeight))
            rw.filterHistory(Keys.wavesBalanceHistory(addressId), currentHeight)

            rw.delete(Keys.leaseBalance(addressId)(currentHeight))
            rw.filterHistory(Keys.leaseBalanceHistory(addressId), currentHeight)

            log.trace(s"Discarding portfolio for $address")

            portfoliosToInvalidate += address
            balanceAtHeightCache.invalidate((currentHeight, addressId))
            leaseBalanceAtHeightCache.invalidate((currentHeight, addressId))

            val kTxSeqNr = Keys.addressTransactionSeqNr(addressId)
            val txSeqNr  = rw.get(kTxSeqNr)
            val kTxIds   = Keys.addressTransactionIds(addressId, txSeqNr)
            for ((_, id) <- rw.get(kTxIds).headOption; (h, _) <- rw.get(Keys.transactionInfo(id)) if h == currentHeight) {
              rw.delete(kTxIds)
              rw.put(kTxSeqNr, (txSeqNr - 1).max(0))
            }
          }

          val txIdsAtHeight = Keys.transactionIdsAtHeight(currentHeight)
          for (txId <- rw.get(txIdsAtHeight)) {
            forgetTransaction(txId)
            val ktxId = Keys.transactionInfo(txId)

            for ((_, tx) <- rw.get(ktxId)) {
              tx match {
                case _: GenesisTransaction                                                       => // genesis transaction can not be rolled back
                case _: PaymentTransaction | _: TransferTransaction | _: MassTransferTransaction =>
                // balances already restored

                case tx: IssueTransaction =>
                  assetInfoToInvalidate += rollbackAssetInfo(rw, tx.id(), currentHeight)
                case tx: ReissueTransaction =>
                  assetInfoToInvalidate += rollbackAssetInfo(rw, tx.assetId, currentHeight)
                case tx: BurnTransaction =>
                  assetInfoToInvalidate += rollbackAssetInfo(rw, tx.assetId, currentHeight)
                case tx: SponsorFeeTransaction =>
                  assetInfoToInvalidate += rollbackSponsorship(rw, tx.assetId, currentHeight)
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

                case tx: DataTransaction =>
                  val address = tx.sender.toAddress
                  for (addressId <- addressId(address)) {
                    tx.data.foreach { e =>
                      log.trace(s"Discarding ${e.key} for $address at $currentHeight")
                      rw.delete(Keys.data(addressId, e.key)(currentHeight))
                      rw.filterHistory(Keys.dataHistory(addressId, e.key), currentHeight)
                    }
                  }

                case tx: CreateAliasTransaction => rw.delete(Keys.addressIdOfAlias(tx.alias))
                case tx: ExchangeTransaction =>
                  ordersToInvalidate += rollbackOrderFill(rw, ByteStr(tx.buyOrder.id()), currentHeight)
                  ordersToInvalidate += rollbackOrderFill(rw, ByteStr(tx.sellOrder.id()), currentHeight)
              }
            }

            rw.delete(ktxId)
          }

          rw.delete(txIdsAtHeight)

          val discardedBlock = rw
            .get(Keys.blockAt(currentHeight))
            .getOrElse(throw new IllegalArgumentException(s"No block at height $currentHeight"))

          rw.delete(Keys.blockAt(currentHeight))
          rw.delete(Keys.heightOf(discardedBlock.uniqueId))

          if (activatedFeatures.get(BlockchainFeatures.DataTransaction.id).contains(currentHeight)) {
            DisableHijackedAliases.revert(rw)
          }
          discardedBlock
        }

        portfoliosToInvalidate.result().foreach(discardPortfolio)
        assetInfoToInvalidate.result().foreach(discardAssetDescription)
        ordersToInvalidate.result().foreach(discardVolumeAndFee)
        scriptsToDiscard.result().foreach(discardScript)
        discardedBlock
      }

      log.debug(s"Rollback to block $targetBlockId at $targetHeight completed")

      discardedBlocks.reverse
    }
  }

  private def rollbackAssetInfo(rw: RW, assetId: ByteStr, currentHeight: Int): ByteStr = {
    rw.delete(Keys.assetInfo(assetId)(currentHeight))
    rw.filterHistory(Keys.assetInfoHistory(assetId), currentHeight)
    assetId
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

  private def rollbackSponsorship(rw: RW, assetId: ByteStr, currentHeight: Int): ByteStr = {
    rw.delete(Keys.sponsorship(assetId)(currentHeight))
    rw.filterHistory(Keys.sponsorshipHistory(assetId), currentHeight)
    assetId
  }

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] = readOnly(db => db.get(Keys.transactionInfo(id)))

  override def transactionHeight(id: ByteStr): Option[Int] = readOnly(db => db.get(Keys.transactionHeight(id)))

  override def addressTransactions(address: Address, types: Set[Type], count: Int, from: Int): Seq[(Int, Transaction)] = readOnly { db =>
    db.get(Keys.addressId(address)).fold(Seq.empty[(Int, Transaction)]) { addressId =>
      val txs = for {
        seqNr          <- (db.get(Keys.addressTransactionSeqNr(addressId)) to 1 by -1).view
        (txType, txId) <- db.get(Keys.addressTransactionIds(addressId, seqNr))
        if types.isEmpty || types.contains(txType.toByte)
        (h, tx) <- db.get(Keys.transactionInfo(txId))
      } yield (h, tx)

      txs.slice(from, count).force
    }
  }

  override def resolveAlias(alias: Alias): Either[ValidationError, Address] = readOnly { db =>
    if (db.get(Keys.aliasIsDisabled(alias))) Left(AliasIsDisabled(alias))
    else
      db.get(Keys.addressIdOfAlias(alias))
        .map(addressId => db.get(Keys.idToAddress(addressId)))
        .toRight(AliasDoesNotExist(alias))
  }

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = readOnly { db =>
    db.get(Keys.transactionInfo(leaseId)) match {
      case Some((h, lt: LeaseTransaction)) =>
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
    .build[(Int, BigInt), java.lang.Long]()

  private val leaseBalanceAtHeightCache = CacheBuilder
    .newBuilder()
    .maximumSize(100000)
    .recordStats()
    .build[(Int, BigInt), LeaseBalance]()

  override def balanceSnapshots(address: Address, from: Int, to: Int): Seq[BalanceSnapshot] = readOnly { db =>
    db.get(Keys.addressId(address)).fold(Seq(BalanceSnapshot(1, 0, 0, 0))) { addressId =>
      val wbh = slice(db.get(Keys.wavesBalanceHistory(addressId)), from, to)
      val lbh = slice(db.get(Keys.leaseBalanceHistory(addressId)), from, to)
      for {
        (wh, lh) <- merge(wbh, lbh)
        wb = balanceAtHeightCache.get((wh, addressId), () => db.get(Keys.wavesBalance(addressId)(wh)))
        lb = leaseBalanceAtHeightCache.get((lh, addressId), () => db.get(Keys.leaseBalance(addressId)(lh)))
      } yield BalanceSnapshot(wh.max(lh), wb, lb.in, lb.out)
    }
  }

  override def allActiveLeases: Set[LeaseTransaction] = readOnly { db =>
    val txs = for {
      h  <- 1 to db.get(Keys.height)
      id <- db.get(Keys.transactionIdsAtHeight(h))
      if loadLeaseStatus(db, id)
      (_, tx) <- db.get(Keys.transactionInfo(id))
    } yield tx

    txs.collect { case lt: LeaseTransaction => lt }.toSet
  }

  override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]) = readOnly { db =>
    val b = Map.newBuilder[Address, A]
    for (id <- BigInt(1) to db.get(Keys.lastAddressId).getOrElse(BigInt(0))) {
      val address = db.get(Keys.idToAddress(id))
      pf.runWith(b += address -> _)(address -> loadLposPortfolio(db, id))
    }
    b.result()
  }

  override def scoreOf(blockId: ByteStr): Option[BigInt] = readOnly(db => db.get(Keys.heightOf(blockId)).map(h => db.get(Keys.score(h))))

  override def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)] = readOnly(_.get(Keys.blockHeader(height)))

  override def blockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)] =
    readOnly(db => db.get(Keys.heightOf(blockId)).flatMap(h => db.get(Keys.blockHeader(h))))

  override def blockBytes(height: Int): Option[Array[Byte]] = readOnly(_.get(Keys.blockBytes(height)))

  override def blockBytes(blockId: ByteStr): Option[Array[Byte]] =
    readOnly(db => db.get(Keys.heightOf(blockId)).flatMap(h => db.get(Keys.blockBytes(h))))

  override def heightOf(blockId: ByteStr): Option[Int] = readOnly(_.get(Keys.heightOf(blockId)))

  override def lastBlockIds(howMany: Int): immutable.IndexedSeq[ByteStr] = readOnly { db =>
    // since this is called from outside of the main blockchain updater thread, instead of using cached height,
    // explicitly read height from storage to make this operation atomic.
    val currentHeight = db.get(Keys.height)
    (currentHeight until (currentHeight - howMany).max(0) by -1)
      .map(h => db.get(Keys.blockHeader(h)).get._1.signerData.signature)
  }

  override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]] = readOnly { db =>
    db.get(Keys.heightOf(parentSignature)).map { parentHeight =>
      (parentHeight until (parentHeight + howMany))
        .flatMap { h =>
          db.get(Keys.blockHeader(h))
        }
        .map { b =>
          b._1.signerData.signature
        }
    }
  }

  override def parent(block: Block, back: Int): Option[Block] = readOnly { db =>
    db.get(Keys.heightOf(block.reference)).flatMap(h => db.get(Keys.blockAt(h - back + 1)))
  }

  override def featureVotes(height: Int): Map[Short, Int] = readOnly { db =>
    fs.activationWindow(height)
      .flatMap(h => db.get(Keys.blockHeader(h)).fold(Seq.empty[Short])(_._1.featureVotes.toSeq))
      .groupBy(identity)
      .mapValues(_.size)
  }

  override def assetDistribution(assetId: ByteStr): Map[Address, Long] = readOnly { db =>
    (for {
      seqNr     <- (1 to db.get(Keys.addressesForAssetSeqNr(assetId))).par
      addressId <- db.get(Keys.addressesForAsset(assetId, seqNr)).par
      balance   <- db.fromHistory(Keys.assetBalanceHistory(addressId, assetId), Keys.assetBalance(addressId, assetId))
      if balance > 0
    } yield db.get(Keys.idToAddress(addressId)) -> balance).toMap.seq
  }

  override def wavesDistribution(height: Int): Map[Address, Long] = readOnly { db =>
    (for {
      seqNr     <- (1 to db.get(Keys.addressesForWavesSeqNr)).par
      addressId <- db.get(Keys.addressesForWaves(seqNr)).par
      history = db.get(Keys.wavesBalanceHistory(addressId))
      actualHeight <- history.partition(_ > height)._2.headOption
      balance = db.get(Keys.wavesBalance(addressId)(actualHeight))
      if balance > 0
    } yield db.get(Keys.idToAddress(addressId)) -> balance).toMap.seq
  }
}
