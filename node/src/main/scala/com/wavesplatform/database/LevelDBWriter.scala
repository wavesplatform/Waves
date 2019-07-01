package com.wavesplatform.database

import java.nio.ByteBuffer

import cats.Monoid
import com.google.common.cache.CacheBuilder
import com.google.common.primitives.{Ints, Shorts}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.database.patch.DisableHijackedAliases
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.settings.{BlockchainSettings, DBSettings, FunctionalitySettings, GenesisSettings}
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{TxNum, _}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.Transaction.Type
import com.wavesplatform.transaction.TxValidationError.{AliasDoesNotExist, AliasIsDisabled, GenericError}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.{CloseableIterator, Paged, ScorexLogging}
import monix.reactive.Observer
import org.iq80.leveldb.DB

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.{immutable, mutable}
import scala.util.{Success, Try}

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

class LevelDBWriter(writableDB: DB, spendableBalanceChanged: Observer[(Address, Asset)], val settings: BlockchainSettings, val dbSettings: DBSettings)
    extends Caches(spendableBalanceChanged)
    with ScorexLogging {

  // Only for tests
  def this(writableDB: DB, spendableBalanceChanged: Observer[(Address, Asset)], fs: FunctionalitySettings, dbSettings: DBSettings) =
    this(writableDB, spendableBalanceChanged, BlockchainSettings('T', fs, GenesisSettings.TESTNET), dbSettings)

  private[this] val balanceSnapshotMaxRollbackDepth: Int = dbSettings.maxRollbackDepth + 1000
  import LevelDBWriter._

  private def readStream[A](f: ReadOnlyDB => CloseableIterator[A]): CloseableIterator[A] = CloseableIterator.defer(writableDB.readOnlyStream(f))

  private def readOnly[A](f: ReadOnlyDB => A): A = writableDB.readOnly(f)

  private def readWrite[A](f: RW => A): A = writableDB.readWrite(f)

  override protected def loadMaxAddressId(): BigInt = readOnly(db => db.get(Keys.lastAddressId).getOrElse(BigInt(0)))

  override protected def loadAddressId(address: Address): Option[BigInt] = readOnly(db => db.get(Keys.addressId(address)))

  override protected def loadHeight(): Int = readOnly(_.get(Keys.height))

  override protected def safeRollbackHeight: Int = readOnly(_.get(Keys.safeRollbackHeight))

  override protected def loadScore(): BigInt = readOnly(db => db.get(Keys.score(db.get(Keys.height))))

  override protected def loadLastBlock(): Option[Block] = readOnly { db =>
    val height = Height(db.get(Keys.height))
    loadBlock(height, db)
  }

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

  override protected def loadAssetScript(asset: IssuedAsset): Option[Script] = readOnly { db =>
    db.fromHistory(Keys.assetScriptHistory(asset), Keys.assetScript(asset)).flatten
  }

  override protected def hasAssetScriptBytes(asset: IssuedAsset): Boolean = readOnly { db =>
    db.fromHistory(Keys.assetScriptHistory(asset), Keys.assetScriptPresent(asset)).flatten.nonEmpty
  }

  override def carryFee: Long = readOnly(_.get(Keys.carryFee(height)))

  override def accountData(address: Address): AccountDataInfo = readOnly { db =>
    AccountDataInfo((for {
      key   <- accountDataKeys(address)
      value <- accountData(address, key)
    } yield key -> value).toMap)
  }

  override def accountDataKeys(address: Address): Seq[String] = readOnly { db =>
    for {
      addressId <- addressId(address).toVector
      keyChunkCount = db.get(Keys.dataKeyChunkCount(addressId))
      chunkNo <- Range(0, keyChunkCount)
      key     <- db.get(Keys.dataKeyChunk(addressId, chunkNo))
    } yield key
  }

  override def accountData(address: Address, key: String): Option[DataEntry[_]] = readOnly { db =>
    addressId(address).fold(Option.empty[DataEntry[_]]) { addressId =>
      db.fromHistory(Keys.dataHistory(addressId, key), Keys.data(addressId, key)).flatten
    }
  }

  protected override def loadBalance(req: (Address, Asset)): Long = readOnly { db =>
    addressId(req._1).fold(0L) { addressId =>
      req._2 match {
        case asset @ IssuedAsset(_) => db.fromHistory(Keys.assetBalanceHistory(addressId, asset), Keys.assetBalance(addressId, asset)).getOrElse(0L)
        case Waves                  => db.fromHistory(Keys.wavesBalanceHistory(addressId), Keys.wavesBalance(addressId)).getOrElse(0L)
      }
    }
  }

  private def loadLeaseBalance(db: ReadOnlyDB, addressId: BigInt): LeaseBalance = {
    val lease = db.fromHistory(Keys.leaseBalanceHistory(addressId), Keys.leaseBalance(addressId)).getOrElse(LeaseBalance.empty)
    lease
  }

  override protected def loadLeaseBalance(address: Address): LeaseBalance = readOnly { db =>
    addressId(address).fold(LeaseBalance.empty)(loadLeaseBalance(db, _))
  }

  private def loadLposPortfolio(db: ReadOnlyDB, addressId: BigInt) = Portfolio(
    db.fromHistory(Keys.wavesBalanceHistory(addressId), Keys.wavesBalance(addressId)).getOrElse(0L),
    loadLeaseBalance(db, addressId),
    Map.empty
  )

  private def loadFullPortfolio(db: ReadOnlyDB, addressId: BigInt) = loadLposPortfolio(db, addressId).copy(
    assets = (for {
      asset <- db.get(Keys.assetList(addressId))
    } yield asset -> db.fromHistory(Keys.assetBalanceHistory(addressId, asset), Keys.assetBalance(addressId, asset)).getOrElse(0L)).toMap
  )

  private def loadPortfolioWithoutNFT(db: ReadOnlyDB, addressId: AddressId) = loadLposPortfolio(db, addressId).copy(
    assets = (for {
      issuedAsset <- db.get(Keys.assetList(addressId))
      asset <- transactionInfo(issuedAsset.id).collect {
        case (_, it: IssueTransaction) if !it.isNFT => issuedAsset
      }
    } yield asset -> db.fromHistory(Keys.assetBalanceHistory(addressId, asset), Keys.assetBalance(addressId, asset)).getOrElse(0L)).toMap
  )

  override protected def loadPortfolio(address: Address): Portfolio = readOnly { db =>
    val excludeNFT = this.isFeatureActivated(BlockchainFeatures.ReduceNFTFee, height)

    addressId(address).fold(Portfolio.empty) { addressId =>
      if (excludeNFT) loadPortfolioWithoutNFT(db, AddressId @@ addressId)
      else loadFullPortfolio(db, addressId)
    }
  }

  override protected def loadAssetDescription(asset: IssuedAsset): Option[AssetDescription] = readOnly { db =>
    transactionInfo(asset.id, db) match {
      case Some((_, i: IssueTransaction)) =>
        val ai          = db.fromHistory(Keys.assetInfoHistory(asset), Keys.assetInfo(asset)).getOrElse(AssetInfo(i.reissuable, i.quantity))
        val sponsorship = db.fromHistory(Keys.sponsorshipHistory(asset), Keys.sponsorship(asset)).fold(0L)(_.minFee)
        val script      = db.fromHistory(Keys.assetScriptHistory(asset), Keys.assetScript(asset)).flatten
        Some(AssetDescription(i.sender, i.name, i.description, i.decimals, ai.isReissuable, ai.volume, script, sponsorship))
      case _ => None
    }
  }

  override protected def loadVolumeAndFee(orderId: ByteStr): VolumeAndFee = readOnly { db =>
    db.fromHistory(Keys.filledVolumeAndFeeHistory(orderId), Keys.filledVolumeAndFee(orderId)).getOrElse(VolumeAndFee.empty)
  }

  override protected def loadApprovedFeatures(): Map[Short, Int] = {
    readOnly(_.get(Keys.approvedFeatures))
  }

  override protected def loadActivatedFeatures(): Map[Short, Int] = {
    val stateFeatures = readOnly(_.get(Keys.activatedFeatures))
    stateFeatures ++ settings.functionalitySettings.preActivatedFeatures
  }

  private def updateHistory(rw: RW, key: Key[Seq[Int]], threshold: Int, kf: Int => Key[_]): Seq[Array[Byte]] =
    updateHistory(rw, rw.get(key), key, threshold, kf)

  private def updateHistory(rw: RW, history: Seq[Int], key: Key[Seq[Int]], threshold: Int, kf: Int => Key[_]): Seq[Array[Byte]] = {
    val (c1, c2) = history.partition(_ > threshold)
    rw.put(key, (height +: c1) ++ c2.headOption)
    c2.drop(1).map(kf(_).keyBytes)
  }

  //noinspection ScalaStyle
  override protected def doAppend(block: Block,
                                  carry: Long,
                                  newAddresses: Map[Address, BigInt],
                                  balances: Map[BigInt, Map[Asset, Long]],
                                  leaseBalances: Map[BigInt, LeaseBalance],
                                  addressTransactions: Map[AddressId, List[TransactionId]],
                                  leaseStates: Map[ByteStr, Boolean],
                                  reissuedAssets: Map[IssuedAsset, AssetInfo],
                                  filledQuantity: Map[ByteStr, VolumeAndFee],
                                  scripts: Map[BigInt, Option[Script]],
                                  assetScripts: Map[IssuedAsset, Option[Script]],
                                  data: Map[BigInt, AccountDataInfo],
                                  aliases: Map[Alias, BigInt],
                                  sponsorship: Map[IssuedAsset, Sponsorship],
                                  totalFee: Long,
                                  scriptResults: Map[ByteStr, InvokeScriptResult]): Unit = readWrite { rw =>
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

    rw.put(Keys.blockHeaderAndSizeAt(Height(height)), Some((block, block.bytes().length)))
    rw.put(Keys.heightOf(block.uniqueId), Some(height))

    val lastAddressId = loadMaxAddressId() + newAddresses.size

    rw.put(Keys.lastAddressId, Some(lastAddressId))
    rw.put(Keys.score(height), rw.get(Keys.score(height - 1)) + block.blockScore())

    for ((address, id) <- newAddresses) {
      rw.put(Keys.addressId(address), Some(id))
      log.trace(s"WRITE ${address.address} -> $id")
      rw.put(Keys.idToAddress(id), address)
    }
    log.trace(s"WRITE lastAddressId = $lastAddressId")

    val threshold        = height - dbSettings.maxRollbackDepth
    val balanceThreshold = height - balanceSnapshotMaxRollbackDepth

    // balances
    val updatedBalanceAddresses = ArrayBuffer.empty[BigInt]
    val newAddressesForAssets   = mutable.AnyRefMap.empty[Asset, Set[BigInt]]
    def addNewAddressForAsset(asset: Asset, addressId: BigInt): Unit =
      newAddressesForAssets += asset -> (newAddressesForAssets.getOrElse(asset, Set.empty) + addressId)

    for ((addressId, updatedBalances) <- balances) {
      lazy val prevAssets    = rw.get(Keys.assetList(addressId))
      lazy val prevAssetsSet = prevAssets.toSet
      val newAssets          = ArrayBuffer.empty[IssuedAsset]

      for ((asset, balance) <- updatedBalances) {
        asset match {
          case Waves =>
            val kwbh = Keys.wavesBalanceHistory(addressId)
            val wbh  = rw.get(kwbh)
            if (wbh.isEmpty) {
              addNewAddressForAsset(Waves, addressId)
            }
            updatedBalanceAddresses += addressId
            rw.put(Keys.wavesBalance(addressId)(height), balance)
            expiredKeys ++= updateHistory(rw, wbh, kwbh, balanceThreshold, Keys.wavesBalance(addressId))
          case a: IssuedAsset =>
            if (!prevAssetsSet.contains(a)) {
              newAssets += a
              addNewAddressForAsset(a, addressId)
            }
            rw.put(Keys.assetBalance(addressId, a)(height), balance)
            expiredKeys ++= updateHistory(rw, Keys.assetBalanceHistory(addressId, a), threshold, Keys.assetBalance(addressId, a))
        }
      }

      if (newAssets.nonEmpty) {
        rw.put(Keys.assetList(addressId), newAssets.toList ++ prevAssets)
      }
    }

    for ((asset, newAddressIds) <- newAddressesForAssets) {
      asset match {
        case Waves =>
          val newSeqNr = rw.get(Keys.addressesForWavesSeqNr) + 1
          rw.put(Keys.addressesForWavesSeqNr, newSeqNr)
          rw.put(Keys.addressesForWaves(newSeqNr), newAddressIds.toSeq)
        case a: IssuedAsset =>
          val seqNrKey  = Keys.addressesForAssetSeqNr(a)
          val nextSeqNr = rw.get(seqNrKey) + 1
          val key       = Keys.addressesForAsset(a, nextSeqNr)
          rw.put(seqNrKey, nextSeqNr)
          rw.put(key, newAddressIds.toSeq)
      }
    }

    val changedAddresses = addressTransactions.keys ++ updatedBalanceAddresses
    rw.put(Keys.changedAddresses(height), changedAddresses.toSeq)

    // leases
    for ((addressId, leaseBalance) <- leaseBalances) {
      rw.put(Keys.leaseBalance(addressId)(height), leaseBalance)
      expiredKeys ++= updateHistory(rw, Keys.leaseBalanceHistory(addressId), balanceThreshold, Keys.leaseBalance(addressId))
    }

    for ((orderId, volumeAndFee) <- filledQuantity) {
      rw.put(Keys.filledVolumeAndFee(orderId)(height), volumeAndFee)
      expiredKeys ++= updateHistory(rw, Keys.filledVolumeAndFeeHistory(orderId), threshold, Keys.filledVolumeAndFee(orderId))
    }

    for ((asset, assetInfo) <- reissuedAssets) {
      val combinedAssetInfo = rw.fromHistory(Keys.assetInfoHistory(asset), Keys.assetInfo(asset)).fold(assetInfo) { p =>
        Monoid.combine(p, assetInfo)
      }
      rw.put(Keys.assetInfo(asset)(height), combinedAssetInfo)
      expiredKeys ++= updateHistory(rw, Keys.assetInfoHistory(asset), threshold, Keys.assetInfo(asset))
    }

    for ((leaseId, state) <- leaseStates) {
      rw.put(Keys.leaseStatus(leaseId)(height), state)
      expiredKeys ++= updateHistory(rw, Keys.leaseStatusHistory(leaseId), threshold, Keys.leaseStatus(leaseId))
    }

    for ((addressId, script) <- scripts) {
      expiredKeys ++= updateHistory(rw, Keys.addressScriptHistory(addressId), threshold, Keys.addressScript(addressId))
      script.foreach(s => rw.put(Keys.addressScript(addressId)(height), Some(s)))
    }

    for ((asset, script) <- assetScripts) {
      expiredKeys ++= updateHistory(rw, Keys.assetScriptHistory(asset), threshold, Keys.assetScript(asset))
      script.foreach(s => rw.put(Keys.assetScript(asset)(height), Some(s)))
    }

    for ((addressId, addressData) <- data) {
      rw.put(Keys.changedDataKeys(height, addressId), addressData.data.keys.toSeq)
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

    if (dbSettings.storeTransactionsByAddress) for ((addressId, txIds) <- addressTransactions) {
      val kk        = Keys.addressTransactionSeqNr(addressId)
      val nextSeqNr = rw.get(kk) + 1
      val txTypeNumSeq = txIds.map { txId =>
        val (tx, num) = transactions(txId)

        (tx.builder.typeId, num)
      }
      rw.put(Keys.addressTransactionHN(addressId, nextSeqNr), Some((Height(height), txTypeNumSeq.sortBy(-_._2))))
      rw.put(kk, nextSeqNr)
    }

    for ((alias, addressId) <- aliases) {
      rw.put(Keys.addressIdOfAlias(alias), Some(addressId))
    }

    for ((id, (tx, num)) <- transactions) {
      rw.put(Keys.transactionAt(Height(height), num), Some(tx))
      rw.put(Keys.transactionHNById(id), Some((Height(height), num)))
    }

    val activationWindowSize = settings.functionalitySettings.activationWindowSize(height)
    if (height % activationWindowSize == 0) {
      val minVotes = settings.functionalitySettings.blocksForFeatureActivation(height)
      val newlyApprovedFeatures = featureVotes(height)
        .filterNot { case (featureId, _) => settings.functionalitySettings.preActivatedFeatures.contains(featureId) }
        .collect { case (featureId, voteCount) if voteCount + (if (block.featureVotes(featureId)) 1 else 0) >= minVotes => featureId -> height }

      if (newlyApprovedFeatures.nonEmpty) {
        approvedFeaturesCache = newlyApprovedFeatures ++ rw.get(Keys.approvedFeatures)
        rw.put(Keys.approvedFeatures, approvedFeaturesCache)

        val featuresToSave = newlyApprovedFeatures.mapValues(_ + activationWindowSize) ++ rw.get(Keys.activatedFeatures)

        activatedFeaturesCache = featuresToSave ++ settings.functionalitySettings.preActivatedFeatures
        rw.put(Keys.activatedFeatures, featuresToSave)
      }
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

        rw.put(Keys.invokeScriptResult(txHeight, txNum), result)
    }

    expiredKeys.foreach(rw.delete(_, "expired-keys"))

    if (activatedFeatures.get(BlockchainFeatures.DataTransaction.id).contains(height)) {
      DisableHijackedAliases(rw)
    }
  }

  override protected def doRollback(targetBlockId: ByteStr): Seq[Block] = {
    readOnly(_.get(Keys.heightOf(targetBlockId))).fold(Seq.empty[Block]) { targetHeight =>
      log.debug(s"Rolling back to block $targetBlockId at $targetHeight")

      val discardedBlocks: Seq[Block] = for (currentHeight <- height until targetHeight by -1) yield {
        val balancesToInvalidate   = Seq.newBuilder[(Address, Asset)]
        val portfoliosToInvalidate = Seq.newBuilder[Address]
        val assetInfoToInvalidate  = Seq.newBuilder[IssuedAsset]
        val ordersToInvalidate     = Seq.newBuilder[ByteStr]
        val scriptsToDiscard       = Seq.newBuilder[Address]
        val assetScriptsToDiscard  = Seq.newBuilder[IssuedAsset]

        val h = Height(currentHeight)

        val discardedBlock = readWrite { rw =>
          log.trace(s"Rolling back to ${currentHeight - 1}")
          rw.put(Keys.height, currentHeight - 1)

          val (discardedHeader, _) = rw
            .get(Keys.blockHeaderAndSizeAt(h))
            .getOrElse(throw new IllegalArgumentException(s"No block at height $currentHeight"))

          for (aId <- rw.get(Keys.changedAddresses(currentHeight))) {
            val addressId = AddressId(aId)
            val address   = rw.get(Keys.idToAddress(addressId))
            balancesToInvalidate += (address -> Waves)

            for (assetId <- rw.get(Keys.assetList(addressId))) {
              balancesToInvalidate += (address -> assetId)
              rw.delete(Keys.assetBalance(addressId, assetId)(currentHeight))
              rw.filterHistory(Keys.assetBalanceHistory(addressId, assetId), currentHeight)
            }

            for (k <- rw.get(Keys.changedDataKeys(currentHeight, addressId))) {
              log.trace(s"Discarding $k for $address at $currentHeight")
              rw.delete(Keys.data(addressId, k)(currentHeight))
              rw.filterHistory(Keys.dataHistory(addressId, k), currentHeight)
            }

            rw.delete(Keys.wavesBalance(addressId)(currentHeight))
            rw.filterHistory(Keys.wavesBalanceHistory(addressId), currentHeight)

            rw.delete(Keys.leaseBalance(addressId)(currentHeight))
            rw.filterHistory(Keys.leaseBalanceHistory(addressId), currentHeight)

            log.trace(s"Discarding portfolio for $address")

            portfoliosToInvalidate += address
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
                  assetInfoToInvalidate += rollbackAssetInfo(rw, IssuedAsset(tx.id()), currentHeight)
                case tx: ReissueTransaction =>
                  assetInfoToInvalidate += rollbackAssetInfo(rw, tx.asset, currentHeight)
                case tx: BurnTransaction =>
                  assetInfoToInvalidate += rollbackAssetInfo(rw, tx.asset, currentHeight)
                case tx: SponsorFeeTransaction =>
                  assetInfoToInvalidate += rollbackSponsorship(rw, tx.asset, currentHeight)
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
                  rw.delete(Keys.invokeScriptResult(h, num))

                case tx: CreateAliasTransaction => rw.delete(Keys.addressIdOfAlias(tx.alias))
                case tx: ExchangeTransaction =>
                  ordersToInvalidate += rollbackOrderFill(rw, tx.buyOrder.id(), currentHeight)
                  ordersToInvalidate += rollbackOrderFill(rw, tx.sellOrder.id(), currentHeight)
              }

              if (tx.builder.typeId != GenesisTransaction.typeId) {
                rw.delete(Keys.transactionAt(h, num))
                rw.delete(Keys.transactionHNById(TransactionId(tx.id())))
              }
          }

          rw.delete(Keys.blockHeaderAndSizeAt(h))
          rw.delete(Keys.heightOf(discardedHeader.signerData.signature))
          rw.delete(Keys.carryFee(currentHeight))
          rw.delete(Keys.blockTransactionsFee(currentHeight))

          if (activatedFeatures.get(BlockchainFeatures.DataTransaction.id).contains(currentHeight)) {
            DisableHijackedAliases.revert(rw)
          }

          Block
            .fromHeaderAndTransactions(
              discardedHeader,
              transactions
                .map(_._2)
            )
            .explicitGet()
        }

        balancesToInvalidate.result().foreach(discardBalance)
        portfoliosToInvalidate.result().foreach(discardPortfolio)
        assetInfoToInvalidate.result().foreach(discardAssetDescription)
        ordersToInvalidate.result().foreach(discardVolumeAndFee)
        scriptsToDiscard.result().foreach(discardScript)
        assetScriptsToDiscard.result().foreach(discardAssetScript)
        discardedBlock
      }

      log.debug(s"Rollback to block $targetBlockId at $targetHeight completed")

      discardedBlocks.reverse
    }
  }

  private def rollbackAssetInfo(rw: RW, asset: IssuedAsset, currentHeight: Int): IssuedAsset = {
    rw.delete(Keys.assetInfo(asset)(currentHeight))
    rw.filterHistory(Keys.assetInfoHistory(asset), currentHeight)
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

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] = readOnly(transactionInfo(id, _))

  protected def transactionInfo(id: ByteStr, db: ReadOnlyDB): Option[(Int, Transaction)] = {
    val txId = TransactionId(id)
    for {
      (height, num) <- db.get(Keys.transactionHNById(txId))
      tx            <- db.get(Keys.transactionAt(height, num))
    } yield (height, tx)
  }

  override def transactionHeight(id: ByteStr): Option[Int] = readOnly { db =>
    val txId = TransactionId(id)
    db.get(Keys.transactionHNById(txId)).map(_._1)
  }

  override def nftList(address: Address, from: Option[IssuedAsset]): CloseableIterator[IssueTransaction] = readStream { db =>
    val assetIdStream: CloseableIterator[IssuedAsset] = db
      .get(Keys.addressId(address))
      .fold(CloseableIterator.empty[IssuedAsset]) { id =>
        val addressId = AddressId @@ id

        val iter = db.get(Keys.assetList(addressId)).iterator

        CloseableIterator(iter, () => CloseableIterator.close(iter))
      }

    val issueTxStream = assetIdStream
      .flatMap(ia => transactionInfo(ia.id, db).map(_._2))
      .collect {
        case itx: IssueTransaction if itx.isNFT => itx
      }

    from
      .flatMap(ia => transactionInfo(ia.id, db))
      .fold(issueTxStream) {
        case (_, afterTx) =>
          issueTxStream
            .dropWhile(_.id() != afterTx.id())
            .drop(1)
      }
  }

  override def addressTransactions(address: Address,
                                   types: Set[TransactionParser],
                                   fromId: Option[ByteStr]): CloseableIterator[(Height, Transaction)] = readStream { db =>
    val maybeAfter = fromId.flatMap(id => db.get(Keys.transactionHNById(TransactionId(id))))

    db.get(Keys.addressId(address)).fold(CloseableIterator.empty[(Height, Transaction)]) { id =>
      val heightAndTxs: CloseableIterator[(Height, TxNum, Transaction)] = if (dbSettings.storeTransactionsByAddress) {
        def takeTypes(txNums: Iterator[(Height, Type, TxNum)], maybeTypes: Set[Type]) =
          if (maybeTypes.nonEmpty) txNums.filter { case (_, tp, _) => maybeTypes.contains(tp) } else txNums

        def takeAfter(txNums: Iterator[(Height, Type, TxNum)], maybeAfter: Option[(Height, TxNum)]) =
          maybeAfter match {
            case None => txNums
            case Some((filterHeight, filterNum)) =>
              txNums
                .dropWhile { case (streamHeight, _, _) => streamHeight > filterHeight }
                .dropWhile { case (streamHeight, _, streamNum) => streamNum >= filterNum && streamHeight >= filterHeight }
          }

        val addressId = AddressId(id)
        val heightNumStream = (db.get(Keys.addressTransactionSeqNr(addressId)) to 1 by -1).toIterator
          .flatMap(seqNr =>
            db.get(Keys.addressTransactionHN(addressId, seqNr)) match {
              case Some((height, txNums)) => txNums.map { case (txType, txNum) => (height, txType, txNum) }
              case None                   => Nil
          })

        takeAfter(takeTypes(heightNumStream, types.map(_.typeId)), maybeAfter)
          .flatMap { case (height, _, txNum) => db.get(Keys.transactionAt(height, txNum)).map((height, txNum, _)) }
      } else {
        def takeAfter(txNums: Iterator[(Height, TxNum, Transaction)], maybeAfter: Option[(Height, TxNum)]) = maybeAfter match {
          case None => txNums
          case Some((filterHeight, filterNum)) =>
            txNums
              .dropWhile { case (streamHeight, _, _) => streamHeight > filterHeight }
              .dropWhile { case (streamHeight, streamNum, _) => streamNum >= filterNum && streamHeight >= filterHeight }
        }

        transactionsIterator(types.toVector, reverse = true)
          .transform(takeAfter(_, maybeAfter))
      }

      heightAndTxs
        .transform(_.map { case (height, _, tx) => (height, tx) })
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
    transactionInfo(leaseId, db) match {
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

  override def balanceSnapshots(address: Address, from: Int, to: BlockId): Seq[BalanceSnapshot] = readOnly { db =>
    db.get(Keys.addressId(address)).fold(Seq(BalanceSnapshot(1, 0, 0, 0))) { addressId =>
      val toHeigth = this.heightOf(to).getOrElse(this.height)
      val wbh      = slice(db.get(Keys.wavesBalanceHistory(addressId)), from, toHeigth)
      val lbh      = slice(db.get(Keys.leaseBalanceHistory(addressId)), from, toHeigth)
      for {
        (wh, lh) <- merge(wbh, lbh)
        wb = balanceAtHeightCache.get((wh, addressId), () => db.get(Keys.wavesBalance(addressId)(wh)))
        lb = leaseBalanceAtHeightCache.get((lh, addressId), () => db.get(Keys.leaseBalance(addressId)(lh)))
      } yield BalanceSnapshot(wh.max(lh), wb, lb.in, lb.out)
    }
  }

  /**
    * @todo move this method to `object LevelDBWriter` once SmartAccountTrading is activated
    */
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

  override def allActiveLeases: CloseableIterator[LeaseTransaction] = readStream { db =>
    db.iterateOverStream(Keys.TransactionHeightNumByIdPrefix).flatMap { kv =>
      val txId = TransactionId(ByteStr(kv.getKey.drop(2)))

      if (loadLeaseStatus(db, txId)) {
        val heightNumBytes = kv.getValue

        val height = Height(Ints.fromByteArray(heightNumBytes.take(4)))
        val txNum  = TxNum(Shorts.fromByteArray(heightNumBytes.takeRight(2)))

        db.get(Keys.transactionAt(height, txNum))
          .collect { case lt: LeaseTransaction => lt }
      } else None
    }
  }

  override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] = readOnly { db =>
    val b = Map.newBuilder[Address, A]
    for (id <- BigInt(1) to db.get(Keys.lastAddressId).getOrElse(BigInt(0))) {
      val address = db.get(Keys.idToAddress(id))
      pf.runWith(b += address -> _)(address -> loadLposPortfolio(db, id))
    }
    b.result()
  }

  def loadScoreOf(blockId: ByteStr): Option[BigInt] = {
    readOnly(db => db.get(Keys.heightOf(blockId)).map(h => db.get(Keys.score(h))))
  }

  override def loadBlockHeaderAndSize(height: Int): Option[(BlockHeader, Int)] = {
    writableDB.get(Keys.blockHeaderAndSizeAt(Height(height)))
  }

  def loadBlockHeaderAndSize(height: Int, db: ReadOnlyDB): Option[(BlockHeader, Int)] = {
    db.get(Keys.blockHeaderAndSizeAt(Height(height)))
  }

  override def loadBlockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)] = {
    writableDB
      .get(Keys.heightOf(blockId))
      .flatMap(loadBlockHeaderAndSize)
  }

  def loadBlockHeaderAndSize(blockId: ByteStr, db: ReadOnlyDB): Option[(BlockHeader, Int)] = {
    db.get(Keys.heightOf(blockId))
      .flatMap(loadBlockHeaderAndSize(_, db))
  }

  override def loadBlockBytes(h: Int): Option[Array[Byte]] = readOnly { db =>
    import com.wavesplatform.crypto._

    val height = Height(h)

    val consensuDataOffset = 1 + 8 + SignatureLength

    // version + timestamp + reference + baseTarget + genSig
    val txCountOffset = consensuDataOffset + 8 + Block.GeneratorSignatureLength

    val headerKey = Keys.blockHeaderBytesAt(height)

    def readTransactionBytes(count: Int) = {
      (0 until count).toArray.flatMap { n =>
        db.get(Keys.transactionBytesAt(height, TxNum(n.toShort)))
          .map { txBytes =>
            Ints.toByteArray(txBytes.length) ++ txBytes
          }
          .getOrElse(throw new Exception(s"Cannot parse ${n}th transaction in block at height: $h"))
      }
    }

    db.get(headerKey)
      .map { headerBytes =>
        val bytesBeforeCData   = headerBytes.take(consensuDataOffset)
        val consensusDataBytes = headerBytes.slice(consensuDataOffset, consensuDataOffset + 40)
        val version            = headerBytes.head
        val (txCount, txCountBytes) = if (version == 1 || version == 2) {
          val byte = headerBytes(txCountOffset)
          (byte.toInt, Array[Byte](byte))
        } else {
          val bytes = headerBytes.slice(txCountOffset, txCountOffset + 4)
          (Ints.fromByteArray(bytes), bytes)
        }

        val bytesAfterTxs =
          if (version > 2) {
            headerBytes.drop(txCountOffset + txCountBytes.length)
          } else {
            headerBytes.takeRight(SignatureLength + KeyLength)
          }

        val txBytes = txCountBytes ++ readTransactionBytes(txCount)

        val bytes = bytesBeforeCData ++
          Ints.toByteArray(consensusDataBytes.length) ++
          consensusDataBytes ++
          Ints.toByteArray(txBytes.length) ++
          txBytes ++
          bytesAfterTxs

        bytes
      }
  }

  override def loadBlockBytes(blockId: ByteStr): Option[Array[Byte]] = {
    readOnly(db => db.get(Keys.heightOf(blockId))).flatMap(h => loadBlockBytes(h))
  }

  override def loadHeightOf(blockId: ByteStr): Option[Int] = {
    readOnly(_.get(Keys.heightOf(blockId)))
  }

  override def lastBlockIds(howMany: Int): immutable.IndexedSeq[ByteStr] = readOnly { db =>
    // since this is called from outside of the main blockchain updater thread, instead of using cached height,
    // explicitly read height from storage to make this operation atomic.
    val currentHeight = db.get(Keys.height)

    (currentHeight until (currentHeight - howMany).max(0) by -1)
      .map { h =>
        val height = Height(h)
        db.get(Keys.blockHeaderAndSizeAt(height))
      }
      .collect {
        case Some((header, _)) => header.signerData.signature
      }
  }

  override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]] = readOnly { db =>
    db.get(Keys.heightOf(parentSignature)).map { parentHeight =>
      (parentHeight + 1 to (parentHeight + howMany))
        .map { h =>
          val height = Height(h)
          db.get(Keys.blockHeaderAndSizeAt(height))
        }
        .collect {
          case Some((header, _)) => header.signerData.signature
        }
    }
  }

  override def parentHeader(block: BlockHeader, back: Int): Option[BlockHeader] = readOnly { db =>
    for {
      h <- db.get(Keys.heightOf(block.reference))
      height = Height(h - back + 1)
      (block, _) <- loadBlockHeaderAndSize(height, db)
    } yield block
  }

  override def totalFee(height: Int): Option[Long] = readOnly { db =>
    Try(db.get(Keys.blockTransactionsFee(height))).toOption
  }

  override def featureVotes(height: Int): Map[Short, Int] = readOnly { db =>
    settings.functionalitySettings
      .activationWindow(height)
      .flatMap { h =>
        val height = Height(h)
        db.get(Keys.blockHeaderAndSizeAt(height))
          .map(_._1.featureVotes.toSeq)
          .getOrElse(Seq.empty)
      }
      .groupBy(identity)
      .mapValues(_.size)
  }

  override def assetDistribution(asset: IssuedAsset): AssetDistribution = readOnly { db =>
    val dst = (for {
      seqNr     <- (1 to db.get(Keys.addressesForAssetSeqNr(asset))).par
      addressId <- db.get(Keys.addressesForAsset(asset, seqNr)).par
      actualHeight <- db
        .get(Keys.assetBalanceHistory(addressId, asset))
        .filterNot(_ > height)
        .headOption
      balance = db.get(Keys.assetBalance(addressId, asset)(actualHeight))
      if balance > 0
    } yield db.get(Keys.idToAddress(addressId)) -> balance).toMap.seq

    AssetDistribution(dst)
  }

  override def assetDistributionAtHeight(asset: IssuedAsset,
                                         height: Int,
                                         count: Int,
                                         fromAddress: Option[Address]): Either[ValidationError, AssetDistributionPage] = readOnly { db =>
    val canGetAfterHeight = db.get(Keys.safeRollbackHeight)

    lazy val maybeAddressId = fromAddress.flatMap(addr => db.get(Keys.addressId(addr)))

    def takeAfter(s: Seq[BigInt], a: Option[BigInt]): Seq[BigInt] = {
      a match {
        case None    => s
        case Some(v) => s.dropWhile(_ != v).drop(1)
      }
    }

    lazy val addressIds: Seq[BigInt] = {
      val all = for {
        seqNr <- 1 to db.get(Keys.addressesForAssetSeqNr(asset))
        addressId <- db
          .get(Keys.addressesForAsset(asset, seqNr))
      } yield addressId

      takeAfter(all, maybeAddressId)
    }

    lazy val distribution: Stream[(Address, Long)] =
      for {
        addressId <- addressIds.toStream
        history = db.get(Keys.assetBalanceHistory(addressId, asset))
        actualHeight <- history.filterNot(_ > height).headOption
        balance = db.get(Keys.assetBalance(addressId, asset)(actualHeight))
        if balance > 0
      } yield db.get(Keys.idToAddress(addressId)) -> balance

    lazy val page: AssetDistributionPage = {
      val dst = distribution.take(count + 1)

      val hasNext = dst.length > count
      val items   = if (hasNext) dst.init else dst
      val lastKey = items.lastOption.map(_._1)

      val result: Paged[Address, AssetDistribution] =
        Paged(hasNext, lastKey, AssetDistribution(items.toMap))

      AssetDistributionPage(result)
    }

    Either
      .cond(
        height > canGetAfterHeight,
        page,
        GenericError(s"Cannot get asset distribution at height less than ${canGetAfterHeight + 1}")
      )
  }

  override def wavesDistribution(height: Int): Either[ValidationError, Map[Address, Long]] = readOnly { db =>
    val canGetAfterHeight = db.get(Keys.safeRollbackHeight)

    def createMap() =
      (for {
        seqNr     <- (1 to db.get(Keys.addressesForWavesSeqNr)).par
        addressId <- db.get(Keys.addressesForWaves(seqNr)).par
        history = db.get(Keys.wavesBalanceHistory(addressId))
        actualHeight <- history.partition(_ > height)._2.headOption
        balance = db.get(Keys.wavesBalance(addressId)(actualHeight))
        if balance > 0
      } yield db.get(Keys.idToAddress(addressId)) -> balance).toMap.seq

    Either.cond(
      height > canGetAfterHeight,
      createMap(),
      GenericError(s"Cannot get waves distribution at height less than ${canGetAfterHeight + 1}")
    )
  }

  override def invokeScriptResult(txId: TransactionId): Either[ValidationError, InvokeScriptResult] =
    for {
      _ <- Either.cond(dbSettings.storeInvokeScriptResults, (), GenericError("InvokeScript results are disabled"))
      result <- Try(readOnly { db =>
        val (height, txNum) = db.get(Keys.transactionHNById(txId)).getOrElse(throw new IllegalArgumentException("No such transaction"))
        db.get(Keys.invokeScriptResult(height, txNum))
      }).toEither.left.map(err => GenericError(s"Couldn't load InvokeScript result: ${err.getMessage}"))
    } yield result

  private[this] def transactionsIterator(ofTypes: Seq[TransactionParser], reverse: Boolean): CloseableIterator[(Height, TxNum, Transaction)] =
    readStream { db =>
      val baseIterator: CloseableIterator[(Height, TxNum)] =
        if (reverse) {
          for {
            height  <- (this.height to 0 by -1).iterator
            (bh, _) <- this.blockHeaderAndSize(height).iterator
            txNum   <- bh.transactionCount to 0 by -1
          } yield (Height(height), TxNum(txNum.toShort))
        } else {
          db.iterateOverStream(Keys.TransactionHeightNumByIdPrefix)
            .transform(_.map { kv =>
              val heightNumBytes = kv.getValue

              val height = Height(Ints.fromByteArray(heightNumBytes.take(4)))
              val txNum  = TxNum(Shorts.fromByteArray(heightNumBytes.takeRight(2)))

              (height, txNum)
            })
        }

      baseIterator
        .transform(_.flatMap {
          case (height, txNum) =>
            db.get(Keys.transactionBytesAt(height, txNum))
              .flatMap { txBytes =>
                if (ofTypes.isEmpty)
                  TransactionParsers.parseBytes(txBytes).toOption
                else {
                  ofTypes.iterator
                    .map(_.parseBytes(txBytes))
                    .collectFirst { case Success(tx) => tx }
                }
              }
              .map((height, txNum, _))
        })
    }

  private[database] def loadBlock(height: Height): Option[Block] = readOnly { db =>
    loadBlock(height, db)
  }

  private[database] def loadBlock(height: Height, db: ReadOnlyDB): Option[Block] = {
    val headerKey = Keys.blockHeaderAndSizeAt(height)

    for {
      (header, _) <- db.get(headerKey)
      txs = (0 until header.transactionCount).toList.flatMap { n =>
        db.get(Keys.transactionAt(height, TxNum(n.toShort)))
      }
      block <- Block.fromHeaderAndTransactions(header, txs).toOption
    } yield block
  }

  private def transactionsAtHeight(h: Height): List[(TxNum, Transaction)] = readOnly { db =>
    val txs = new ListBuffer[(TxNum, Transaction)]()

    val prefix = ByteBuffer
      .allocate(6)
      .putShort(Keys.TransactionInfoPrefix)
      .putInt(h)
      .array()

    db.iterateOver(prefix) { entry =>
      val k = entry.getKey
      val v = entry.getValue

      for {
        idx <- Try(Shorts.fromByteArray(k.slice(6, 8)))
        tx  <- TransactionParsers.parseBytes(v)
      } txs.append((TxNum(idx), tx))
    }

    txs.toList
  }
}
