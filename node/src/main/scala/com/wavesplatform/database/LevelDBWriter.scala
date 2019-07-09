package com.wavesplatform.database

import java.io.Closeable

import cats.Monoid
import com.google.common.base.Charsets.UTF_8
import com.google.common.cache.CacheBuilder
import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.database.patch.DisableHijackedAliases
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.{Script, ScriptReader}
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
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object LevelDBWriter {

  /** {{{
    * ([10, 7, 4], 5, 11) => [10, 7, 4]
    * ([10, 7], 5, 11) => [10, 7, 1]
    * }}}
    */
  private[database] def slice(v: Seq[Int], from: Int, to: Int): Seq[Int] = { // TODO: Remove if not used
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

class LevelDBWriter(override val writableDB: DB, spendableBalanceChanged: Observer[(Address, Asset)], val settings: BlockchainSettings, val dbSettings: DBSettings)
    extends Caches(spendableBalanceChanged)
      with ScorexLogging
      with Closeable {

  // Only for tests
  def this(writableDB: DB, spendableBalanceChanged: Observer[(Address, Asset)], fs: FunctionalitySettings, dbSettings: DBSettings) =
    this(writableDB, spendableBalanceChanged, BlockchainSettings('T', fs, GenesisSettings.TESTNET), dbSettings)

  private[this] val balanceSnapshotMaxRollbackDepth: Int = dbSettings.maxRollbackDepth + 1000

  import LevelDBWriter._

  private def readStream[A](f: ReadOnlyDB => CloseableIterator[A]): CloseableIterator[A] = CloseableIterator.defer(dbContext.readOnlyStream(f))

  private def readOnly[A](f: ReadOnlyDB => A): A = dbContext.readOnly(f)

  private def readWrite[A](f: RW => A): A = dbContext.readWrite(f)

  override protected def loadMaxAddressId(): AddressId = readOnly(db => db.get(Keys.lastAddressId).getOrElse(AddressId @@ 0L))

  override protected def loadAddressId(address: Address): Option[AddressId] = readOnly(db => db.get(Keys.addressId(address)))

  override protected def loadHeight(): Int = readOnly(_.get(Keys.height))

  override protected def safeRollbackHeight: Int = readOnly(_.get(Keys.safeRollbackHeight))

  override protected def loadScore(): BigInt = readOnly(db => db.get(Keys.score(db.get(Keys.height))))

  override protected def loadLastBlock(): Option[Block] = readOnly { db =>
    val height = Height(db.get(Keys.height))
    log.info(s"Last block is $height")
    loadBlock(height)
  }

  override protected def loadScript(address: Address): Option[Script] = readOnly { db =>
    addressId(address).fold(Option.empty[Script]) { addressId =>
      getAddressScript(addressId).map(_())
    }
  }

  override protected def hasScriptBytes(address: Address): Boolean = readOnly { db =>
    addressId(address).fold(false) { addressId =>
      getAddressScript(addressId).isDefined
    }
  }

  private[this] def getAddressScript(address: Long): Option[() => Script] = readOnly { db =>
    db.lastValue(Keys.AddressScriptPrefix, AddressId.toBytes(address), this.height)
      .flatMap(e => Option(e.getValue).filter(_.nonEmpty).map(bs => () => ScriptReader.fromBytes(bs).explicitGet()))
  }

  override protected def loadAssetScript(asset: IssuedAsset): Option[Script] =
    getAssetScript(asset)

  override protected def hasAssetScriptBytes(asset: IssuedAsset): Boolean =
    getAssetScript(asset).isDefined

  private[this] def getAssetScript(asset: IssuedAsset): Option[Script] =
    for {
      (desc, _) <- assetDescriptionCache.get(asset)
      script    <- desc.script
    } yield script

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
      db.lastValue(Keys.DataPrefix, Bytes.concat(AddressId.toBytes(addressId), key.getBytes(UTF_8)), this.height)
        .map { e =>
          val (_, _, bs, _) = Keys.parseAddressBytesHeight(e.getKey)
          DataEntry.parseValue(new String(bs, UTF_8), e.getValue, 0)._1
        }
    }
  }

  private[this] def readFromStartForAddress(db: ReadOnlyDB)(prefix: Short, addressId: Long) = {
    val prefixBytes = Bytes.concat(
      Shorts.toByteArray(prefix),
      AddressId.toBytes(addressId)
    )
    db.iterateOverStream(prefixBytes)
  }

  private[this] def readLastValue[T](db: ReadOnlyDB)(prefix: Short, bytes: Array[Byte], read: Array[Byte] => T) =
    db.lastValue(prefix, bytes, this.height)
      .map(e => read(e.getValue))

  private[this] def loadBalanceForAssetHN(db: ReadOnlyDB)(addressId: AddressId, issueH: Height, issueN: TxNum): Option[Long] = {
    db.fromHistory(Keys.assetBalanceHistory(addressId, issueH, issueN), Keys.assetBalance(addressId, issueH, issueN))
  }

  private[this] def loadBalanceForAsset(db: ReadOnlyDB)(addressId: AddressId, ia: IssuedAsset) = {
    val result = for {
      (issueH, issueN) <- getAssetHNOption(ia)
      balance          <- loadBalanceForAssetHN(db)(addressId, issueH, issueN)
    } yield balance

    result.getOrElse(0L)
  }

  @noinline
  private[this] def loadTransactionHN(id: TransactionId): Option[(Height, TxNum)] =
    Try(blocksWriter.getTransactionHN(id)).toOption

  override protected def loadAssetHN(asset: IssuedAsset): Option[(Height, TxNum)] =
    loadTransactionHN(TransactionId @@ asset.id)

  protected override def loadBalance(req: (Address, Asset)): Long = readOnly { db =>
    addressId(req._1).fold(0L) { addressId =>
      req._2 match {
        case asset @ IssuedAsset(_) =>
          loadBalanceForAsset(db)(addressId, asset)

        case Waves =>
          db.fromHistory(Keys.wavesBalanceHistory(addressId), Keys.wavesBalance(addressId)).getOrElse(0L)
      }
    }
  }

  private def loadLeaseBalance(db: ReadOnlyDB, addressId: AddressId): LeaseBalance = {
    val lease = db.fromHistory(Keys.leaseBalanceHistory(addressId), Keys.leaseBalance(addressId)).getOrElse(LeaseBalance.empty)
    lease
  }

  override protected def loadLeaseBalance(address: Address): LeaseBalance = readOnly { db =>
    addressId(address).fold(LeaseBalance.empty)(loadLeaseBalance(db, _))
  }

  private def loadLposPortfolio(db: ReadOnlyDB, addressId: AddressId) = Portfolio(
    db.fromHistory(Keys.wavesBalanceHistory(addressId), Keys.wavesBalance(addressId)).getOrElse(0L),
    loadLeaseBalance(db, addressId),
    Map.empty
  )

  private def loadFullPortfolio(db: ReadOnlyDB, addressId: AddressId) = loadLposPortfolio(db, addressId).copy(
    assets = readFromStartForAddress(db)(Keys.AssetBalancePrefix, addressId).closeAfter(_.foldLeft(Map.empty[IssuedAsset, Long]) { (map, e) =>
      val (_, _, bs, _) = Keys.parseAddressBytesHeight(e.getKey)
      val (txH, txN)    = Keys.parseHeightNum(bs)
      val tx = getTransactionByHN(txH, txN)
      map + (IssuedAsset(tx.id()) -> Longs.fromByteArray(e.getValue))
    })
  )

  private def loadPortfolioWithoutNFT(db: ReadOnlyDB, addressId: AddressId) = loadLposPortfolio(db, addressId).copy(
    assets = readFromStartForAddress(db)(Keys.AssetBalancePrefix, addressId).closeAfter(_.foldLeft(Map.empty[IssuedAsset, Long]) { (map, e) =>
      val (_, _, bs, _) = Keys.parseAddressBytesHeight(e.getKey)
      val (txH, txN)    = Keys.parseHeightNum(bs)
      val tx = getTransactionByHN(txH, txN)
      val isNFT = tx match {
        case it: IssueTransaction => it.isNFT
        case _ => false
      }
      if (isNFT) map else map + (IssuedAsset(tx.id()) -> Longs.fromByteArray(e.getValue))
    })
  )

  override protected def loadPortfolio(address: Address): Portfolio = readOnly { db =>
    val excludeNFT = this.isFeatureActivated(BlockchainFeatures.ReduceNFTFee, height)

    addressId(address).fold(Portfolio.empty) { addressId =>
      if (excludeNFT) loadPortfolioWithoutNFT(db, AddressId @@ addressId)
      else loadFullPortfolio(db, addressId)
    }
  }

  override protected def loadAssetDescription(asset: IssuedAsset): Option[(AssetDescription, (Height, TxNum))] = readOnly { db =>
    val maybeTxInfo = Try(blocksWriter.getTransaction(TransactionId @@ asset.id)).toOption
    maybeTxInfo collect {
      case (h, n, i: IssueTransaction) =>
        val ai = readLastValue(db)(Keys.AssetInfoPrefix, Keys.heightWithNum(h, n), readAssetInfo)
          .getOrElse(AssetInfo(i.reissuable, i.quantity))

        val sponsorship = readLastValue(db)(Keys.SponsorshipPrefix, Keys.heightWithNum(h, n), readSponsorship)
          .fold(0L)(_.minFee)

        val script      = readLastValue(db)(Keys.AssetScriptPrefix, Keys.heightWithNum(h, n), ScriptReader.fromBytes(_).toOption).flatten
        val description = AssetDescription(i.sender, i.name, i.description, i.decimals, ai.isReissuable, ai.volume, script, sponsorship)
        (description, (h, n))
    }
  }

  override protected def loadVolumeAndFee(orderId: ByteStr): VolumeAndFee = readOnly { db =>
    db.lastValue(Keys.FilledVolumeAndFeePrefix, orderId, height)
      .fold(VolumeAndFee.empty)(e => readVolumeAndFee(e.getValue))
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
                                  newAddresses: Map[Address, AddressId],
                                  wavesBalances: Map[AddressId, Long],
                                  assetBalances: Map[AddressId, Map[IssuedAsset, Long]],
                                  leaseBalances: Map[AddressId, LeaseBalance],
                                  addressTransactions: Map[AddressId, List[TransactionId]],
                                  leaseStates: Map[ByteStr, Boolean],
                                  reissuedAssets: Map[IssuedAsset, AssetInfo],
                                  filledQuantity: Map[ByteStr, VolumeAndFee],
                                  scripts: Map[AddressId, Option[Script]],
                                  assetScripts: Map[IssuedAsset, Option[Script]],
                                  data: Map[AddressId, AccountDataInfo],
                                  aliases: Map[Alias, AddressId],
                                  sponsorship: Map[IssuedAsset, Sponsorship],
                                  totalFee: Long,
                                  scriptResults: Map[ByteStr, InvokeScriptResult]): Unit = readWrite { rw =>
    val expiredKeys = new ArrayBuffer[Array[Byte]]

    val transactions: Map[TransactionId, (Transaction, TxNum)] =
      block.transactionData.zipWithIndex.map { in =>
        val (tx, idx) = in
        val k         = TransactionId(tx.id())
        val v         = (tx, TxNum(idx.toShort))
        k -> v
      }.toMap

    val threshold        = height - dbSettings.maxRollbackDepth
    val balanceThreshold = height - balanceSnapshotMaxRollbackDepth

    object writes {
      @noinline
      def writeNewBlock(): Unit = {
        rw.put(Keys.height, height)

        val previousSafeRollbackHeight = rw.get(Keys.safeRollbackHeight)
        if (previousSafeRollbackHeight < (height - dbSettings.maxRollbackDepth)) {
          rw.put(Keys.safeRollbackHeight, height - dbSettings.maxRollbackDepth)
        }
        // rw.put(Keys.blockHeaderAndSizeAt(Height(height)), Some((block, block.bytes().length)))
        blocksWriter.writeBlock(Height @@ height, block)
        rw.put(Keys.heightOf(block.uniqueId), Some(height))
        rw.put(Keys.score(height), rw.get(Keys.score(height - 1)) + block.blockScore())
      }

      @noinline
      def writeNewAddresses(): Unit = {
        val lastAddressId = AddressId @@ (loadMaxAddressId() + newAddresses.size)
        rw.put(Keys.lastAddressId, Some(lastAddressId))

        for ((address, id) <- newAddresses) {
          rw.put(Keys.addressId(address), Some(id))
          log.trace(s"WRITE ${address.address} -> $id")
          rw.put(Keys.idToAddress(id), address)
        }
        log.trace(s"WRITE lastAddressId = $lastAddressId")
      }

      @noinline
      def writeWavesBalances(): Unit = {
        val updatedBalanceAddresses = for ((addressId, balance) <- wavesBalances) yield {
          val wavesBalanceHistory = Keys.wavesBalanceHistory(addressId)
          rw.put(Keys.wavesBalance(addressId)(height), balance)
          expiredKeys ++= updateHistory(rw, rw.get(wavesBalanceHistory), wavesBalanceHistory, balanceThreshold, Keys.wavesBalance(addressId))
          addressId
        }

        val changedAddresses = addressTransactions.keys ++ updatedBalanceAddresses
        rw.put(Keys.changedAddresses(height), changedAddresses.toSeq)
      }

      @noinline
      private[this] def getHNForAsset(assetId: IssuedAsset) = {
        lazy val maybeHNFromState = assetDescriptionCache.get(assetId).map(_._2)
        lazy val maybeHNFromNewTransactions = transactions
          .get(TransactionId @@ assetId.id)
          .map { case (_, n) => (Height @@ height, n) }

        maybeHNFromNewTransactions
          .orElse(maybeHNFromState)
      }

      @noinline
      def writeAssetBalances(): Unit = {
        for ((addressId, assets) <- assetBalances; (assetId, balance) <- assets; (h, n) <- getHNForAsset(assetId)) {
          rw.put(Keys.addressesForAsset(h, n, addressId), addressId)
          rw.put(Keys.assetBalance(addressId, h, n)(height), balance)
          expiredKeys ++= updateHistory(rw, Keys.assetBalanceHistory(addressId, h, n), threshold, Keys.assetBalance(addressId, h, n))
        }
      }

      @noinline
      def writeVolumeAndFee(): Unit = {
        for ((orderId, volumeAndFee) <- filledQuantity) {
          rw.put(Keys.filledVolumeAndFee(orderId)(height), volumeAndFee)
        }
      }

      @noinline
      def writeAssets(): Unit = {
        for ((asset, assetInfo) <- reissuedAssets; (h, n) <- getHNForAsset(asset)) {
          val combinedAssetInfo =
            readLastValue(rw)(Keys.AssetInfoPrefix, Keys.heightWithNum(h, n), readAssetInfo).fold(assetInfo)(old => Monoid.combine(old, assetInfo))
          rw.put(Keys.assetInfo(h, n)(height), combinedAssetInfo)
        }
      }

      @noinline
      def writeLeaseBalances(): Unit = {
        for ((addressId, leaseBalance) <- leaseBalances) {
          rw.put(Keys.leaseBalance(addressId)(height), leaseBalance)
          expiredKeys ++= updateHistory(rw, Keys.leaseBalanceHistory(addressId), balanceThreshold, Keys.leaseBalance(addressId))
        }

        for ((leaseId, state) <- leaseStates) {
          rw.put(Keys.leaseStatus(leaseId)(height), state)
        }
      }

      @noinline
      def writeScripts(): Unit = {
        for ((addressId, script) <- scripts) {
          rw.put(Keys.addressScript(addressId)(height), script)
        }

        for ((asset, script) <- assetScripts; (h, n) <- getHNForAsset(asset)) {
          rw.put(Keys.assetScript(h, n)(height), script)
        }
      }

      @noinline
      def writeDataEntries(): Unit = {
        for ((addressId, addressData) <- data) {
          rw.put(Keys.changedDataKeys(height, addressId), addressData.data.keys.toSeq)
          val newKeys = (
            for {
              (key, value) <- addressData.data
              dataKeySuffix = Bytes.concat(AddressId.toBytes(addressId), key.getBytes(UTF_8))
              isNew         = rw.lastValue(Keys.DataPrefix, dataKeySuffix, height).isEmpty
              _             = rw.put(Keys.data(addressId, key)(height), Some(value))
              // _             = deleteOldKeys(Keys.DataPrefix, dataKeySuffix)(Keys.data(addressId, key))
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
      }

      @noinline
      def writeTransactionsByAddress(): Unit = {
        if (dbSettings.storeTransactionsByAddress) for ((addressId, txIds) <- addressTransactions) {
          val kk = Keys.addressTransactionSeqNr(addressId)
          val nextSeqNr = rw.get(kk) + 1
          val txTypeNumSeq = txIds.map { txId =>
            val (tx, num) = transactions(txId)
            (tx.id(), tx.builder.typeId, num)
          }
          // for ((id, _, num) <- txTypeNumSeq) rw.put(Keys.transactionIdByHN(Height @@ height, num), TransactionId @@ id)
          rw.put(Keys.addressTransactionHN(addressId, nextSeqNr), Some((Height(height), txTypeNumSeq.map(v => (v._2, v._3)).sortBy(-_._2))))
          rw.put(kk, nextSeqNr)
        }
      }

      @noinline
      def writeAliases(): Unit = {
        for ((alias, addressId) <- aliases) {
          rw.put(Keys.addressIdOfAlias(alias), Some(addressId))
        }
      }

      @noinline
      def writeFeatures(): Unit = {
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
      }

      @noinline
      def writeSponsorship(): Unit = {
        for ((asset, sp: SponsorshipValue) <- sponsorship; (h, n) <- getHNForAsset(asset)) {
          rw.put(Keys.sponsorship(h, n)(height), sp)
        }
      }

      @noinline
      def writeFee(): Unit = {
        rw.put(Keys.carryFee(height), carry)
        expiredKeys += Keys.carryFee(threshold - 1).keyBytes

        rw.put(Keys.blockTransactionsFee(height), totalFee)
      }

      @noinline
      def writeInvokeScriptResults(): Unit = {
        if (dbSettings.storeInvokeScriptResults) scriptResults.foreach {
          case (txId, result) =>
            val (txHeight, txNum) = transactions
              .get(TransactionId(txId))
              .map { case (_, txN) => (height, txN) }
              .orElse(loadTransactionHN(TransactionId(txId)))
              .getOrElse(throw new IllegalArgumentException(s"Couldn't find transaction height and num: $txId"))

            rw.put(Keys.invokeScriptResult(txHeight, txNum), result)
        }
      }

      @noinline
      def deleteExpiredKeys(): Unit = {
        expiredKeys.foreach(rw.delete(_, "expired-keys"))
      }

      @noinline
      def writeAllEntities(): Unit = {
        writeNewBlock()
        writeNewAddresses()
        writeWavesBalances()
        writeAssets()
        writeAssetBalances()
        writeVolumeAndFee()
        writeLeaseBalances()
        writeScripts()
        writeDataEntries()
        writeAliases()
        writeFeatures()
        writeSponsorship()
        writeFee()
        writeInvokeScriptResults()
        writeTransactionsByAddress()
        deleteExpiredKeys()
      }
    }

    writes.writeAllEntities()

    if (activatedFeatures.get(BlockchainFeatures.DataTransaction.id).contains(height)) {
      DisableHijackedAliases(rw, blocksWriter)
    }
  }

  private[this] def assetBalanceIterator(db: ReadOnlyDB, addressId: Long) = {
    db.iterateOverStream(KeyHelpers.addr(Keys.AssetBalancePrefix, addressId))
      .map { e =>
        val (_, _, hn, height) = Keys.parseAddressBytesHeight(e.getKey)
        val (issueH, issueN) = Keys.parseHeightNum(hn)
        val tx = getTransactionByHN(issueH, issueN)
        (IssuedAsset(tx.id()), height) -> Longs.fromByteArray(e.getValue)
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

          val discardedHeader = Try(blocksWriter.getBlock(h))
            .getOrElse(throw new IllegalArgumentException(s"No block at height $currentHeight"))

          for (aId <- rw.get(Keys.changedAddresses(currentHeight))) {
            val addressId = AddressId(aId)
            val address   = rw.get(Keys.idToAddress(addressId))
            balancesToInvalidate += (address -> Waves)

            for (((asset, h), _) <- assetBalanceIterator(rw, addressId) if h == currentHeight) {
              balancesToInvalidate += (address -> asset)
              getAssetHNOption(asset)
                .foreach {
                  case (txH, txN) =>
                    rw.delete(Keys.assetBalance(addressId, txH, txN)(currentHeight))
                    rw.filterHistory(Keys.assetBalanceHistory(addressId, txH, txN), currentHeight)
                  // resetLastForAddress(Keys.assetBalanceLastHeight(addressId, txH, txN), Keys.AssetBalancePrefix, Keys.heightWithNum(txH, txN))
                }
            }

            for (k <- rw.get(Keys.changedDataKeys(currentHeight, addressId))) {
              log.trace(s"Discarding $k for $address at $currentHeight")
              rw.delete(Keys.data(addressId, k)(currentHeight))
            }

            rw.delete(Keys.wavesBalance(addressId)(currentHeight))
            // resetLastForAddress(Keys.wavesBalanceLastHeight(addressId), Keys.WavesBalancePrefix)
            rw.filterHistory(Keys.wavesBalanceHistory(addressId), currentHeight)

            rw.delete(Keys.leaseBalance(addressId)(currentHeight))
            //resetLastForAddress(Keys.leaseBalanceLastHeight(addressId), Keys.LeaseBalancePrefix)
            rw.filterHistory(Keys.leaseBalanceHistory(addressId), currentHeight)

            log.trace(s"Discarding portfolio for $address")

            portfoliosToInvalidate += address
            balanceAtHeightCache.invalidate((currentHeight, addressId))
            leaseBalanceAtHeightCache.invalidate((currentHeight, addressId))
            discardLeaseBalance(address)

            if (dbSettings.storeTransactionsByAddress) {
              val kTxSeqNr = Keys.addressTransactionSeqNr(addressId)
              val txSeqNr = rw.get(kTxSeqNr)
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
                  }

                case tx: SetAssetScriptTransaction =>
                  val asset = tx.asset
                  assetScriptsToDiscard += asset
                  for ((h, n) <- getAssetHNOption(asset))
                    rw.delete(Keys.assetScript(h, n)(currentHeight))

                case _: DataTransaction => // see changed data keys removal

                case _: InvokeScriptTransaction =>
                  rw.delete(Keys.invokeScriptResult(h, num))

                case tx: CreateAliasTransaction => rw.delete(Keys.addressIdOfAlias(tx.alias))
                case tx: ExchangeTransaction =>
                  ordersToInvalidate += rollbackOrderFill(rw, tx.buyOrder.id(), currentHeight)
                  ordersToInvalidate += rollbackOrderFill(rw, tx.sellOrder.id(), currentHeight)
              }
          }

          blocksWriter.deleteBlock(Height @@ currentHeight)
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
    for ((h, n) <- getAssetHNOption(asset))
      rw.delete(Keys.assetInfo(h, n)(currentHeight))
    asset
  }

  private def rollbackOrderFill(rw: RW, orderId: ByteStr, currentHeight: Int): ByteStr = {
    rw.delete(Keys.filledVolumeAndFee(orderId)(currentHeight))
    orderId
  }

  private def rollbackLeaseStatus(rw: RW, leaseId: ByteStr, currentHeight: Int): Unit = {
    rw.delete(Keys.leaseStatus(leaseId)(currentHeight))
  }

  private def rollbackSponsorship(rw: RW, asset: IssuedAsset, currentHeight: Int): IssuedAsset = {
    for ((h, n) <- assetHNCache.get(asset))
      rw.delete(Keys.sponsorship(h, n)(currentHeight))
    asset
  }

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] = readOnly(transactionInfo(id, _))

  protected def transactionInfo(id: ByteStr, db: ReadOnlyDB): Option[(Int, Transaction)] = {
    for {
      (height, _, tx) <- Try(blocksWriter.getTransaction(TransactionId(id))).toOption
    } yield (height, tx)
  }

  override def transactionHeight(id: ByteStr): Option[Int] =
    loadTransactionHN(TransactionId @@ id).map(_._1)

  override def nftList(address: Address, from: Option[IssuedAsset]): CloseableIterator[IssueTransaction] = readStream { db =>
    val assetIdStream: CloseableIterator[IssuedAsset] = db
      .get(Keys.addressId(address))
      .map(assetBalanceIterator(db, _))
      .getOrElse(CloseableIterator.empty)
      .map { case ((asset, _), _) => asset }
      .closeAfter(_.toVector) // FIXME: Proper reverse iterator
      .distinct
      .reverseIterator

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
    val maybeAfter = fromId.flatMap(id => Try(blocksWriter.getTransactionHN(TransactionId(id))).toOption)

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
              case None => Nil
            })

        val hns = takeAfter(takeTypes(heightNumStream, types.map(_.typeId)), maybeAfter)
          .map { case (height, _, txNum) => (height, txNum) }
          .toVector

        blocksWriter.getTransactionsByHN(hns: _*)
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

  private[this] def getTransactionByHN(height: Height, txNum: TxNum) = {
    //    readOnly { db =>
    //      val transactionId = db.get(Keys.transactionIdByHN(height, txNum))
    //      val (_, _, tx) = blocksWriter.getTransaction(transactionId)
    //      tx
    //    }

    blocksWriter.getTransactionsByHN(height -> txNum)
      .toStream
      .headOption
      .map(_._3)
      .getOrElse(throw new NoSuchElementException(s"No transaction at $height/$txNum"))
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
    db.iterateOverStream(123.toShort).flatMap { kv =>
      val txId = TransactionId(ByteStr(kv.getKey.drop(2)))

      if (loadLeaseStatus(db, txId)) {
        Try(blocksWriter.getTransaction(txId)).toOption
          .collect { case (_, _, lt: LeaseTransaction) => lt }
      } else None
    }
  }

  override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] = readOnly { db =>
    val b = Map.newBuilder[Address, A]
    for (id <- 1L to db.get(Keys.lastAddressId).getOrElse(0L)) {
      val address = db.get(Keys.idToAddress(AddressId @@ id))
      pf.runWith(b += address -> _)(address -> loadLposPortfolio(db, AddressId @@ id))
    }
    b.result()
  }

  def loadScoreOf(blockId: ByteStr): Option[BigInt] = {
    readOnly(db => db.get(Keys.heightOf(blockId)).map(h => db.get(Keys.score(h))))
  }

  override def loadBlockHeaderAndSize(height: Int): Option[(BlockHeader, Int)] = {
    Try(blocksWriter.getBlock(Height @@ height)).toOption
      .map(_ -> 1234) // TODO store block size
  }

  override def loadBlockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)] = {
    writableDB
      .get(Keys.heightOf(blockId))
      .flatMap(loadBlockHeaderAndSize)
  }

  override def loadBlockBytes(h: Int): Option[Array[Byte]] = {
    val height = Height(h)
    Try(blocksWriter.getBlock(height, withTxs = true))
      .map(_.bytes()) // TODO: Read without deser
      .toOption
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

    (currentHeight until (currentHeight - howMany).max(0) by -1).iterator
      .map(loadBlockHeaderAndSize)
      .collect {
        case Some((header, _)) => header.signerData.signature
      }
      .toVector
  }

  override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]] = readOnly { db =>
    db.get(Keys.heightOf(parentSignature)).map { parentHeight =>
      (parentHeight + 1 to (parentHeight + howMany))
        .map(loadBlockHeaderAndSize)
        .collect {
          case Some((header, _)) => header.signerData.signature
        }
    }
  }

  override def parentHeader(block: BlockHeader, back: Int): Option[BlockHeader] = readOnly { db =>
    for {
      h <- db.get(Keys.heightOf(block.reference))
      height = Height(h - back + 1)
      (block, _) <- loadBlockHeaderAndSize(height)
    } yield block
  }

  override def totalFee(height: Int): Option[Long] = readOnly { db =>
    Try(db.get(Keys.blockTransactionsFee(height))).toOption
  }

  override def featureVotes(height: Int): Map[Short, Int] = readOnly { db =>
    settings.functionalitySettings
      .activationWindow(height)
      .flatMap { height =>
        loadBlockHeaderAndSize(Height(height))
          .map(_._1.featureVotes.toSeq)
          .getOrElse(Seq.empty)
      }
      .groupBy(identity)
      .mapValues(_.size)
  }

  override def assetDistribution(asset: IssuedAsset): AssetDistribution = readOnly { db =>
    val (issueH, issueN) = getAssetHN(asset)

    val dst = (for {
      addressId <- db
        .iterateOverStream(Bytes.concat(Shorts.toByteArray(Keys.AddressesForAssetPrefix), Keys.heightWithNum(issueH, issueN)))
        .map(e => AddressId.fromBytes(e.getKey.takeRight(4)))
        .closeAfter(_.toVector)
        .par
      balance <- loadBalanceForAssetHN(db)(addressId, issueH, issueN) if balance > 0
    } yield db.get(Keys.idToAddress(addressId)) -> balance).toMap.seq

    AssetDistribution(dst)
  }

  override def assetDistributionAtHeight(asset: IssuedAsset,
                                         height: Int,
                                         count: Int,
                                         fromAddress: Option[Address]): Either[ValidationError, AssetDistributionPage] = readOnly { db =>
    lazy val (issueH, issueN) = getAssetHN(asset)

    lazy val page: AssetDistributionPage = {
      val addressIds: CloseableIterator[AddressId] = {
        def takeAfter(s: CloseableIterator[AddressId], a: Option[AddressId]): CloseableIterator[AddressId] = {
          a match {
            case None    => s
            case Some(v) => s.dropWhile(_ != v).drop(1)
          }
        }

        val all = for {
          addressId <- db
            .iterateOverStream(Bytes.concat(Shorts.toByteArray(Keys.AddressesForAssetPrefix), Keys.heightWithNum(issueH, issueN)))
            .map(e => AddressId.fromBytes(e.getKey.takeRight(AddressId.Bytes)))
        } yield addressId

        val maybeAddressId = fromAddress.flatMap(addr => db.get(Keys.addressId(addr)))
        takeAfter(all, maybeAddressId)
      }

      val distribution: CloseableIterator[(Address, Long)] =
        for {
          addressId <- addressIds
          balance <- db
            .lastValue(Keys.AssetBalancePrefix, Bytes.concat(AddressId.toBytes(addressId), Keys.heightWithNum(issueH, issueN)), height)
            .map(e => Longs.fromByteArray(e.getValue))
            .filter(_ > 0)
        } yield db.get(Keys.idToAddress(addressId)) -> balance

      distribution.closeAfter { distribution =>
        val dst = distribution.take(count + 1).toStream

        val hasNext = dst.length > count
        val items   = if (hasNext) dst.init else dst
        val lastKey = items.lastOption.map(_._1)

        val result: Paged[Address, AssetDistribution] =
          Paged(hasNext, lastKey, AssetDistribution(items.toMap))
        AssetDistributionPage(result)
      }
    }

    val canGetAfterHeight = db.get(Keys.safeRollbackHeight)
    Either
      .cond(
        height > canGetAfterHeight,
        page,
        GenericError(s"Cannot get asset distribution at height less than ${canGetAfterHeight + 1}")
      )
  }

  private[this] def getAssetHNOption(asset: IssuedAsset): Option[(Height, TxNum)] = {
    assetHNCache.get(asset)
  }

  private[this] def getAssetHN(asset: IssuedAsset): (Height, TxNum) = {
    getAssetHNOption(asset).getOrElse((Height @@ 0, TxNum @@ 0.toShort))
  }

  override def wavesDistribution(height: Int): Either[ValidationError, Map[Address, Long]] = readOnly { db =>
    val canGetAfterHeight = db.get(Keys.safeRollbackHeight)

    def createMap() = {
      val balances = db.iterateOverStream(Keys.WavesBalancePrefix).map { e =>
        val (_, addressId, _, height) = Keys.parseAddressBytesHeight(e.getKey)
        (addressId, height) -> Longs.fromByteArray(e.getValue)
      }

      val byAddressId = balances.closeAfter(_.foldLeft(Map.empty[AddressId, Long]) {
        case (map, ((addressId, h), balance)) =>
          if (h <= height) map + (addressId -> balance)
          else map
      })

      byAddressId.map(kv => db.get(Keys.idToAddress(kv._1)) -> kv._2)
    }

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
        val (height, txNum) = loadTransactionHN(txId).getOrElse(throw new IllegalArgumentException("No such transaction"))
        db.get(Keys.invokeScriptResult(height, txNum))
      }).toEither.left.map(err => GenericError(s"Couldn't load InvokeScript result: ${err.getMessage}"))
    } yield result

  override def close(): Unit = {
    blocksWriter.close()
    writableDB.close()
  }

  private[this] def transactionsIterator(ofTypes: Seq[TransactionParser], reverse: Boolean): CloseableIterator[(Height, TxNum, Transaction)] = {
    val codeSet = ofTypes.map(_.typeId).toSet
    for {
      //            height  <- (1 to this.height).iterator
      //            block = blocksWriter.getBlock(Height @@ height, withTxs = true)
      (block, height) <- blocksWriter.blocksIterator().zipWithIndex
      // _ = log.info(s"Block $height of ${this.height} processed")
      (height, txNum, tx) <- block.transactionData.zipWithIndex.map(kv => (Height @@ height, TxNum @@ kv._2.toShort, kv._1))
      if codeSet.contains(tx.builder.typeId)
    } yield (height, txNum, tx)
  }

  private[database] def loadBlock(height: Height): Option[Block] = {
    val tr = Try(blocksWriter.getBlock(height, withTxs = true))
    // tr.failed.foreach(log.error(s"Error loading block at $height", _))
    // tr.foreach(block => log.warn(s"Loaded block at $height is $block"))
    tr.toOption
  }

  private def transactionsAtHeight(h: Height): List[(TxNum, Transaction)] =
    blocksWriter.getBlock(h, withTxs = true).transactionData.zipWithIndex.map(kv => (TxNum @@ kv._2.toShort, kv._1)).toList

  private[this] def loadLeaseStatus(db: ReadOnlyDB, leaseId: ByteStr): Boolean =
    db.lastValue(Keys.LeaseStatusPrefix, leaseId, this.height)
      .exists(e => e.getValue.headOption.contains(1: Byte))
}
