package com.wavesplatform.ride.runner.blockchain

import cats.syntax.option.*
import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.blockchain.SignedBlockHeaderWithVrf
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.features.EstimatorProvider
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.PBTransactions.toVanillaScript
import com.wavesplatform.protobuf.transaction.SignedTransaction.Transaction
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.ride.runner.caches.*
import com.wavesplatform.ride.runner.caches.disk.DiskCaches
import com.wavesplatform.ride.runner.caches.mem.{GrpcCacheKeyConverters, MemBlockchainDataCache, MemCacheKey, MemCacheWeights}
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite, RideDbAccess}
import com.wavesplatform.ride.runner.estimate
import com.wavesplatform.ride.runner.stats.RideRunnerStats
import com.wavesplatform.ride.runner.stats.RideRunnerStats.*
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.{
  AccountScriptInfo,
  AssetDescription,
  AssetScriptInfo,
  BalanceSnapshot,
  DataEntry,
  Height,
  LeaseBalance,
  TransactionId,
  TxMeta
}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.AliasDoesNotExist
import com.wavesplatform.utils.ScorexLogging

import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

class LazyBlockchain[TagT] private (
    override val settings: BlockchainSettings,
    blockchainApi: BlockchainApi,
    db: RideDbAccess,
    diskCaches: DiskCaches,
    blockHeaders: BlockHeaderStorage,
    memCache: MemBlockchainDataCache,
    allTags: CacheKeyTags[TagT]
) extends SupportedBlockchain
    with ScorexLogging {
  // Ride: get*Value (data), get* (data)
  override def accountData(address: Address, dataKey: String): Option[DataEntry[?]] = db.directReadWrite { implicit ctx =>
    val atMaxHeight = heightUntagged
    // If a key doesn't exist, the lambda will block other memCache updates.
    // For any blockchain update use memCache.set first and then update a disk cache
    //   to guarantee overwrite of fetched from gRPC API data.
    memCache
      .getOrLoad(MemCacheKey.AccountData(address, dataKey)) { key =>
        val cached = diskCaches.accountDataEntries.get(atMaxHeight, (key.address, key.dataKey))
        if (cached.loaded) cached
        else
          RemoteData
            .loaded(blockchainApi.getAccountDataEntry(key.address, key.dataKey))
            .tap { r => diskCaches.accountDataEntries.set(atMaxHeight, (key.address, key.dataKey), r) }
      }
  }.mayBeValue

  // Ride: scriptHash
  override def accountScript(address: Address): Option[AccountScriptInfo] = db
    .directReadWrite { implicit ctx =>
      val atMaxHeight = heightUntagged
      memCache.getOrLoad(MemCacheKey.AccountScript(address)) { key =>
        val cached = diskCaches.accountScripts.get(atMaxHeight, key.address)
        if (cached.loaded) cached
        else
          RemoteData
            .loaded(blockchainApi.getAccountScript(key.address))
            .map(Function.tupled(toWeightedAccountScriptInfo))
            .tap { r => diskCaches.accountScripts.set(atMaxHeight, key.address, r) }
      }
    }
    .mayBeValue
    .map(_.accountScriptInfo)

  // Ride: blockInfoByHeight, lastBlock
  override def blockHeader(height: Int): Option[SignedBlockHeader] = blockHeaderWithVrf(Height(height)).map(_.header)

  // Ride: blockInfoByHeight
  override def hitSource(height: Int): Option[ByteStr] = blockHeaderWithVrf(Height(height)).map(_.vrf)

  // Ride: blockInfoByHeight
  override def blockReward(height: Int): Option[Long] = blockHeaderWithVrf(Height(height)).map(_.blockReward)

  private def blockHeaderWithVrf(atHeight: Height): Option[SignedBlockHeaderWithVrf] = db.directReadWrite { implicit ctx =>
    blockHeaders.getOrFetch(atHeight)
  }

  // Ride: wavesBalance, height, lastBlock
  override def height: Int = heightUntagged

  // Ride: environment initialization
  override def activatedFeatures: ActivatedFeatures = currentActivatedFeatures.get()

  // Ride: assetInfo
  override def assetDescription(asset: IssuedAsset): Option[AssetDescription] = db
    .directReadWrite { implicit ctx =>
      val atMaxHeight = heightUntagged
      memCache.getOrLoad(MemCacheKey.Asset(asset)) { key =>
        val cached = diskCaches.assetDescriptions.get(atMaxHeight, key.asset)
        if (cached.loaded) cached
        else
          RemoteData
            .loaded(blockchainApi.getAssetDescription(key.asset))
            .map(toWeightedAssetDescription)
            .tap { r => diskCaches.assetDescriptions.set(atMaxHeight, key.asset, r) }
      }
    }
    .mayBeValue
    .map(_.assetDescription)

  // Ride (indirectly): asset script validation
  override def assetScript(id: Asset.IssuedAsset): Option[AssetScriptInfo] = assetDescription(id).flatMap(_.script)

  // Ride: get*Value (data), get* (data), isDataStorageUntouched, balance, scriptHash, wavesBalance
  override def resolveAlias(a: Alias): Either[ValidationError, Address] = db
    .directReadWrite { implicit ctx =>
      memCache.getOrLoad(MemCacheKey.Alias(a)) { key =>
        val cached = diskCaches.aliases.getAddress(key.alias)
        if (cached.loaded) cached
        else
          RemoteData
            .loaded(blockchainApi.resolveAlias(key.alias))
            .tap { r => diskCaches.aliases.setAddress(heightUntagged, key.alias, r) }
      }
    }
    .mayBeValue
    .toRight(AliasDoesNotExist(a))

  // Ride: wavesBalance
  override def leaseBalance(address: Address): LeaseBalance = db
    .directReadWrite { implicit ctx =>
      val atMaxHeight = heightUntagged
      memCache.getOrLoad(MemCacheKey.AccountLeaseBalance(address)) { key =>
        val cached = diskCaches.accountLeaseBalances.get(atMaxHeight, key.address)
        if (cached.loaded) cached
        else
          RemoteData
            .loaded(blockchainApi.getLeaseBalance(key.address))
            .map(r => LeaseBalance(r.leaseIn, r.leaseOut))
            .tap { r => diskCaches.accountLeaseBalances.set(atMaxHeight, key.address, r) }
      }
    }
    .mayBeValue
    .getOrElse(LeaseBalance.empty)

  // Ride: assetBalance, wavesBalance
  override def balance(address: Address, asset: Asset): Long = db
    .directReadWrite { implicit ctx =>
      val atMaxHeight = heightUntagged
      memCache.getOrLoad(MemCacheKey.AccountBalance(address, asset)) { key =>
        val cached = diskCaches.accountBalances.get(atMaxHeight, (key.address, key.asset))
        if (cached.loaded) cached
        else
          RemoteData
            .loaded(blockchainApi.getBalance(key.address, key.asset))
            .tap { r => diskCaches.accountBalances.set(atMaxHeight, (key.address, key.asset), r) }
      }
    }
    .mayBeValue
    .getOrElse(0L)

  // Retrieves Waves balance snapshot in the [from, to] range (inclusive)
  // Ride: wavesBalance (specifies to=None), "to" always None and means "to the end"
  override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] = {
    // NOTE: This code leads to a wrong generating balance, but we see no use-cases for now
    val lb           = leaseBalance(address)
    val wavesBalance = balance(address, Asset.Waves)
    List(BalanceSnapshot(height, wavesBalance, lb.in, lb.out))
  }

  // Ride: transactionHeightById
  override def transactionMeta(id: ByteStr): Option[TxMeta] =
    getTransactionHeight(TransactionId(id)).map(TxMeta(_, succeeded = true, 0)) // Other information not used

  private def getTransactionHeight(id: TransactionId): Option[Height] = db.directReadWrite { implicit ctx =>
    memCache.getOrLoad(MemCacheKey.Transaction(id)) { key =>
      val cached = diskCaches.transactions.getHeight(key.id)
      if (cached.loaded) cached
      else
        RemoteData
          .loaded(blockchainApi.getTransactionHeight(key.id))
          .tap { r => diskCaches.transactions.setHeight(key.id, r) }
    }
  }.mayBeValue

  private val chainId = settings.addressSchemeCharacter.toByte
  private val conv    = new GrpcCacheKeyConverters(chainId)

  def heightUntaggedOpt: Option[Height] = blockHeaders.latestHeight
  private def heightUntagged: Height    = blockHeaders.latestHeight.getOrElse(blockchainApi.getCurrentBlockchainHeight())

  def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean] =
    db.directReadWrite { implicit ctx =>
      blockHeaders.getLocal(height).map(_.header.id() == id)
    }

  private val currentActivatedFeatures = new AtomicReference(
    diskCaches
      .getActivatedFeatures()
      .orElse(RemoteData.loaded(blockchainApi.getActivatedFeatures(heightUntagged).tap(diskCaches.setActivatedFeatures)))
      .mayBeValue
      .getOrElse(throw new RuntimeException("Impossible: activated features are empty"))
  )

  private def updateFeatures(xs: Map[Short, Height]): Unit = {
    currentActivatedFeatures.set(xs)
    diskCaches.setActivatedFeatures(xs)
  }

  private def estimator: ScriptEstimator = EstimatorProvider.byActivatedFeatures(settings.functionalitySettings, activatedFeatures, heightUntagged)

  def removeAllFrom(height: Height): Unit = db.batchedReadWrite { implicit ctx =>
    removeAllFromCtx(height)
  }

  private def removeAllFromCtx(height: Height)(implicit ctx: ReadWrite): Unit = {
    blockHeaders.removeFrom(height)
    diskCaches.accountDataEntries
      .removeAllFrom(height)
      .foreach(x => memCache.remove(MemCacheKey.AccountData(x._1, x._2)))
    diskCaches.accountScripts
      .removeAllFrom(height)
      .foreach(x => memCache.remove(MemCacheKey.AccountScript(x)))
    diskCaches.assetDescriptions
      .removeAllFrom(height)
      .foreach(x => memCache.remove(MemCacheKey.Asset(x)))
    diskCaches.aliases
      .removeAllFrom(height)
      .foreach(x => memCache.remove(MemCacheKey.Alias(x)))
    diskCaches.accountBalances
      .removeAllFrom(height)
      .foreach(x => memCache.remove(MemCacheKey.AccountBalance(x._1, x._2)))
    diskCaches.accountLeaseBalances
      .removeAllFrom(height)
      .foreach(x => memCache.remove(MemCacheKey.AccountLeaseBalance(x)))
    diskCaches.transactions
      .removeAllFrom(height)
      .foreach(x => memCache.remove(MemCacheKey.Transaction(x)))

    val features         = currentActivatedFeatures.get()
    val filteredFeatures = features.filterNot { case (_, featureHeight) => featureHeight >= height }
    if (filteredFeatures.size != features.size) updateFeatures(filteredFeatures)
  }

  private val empty = AffectedTags.empty[TagT]
  def process(event: BlockchainUpdated): AffectedTags[TagT] =
    db.batchedReadWrite { implicit ctx =>
      val toHeight = Height(event.height)
      val affected = event.update match {
        case Update.Empty         => empty // Ignore
        case Update.Append(evt)   => append(toHeight, evt)
        case Update.Rollback(evt) => rollback(toHeight, evt)
      }

      blockHeaders.update(event)
      affected
    }

  private def updateCacheIfExists[CacheKeyT <: MemCacheKey](key: CacheKeyT)(v: RemoteData[CacheKeyT#ValueT]): AffectedTags[TagT] = {
    getAffectedTags(key).tap { tags =>
      if (tags.isEmpty) memCache.updateIfExists(key, v) // Not yet removed from memCache, but already removed from tags
      else memCache.set(key, v)
    }
  }

  private def removeCache[CacheKeyT <: MemCacheKey](key: CacheKeyT): AffectedTags[TagT] = {
    memCache.remove(key)
    getAffectedTags(key)
  }

  private def getAffectedTags(key: MemCacheKey): AffectedTags[TagT] = allTags.get(key).getOrElse(AffectedTags.empty)

  private def append(atHeight: Height, evt: BlockchainUpdated.Append)(implicit ctx: ReadWrite): AffectedTags[TagT] = {
    val (initialAffectedTags, txs, timer) = evt.body match {
      case Body.Block(block)           => (getAffectedTags(MemCacheKey.Height), block.getBlock.transactions, RideRunnerStats.blockProcessingTime.some)
      case Body.MicroBlock(microBlock) => (empty, microBlock.getMicroBlock.getMicroBlock.transactions, RideRunnerStats.microBlockProcessingTime.some)
      case Body.Empty                  => (empty, Seq.empty, none)
    }

    timer.fold(empty) { timer =>
      timer.measure {
        if (evt.getBlock.activatedFeatures.nonEmpty)
          updateFeatures(currentActivatedFeatures.get() ++ evt.getBlock.activatedFeatures.map(featureId => featureId.toShort -> atHeight))

        val setScriptTxSenderPublicKeys = mutable.Map.empty[Address, PublicKey]
        val withTxAffectedTags = txs.view
          .map(_.transaction)
          .foldLeft(initialAffectedTags) { case (r, tx) =>
            tx match {
              case Transaction.WavesTransaction(tx) =>
                tx.data match {
                  case Data.CreateAlias(txData) =>
                    val cacheKey = conv.aliasKey(txData)
                    val v        = RemoteData.Cached(conv.aliasValue(tx))
                    diskCaches.aliases.setAddress(atHeight, cacheKey.alias, v)
                    r ++ updateCacheIfExists(cacheKey)(v)

                  case Data.SetScript(_) =>
                    val pk = tx.senderPublicKey.toPublicKey
                    setScriptTxSenderPublicKeys.put(pk.toAddress(chainId), pk)
                    r

                  case _ => r
                }
              case _ => r
            }
          }

        val stateUpdate = (evt.getStateUpdate +: evt.transactionStateUpdates).view
        withTxAffectedTags ++
          stateUpdate.flatMap(_.assets).foldLeft(empty) { case (r, x) =>
            val cacheKey = conv.assetKey(x)
            val v        = RemoteData.loaded(conv.assetValueAfter(cacheKey.asset, x).map(toWeightedAssetDescription))
            diskCaches.assetDescriptions.set(atHeight, cacheKey.asset, v)
            r ++ updateCacheIfExists(cacheKey)(v)
          } ++
          stateUpdate.flatMap(_.balances).foldLeft(empty) { case (r, x) =>
            val (cacheKey, rawValue) = conv.accountBalanceKeyAndValueAfter(x)
            val v                    = RemoteData.loaded(rawValue)
            diskCaches.accountBalances.set(atHeight, (cacheKey.address, cacheKey.asset), v)
            r ++ updateCacheIfExists(cacheKey)(v)
          } ++
          stateUpdate.flatMap(_.leasingForAddress).foldLeft(empty) { case (r, x) =>
            val (cacheKey, rawValue) = conv.accountLeaseBalanceKeyAndValueAfter(x)
            val v                    = RemoteData.loaded(rawValue)
            diskCaches.accountLeaseBalances.set(atHeight, cacheKey.address, v)
            r ++ updateCacheIfExists(cacheKey)(v)
          } ++
          stateUpdate.flatMap(_.dataEntries).foldLeft(empty) { case (r, x) =>
            val cacheKey = conv.accountDataKey(x)
            val v        = RemoteData.loaded(conv.accountDataValueAfter(x))
            diskCaches.accountDataEntries.set(atHeight, (cacheKey.address, cacheKey.dataKey), v)
            r ++ updateCacheIfExists(cacheKey)(v)
          } ++
          stateUpdate.flatMap(_.scripts).foldLeft(empty) { case (r, x) =>
            val cacheKey = conv.accountScriptKey(x)
            val pk = setScriptTxSenderPublicKeys.getOrElse(
              x.address.toAddress(chainId),
              throw new RuntimeException("Impossible: there is a script, but no a corresponding transaction")
            )
            val v = RemoteData.loaded(toVanillaScript(x.after).map(toWeightedAccountScriptInfo(pk, _)))
            diskCaches.accountScripts.set(atHeight, cacheKey.address, v)
            r ++ updateCacheIfExists(cacheKey)(v)
          } ++
          // We have to do this, otherwise:
          // 1. A transaction could be moved to a new block during NG process
          // 2. We couldn't observe it, e.g. comes in a next micro block or even a block
          // 3. So a script returns a wrong result until the next height, when we re-evaluate all scripts forcefully
          evt.transactionIds.foldLeft(empty) { case (r, txId) =>
            val cacheKey = conv.transactionIdKey(txId)
            val v        = RemoteData.loaded(atHeight)
            diskCaches.transactions.updateHeightIfExist(cacheKey.id, v)
            r ++ updateCacheIfExists(cacheKey)(v)
          }
      }
    }
  }

  private def rollback(toHeight: Height, rollback: BlockchainUpdated.Rollback)(implicit ctx: ReadWrite): AffectedTags[TagT] =
    RideRunnerStats.rollbackProcessingTime.measure {
      removeAllFromCtx(Height(toHeight + 1))

      val stateUpdate = rollback.getRollbackStateUpdate
      getAffectedTags(MemCacheKey.Height) ++
        stateUpdate.assets.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.assetKey(x)
          val v        = RemoteData.loaded(conv.assetValueAfter(cacheKey.asset, x).map(toWeightedAssetDescription))
          r ++ updateCacheIfExists(cacheKey)(v)
        } ++
        stateUpdate.balances.foldLeft(empty) { case (r, x) =>
          val (cacheKey, rawValue) = conv.accountBalanceKeyAndValueAfter(x)
          r ++ updateCacheIfExists(cacheKey)(RemoteData.loaded(rawValue))
        } ++
        stateUpdate.leasingForAddress.foldLeft(empty) { case (r, x) =>
          val (cacheKey, rawValue) = conv.accountLeaseBalanceKeyAndValueAfter(x)
          r ++ updateCacheIfExists(cacheKey)(RemoteData.loaded(rawValue))
        } ++
        stateUpdate.dataEntries.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.accountDataKey(x)
          r ++ updateCacheIfExists(cacheKey)(RemoteData.loaded(conv.accountDataValueAfter(x)))
        } ++
        stateUpdate.scripts.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.accountScriptKey(x)
          // Just a removing eliminates a complex logic of restoring the old script here. In rare cases this could lead to a slow response (once).
          r ++ removeCache(cacheKey)
        } ++
        stateUpdate.deletedAliases.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.aliasKey(x)
          r ++ updateCacheIfExists(cacheKey)(RemoteData.Absence) // Because there is only one such alias
        } ++
        rollback.removedTransactionIds.foldLeft(empty) { case (r, txId) =>
          val cacheKey = conv.transactionIdKey(txId)
          r ++ updateCacheIfExists(cacheKey)(RemoteData.Absence)
        }
    }

  /** @param events
    *   The recent events should be in the front of the list. Must contain only Appends.
    * @return
    */
  def undo(events: List[BlockchainUpdated]): AffectedTags[TagT] = db.batchedReadWrite { implicit ctx =>
    events match {
      case last :: _ =>
        log.info(s"Undo to ${last.height - 1}")
        removeAllFromCtx(Height(last.height))
        events.foldLeft(getAffectedTags(MemCacheKey.Height)) { case (r, event) =>
          val updates = event.update match {
            case Update.Append(append) =>
              log.debug(s"Undo id=${ByteStr(event.id.toByteArray)}")
              // Update a memory cache to eliminate stale reads from disk caches,
              //  when getOrFetch doesn't find a key and tries to read from the disk.
              undoCaches(append)

            case _ => empty
          }

          r ++ updates
        }

      case Nil => empty
    }
  }

  private def undoCaches(append: BlockchainUpdated.Append): AffectedTags[TagT] = {
    val txs = append.body match {
      case Body.Block(block)           => block.getBlock.transactions
      case Body.MicroBlock(microBlock) => microBlock.getMicroBlock.getMicroBlock.transactions
      case Body.Empty                  => Seq.empty
    }

    val setScriptTxSenderPublicKeys = mutable.Map.empty[Address, PublicKey]
    val withTxAffectedTags = txs.view.map(_.transaction).foldLeft(empty) { case (r, tx) =>
      tx match {
        case Transaction.WavesTransaction(tx) =>
          tx.data match {
            case Data.CreateAlias(txData) =>
              val cacheKey = conv.aliasKey(txData)
              r ++ updateCacheIfExists(cacheKey)(RemoteData.Absence)

            case Data.SetScript(_) =>
              val pk = tx.senderPublicKey.toPublicKey
              setScriptTxSenderPublicKeys.put(pk.toAddress(chainId), pk)
              r

            case _ => r
          }
        case _ => r
      }
    }

    val stateUpdate = (append.getStateUpdate +: append.transactionStateUpdates).view
    withTxAffectedTags ++
      stateUpdate.flatMap(_.assets).foldLeft(empty) { case (r, x) =>
        val cacheKey = conv.assetKey(x)
        val v        = RemoteData.loaded(conv.assetValueBefore(cacheKey.asset, x).map(toWeightedAssetDescription))
        r ++ updateCacheIfExists(cacheKey)(v)
      } ++
      stateUpdate.flatMap(_.balances).foldLeft(empty) { case (r, x) =>
        val (cacheKey, rawValue) = conv.accountBalanceKeyAndValueBefore(x)
        r ++ updateCacheIfExists(cacheKey)(RemoteData.loaded(rawValue))
      } ++
      stateUpdate.flatMap(_.leasingForAddress).foldLeft(empty) { case (r, x) =>
        val (cacheKey, rawValue) = conv.accountLeaseBalanceKeyAndValueBefore(x)
        r ++ updateCacheIfExists(cacheKey)(RemoteData.loaded(rawValue))
      } ++
      stateUpdate.flatMap(_.dataEntries).foldLeft(empty) { case (r, x) =>
        val cacheKey = conv.accountDataKey(x)
        r ++ updateCacheIfExists(cacheKey)(RemoteData.loaded(conv.accountDataValueBefore(x)))
      } ++
      stateUpdate.flatMap(_.scripts).foldLeft(empty) { case (r, x) =>
        val cacheKey = conv.accountScriptKey(x)
        val pk = setScriptTxSenderPublicKeys.getOrElse(
          x.address.toAddress(chainId),
          throw new RuntimeException("Impossible: there is a script, but no a corresponding transaction")
        )
        val v = RemoteData.loaded(toVanillaScript(x.before).map(toWeightedAccountScriptInfo(pk, _)))
        r ++ updateCacheIfExists(cacheKey)(v)
      } ++
      append.transactionIds.foldLeft(empty) { case (r, txId) =>
        val cacheKey = conv.transactionIdKey(txId)
        r ++ updateCacheIfExists(cacheKey)(RemoteData.Absence)
      }
  }

  private def toWeightedAssetDescription(x: AssetDescription): WeighedAssetDescription =
    WeighedAssetDescription(x.script.fold(0)(MemCacheWeights.ofAssetScriptInfo), x)

  private def toWeightedAccountScriptInfo(pk: PublicKey, script: Script): WeighedAccountScriptInfo = {
    val estimated = Map(estimator.version -> estimate(heightUntagged, activatedFeatures, estimator, script, isAsset = false))
    WeighedAccountScriptInfo(
      scriptInfoWeight = MemCacheWeights.ofScript(script),
      accountScriptInfo = AccountScriptInfo(
        publicKey = pk,
        script = script, // See WavesEnvironment.accountScript
        verifierComplexity = estimated.maxBy { case (version, _) => version }._2.verifierComplexity,
        // "Cannot find complexity storage" if empty
        complexitiesByEstimator = estimated.map { case (version, x) =>
          version -> x.callableComplexities
        }
      )
    )
  }
}

object LazyBlockchain {
  def init[TagT](
      settings: BlockchainSettings,
      blockchainApi: BlockchainApi,
      db: RideDbAccess,
      diskCaches: DiskCaches,
      memCache: MemBlockchainDataCache,
      allTags: CacheKeyTags[TagT]
  )(implicit ctx: ReadOnly): LazyBlockchain[TagT] = {
    val blockHeaders = new BlockHeaderStorage(blockchainApi, diskCaches.blockHeaders)
    blockHeaders.load()
    new LazyBlockchain[TagT](settings, blockchainApi, db, diskCaches, blockHeaders, memCache, allTags)
  }
}
