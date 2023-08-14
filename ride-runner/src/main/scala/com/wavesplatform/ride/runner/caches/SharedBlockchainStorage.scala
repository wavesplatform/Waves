package com.wavesplatform.ride.runner.caches

import cats.syntax.option.*
import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.api.BlockchainApi
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
import com.wavesplatform.ride.runner.caches.SharedBlockchainStorage.Settings
import com.wavesplatform.ride.runner.caches.disk.DiskCaches
import com.wavesplatform.ride.runner.caches.mem.{GrpcCacheKeyConverters, MemBlockchainDataCache, MemCacheKey, MemCacheWeights}
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite, RideDbAccess}
import com.wavesplatform.ride.runner.estimate
import com.wavesplatform.ride.runner.stats.RideRunnerStats
import com.wavesplatform.ride.runner.stats.RideRunnerStats.*
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, Height, LeaseBalance}
import com.wavesplatform.transaction.TxValidationError.AliasDoesNotExist
import com.wavesplatform.utils.ScorexLogging

import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

class SharedBlockchainStorage[TagT] private (
    settings: Settings,
    allTags: CacheKeyTags[TagT],
    db: RideDbAccess,
    diskCaches: DiskCaches,
    blockchainApi: BlockchainApi
) extends ScorexLogging {
  private val blockHeaders = new BlockHeaderStorage(blockchainApi, diskCaches.blockHeaders)

  def load()(implicit ctx: ReadOnly): Unit = blockHeaders.load()

  private val memCache = new MemBlockchainDataCache(settings.memBlockchainDataCache)

  def getOrFetchBlock(atHeight: Height): Option[SignedBlockHeaderWithVrf] = db.directReadWrite { implicit ctx =>
    blockHeaders.getOrFetch(atHeight)
  }

  //  Only for tests
  private[caches] def getCachedInMem[T <: MemCacheKey](key: T): RemoteData[T#ValueT] = memCache.get(key)

  def getOrFetch[T <: MemCacheKey](key: T): Option[T#ValueT] = db.directReadWrite { implicit ctx =>
    getLatestInternal(heightUntagged, key)
  }

  private def getLatestInternal[T <: MemCacheKey](atMaxHeight: Height, key: T)(implicit ctx: ReadWrite): Option[T#ValueT] = {
    val r = key match {
      case key: MemCacheKey.AccountData =>
        // If a key doesn't exist, the lambda will block other memCache updates.
        // For any blockchain update use memCache.set first and then update a disk cache
        //   to guarantee overwrite of fetched from gRPC API data.
        memCache.getOrLoad(key) { key =>
          val cached = diskCaches.accountDataEntries.get(atMaxHeight, key)
          if (cached.loaded) cached
          else
            RemoteData
              .loaded(blockchainApi.getAccountDataEntry(key.address, key.dataKey))
              .tap { r => diskCaches.accountDataEntries.set(atMaxHeight, key, r) }
        }

      case key: MemCacheKey.Transaction =>
        memCache.getOrLoad(key) { key =>
          val cached = diskCaches.transactions.getHeight(key)
          if (cached.loaded) cached
          else
            RemoteData
              .loaded(blockchainApi.getTransactionHeight(key.id))
              .tap { r => diskCaches.transactions.setHeight(key, r) }
        }

      case key: MemCacheKey.Alias =>
        memCache.getOrLoad(key) { key =>
          val cached = diskCaches.aliases.getAddress(key)
          if (cached.loaded) cached
          else
            RemoteData
              .loaded(blockchainApi.resolveAlias(key.alias))
              .tap { r => diskCaches.aliases.setAddress(atMaxHeight, key, r) }
        }

      case key: MemCacheKey.Asset =>
        memCache.getOrLoad(key) { key =>
          val cached = diskCaches.assetDescriptions.get(atMaxHeight, key)
          if (cached.loaded) cached
          else
            RemoteData
              .loaded(blockchainApi.getAssetDescription(key.asset))
              .map(toWeightedAssetDescription)
              .tap { r => diskCaches.assetDescriptions.set(atMaxHeight, key, r) }
        }

      case key: MemCacheKey.AccountBalance =>
        memCache.getOrLoad(key) { key =>
          val cached = diskCaches.accountBalances.get(atMaxHeight, key)
          if (cached.loaded) cached
          else
            RemoteData
              .loaded(blockchainApi.getBalance(key.address, key.asset))
              .tap { r => diskCaches.accountBalances.set(atMaxHeight, key, r) }
        }

      case key: MemCacheKey.AccountLeaseBalance =>
        memCache.getOrLoad(key) { key =>
          val cached = diskCaches.accountLeaseBalances.get(atMaxHeight, key)
          if (cached.loaded) cached
          else
            RemoteData
              .loaded(blockchainApi.getLeaseBalance(key.address))
              .map(r => LeaseBalance(r.leaseIn, r.leaseOut))
              .tap { r => diskCaches.accountLeaseBalances.set(atMaxHeight, key, r) }
        }

      case key: MemCacheKey.AccountScript =>
        memCache.getOrLoad(key) { key =>
          val cached = diskCaches.accountScripts.get(atMaxHeight, key)
          if (cached.loaded) cached
          else
            RemoteData
              .loaded(blockchainApi.getAccountScript(key.address))
              .map(Function.tupled(toWeightedAccountScriptInfo))
              .tap { r => diskCaches.accountScripts.set(atMaxHeight, key, r) }
        }

      case MemCacheKey.Height => RemoteData.loaded(Some(height)) // TODO remove this
    }

    // TODO types!
    r.asInstanceOf[RemoteData[T#ValueT]].mayBeValue
  }

  val blockchainSettings: BlockchainSettings = settings.blockchain

  val chainId = blockchainSettings.addressSchemeCharacter.toByte

  private val conv = new GrpcCacheKeyConverters(chainId)

  def height: Int = heightUntagged

  // Ride: wavesBalance, height, lastBlock
  def heightUntaggedOpt: Option[Height] = blockHeaders.latestHeight
  def heightUntagged: Height            = blockHeaders.latestHeight.getOrElse(blockchainApi.getCurrentBlockchainHeight())

  def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean] =
    db.directReadWrite { implicit ctx =>
      blockHeaders.getLocal(height).map(_.header.id() == id)
    }

  private val activatedFeatures_ = new AtomicReference(
    diskCaches
      .getActivatedFeatures()
      .orElse(RemoteData.loaded(blockchainApi.getActivatedFeatures(heightUntagged).tap(diskCaches.setActivatedFeatures)))
      .mayBeValue
      .getOrElse(throw new RuntimeException("Impossible: activated features are empty"))
  )

  private def updateFeatures(xs: Map[Short, Height]): Unit = {
    activatedFeatures_.set(xs)
    diskCaches.setActivatedFeatures(xs)
  }

  def activatedFeatures = activatedFeatures_.get()

  def resolveAlias(a: Alias): Either[ValidationError, Address] =
    getOrFetch(MemCacheKey.Alias(a)).toRight(AliasDoesNotExist(a): ValidationError)

  private def estimator: ScriptEstimator =
    EstimatorProvider.byActivatedFeatures(blockchainSettings.functionalitySettings, activatedFeatures, heightUntagged)

  def removeAllFrom(height: Height): Unit = db.batchedReadWrite { implicit ctx =>
    removeAllFromCtx(height)
  }

  private def removeAllFromCtx(height: Height)(implicit ctx: ReadWrite): Unit = {
    blockHeaders.removeFrom(height) // .tap { x => log.trace(s"removedBlocks: $x") }
    diskCaches.accountDataEntries
      .removeAllFrom(height)
//      .tap { x => log.trace(s"removed data entries: $x") }
      .foreach(memCache.remove)
    diskCaches.accountScripts
      .removeAllFrom(height)
//      .tap { x => log.trace(s"removed : $x") }
      .foreach(memCache.remove)
    diskCaches.assetDescriptions
      .removeAllFrom(height)
//      .tap { x => log.trace(s"removed : $x") }
      .foreach(memCache.remove)
    diskCaches.aliases
      .removeAllFrom(height)
//      .tap { x => log.trace(s"removed : $x") }
      .foreach(memCache.remove)
    diskCaches.accountBalances
      .removeAllFrom(height)
//      .tap { x => log.trace(s"removed : $x") }
      .foreach(memCache.remove)
    diskCaches.accountLeaseBalances
      .removeAllFrom(height)
//      .tap { x => log.trace(s"removed : $x") }
      .foreach(memCache.remove)
    diskCaches.transactions
      .removeAllFrom(height)
//      .tap { x => log.trace(s"removed transactions: $x") }
      .foreach(memCache.remove)

    val filteredFeatures = activatedFeatures.filterNot { case (_, featureHeight) => featureHeight >= height }
    if (filteredFeatures.size != activatedFeatures.size) updateFeatures(filteredFeatures)
  }

  private val empty = AffectedTags[TagT](Set.empty)
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

  private def logIfTagPresent(hint: String, key: MemCacheKey): AffectedTags[TagT] =
    getAffectedTags(s"log $hint", key)

  private def updateCacheIfExists[CacheKeyT <: MemCacheKey](hint: String, key: CacheKeyT)(v: RemoteData[CacheKeyT#ValueT]): AffectedTags[TagT] = {
    getAffectedTags(s"update $hint", key).tap { tags =>
      if (tags.isEmpty) memCache.updateIfExists(key, v) // Not yet removed from memCache, but already removed from tags
      else memCache.set(key, v)
    }
  }

  private def removeCache[CacheKeyT <: MemCacheKey](hint: String, key: CacheKeyT): AffectedTags[TagT] = {
    memCache.remove(key)
    getAffectedTags(s"remove $hint", key)
  }

  private def getAffectedTags(hint: => String, key: MemCacheKey): AffectedTags[TagT] = allTags.get(key) match {
    case None       => empty
    case Some(tags) =>
      // log.trace(s"dep $hint: ${self.xs.toList.map(_.toString).sorted.mkString("; ")}")
      AffectedTags(tags)
  }

  private def append(atHeight: Height, evt: BlockchainUpdated.Append)(implicit ctx: ReadWrite): AffectedTags[TagT] = {
    val (initialAffectedTags, txs, timer) = evt.body match {
      case Body.Block(block) =>
        (logIfTagPresent("append.height", MemCacheKey.Height), block.getBlock.transactions, RideRunnerStats.blockProcessingTime.some)
      case Body.MicroBlock(microBlock) => (empty, microBlock.getMicroBlock.getMicroBlock.transactions, RideRunnerStats.microBlockProcessingTime.some)
      case Body.Empty                  => (empty, Seq.empty, none)
    }

    timer.fold(empty) { timer =>
      timer.measure {
        if (evt.getBlock.activatedFeatures.nonEmpty)
          updateFeatures(activatedFeatures ++ evt.getBlock.activatedFeatures.map(featureId => featureId.toShort -> atHeight))

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
                    diskCaches.aliases.setAddress(atHeight, cacheKey, v)
                    r ++ updateCacheIfExists("append.alias", cacheKey)(v)

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
            diskCaches.assetDescriptions.set(atHeight, cacheKey, v)
            r ++ updateCacheIfExists("append.asset", cacheKey)(v)
          } ++
          stateUpdate.flatMap(_.balances).foldLeft(empty) { case (r, x) =>
            val (cacheKey, rawValue) = conv.accountBalanceKeyAndValueAfter(x)
            val v                    = RemoteData.loaded(rawValue)
            diskCaches.accountBalances.set(atHeight, cacheKey, v)
            r ++ updateCacheIfExists("append.accountBalance", cacheKey)(v)
          } ++
          stateUpdate.flatMap(_.leasingForAddress).foldLeft(empty) { case (r, x) =>
            val (cacheKey, rawValue) = conv.accountLeaseBalanceKeyAndValueAfter(x)
            val v                    = RemoteData.loaded(rawValue)
            diskCaches.accountLeaseBalances.set(atHeight, cacheKey, v)
            r ++ updateCacheIfExists("append.accountLeaseBalance", cacheKey)(v)
          } ++
          stateUpdate.flatMap(_.dataEntries).foldLeft(empty) { case (r, x) =>
            val cacheKey = conv.accountDataKey(x)
            val v        = RemoteData.loaded(conv.accountDataValueAfter(x))
            diskCaches.accountDataEntries.set(atHeight, cacheKey, v)
            r ++ updateCacheIfExists("append.data", cacheKey)(v)
          } ++
          stateUpdate.flatMap(_.scripts).foldLeft(empty) { case (r, x) =>
            val cacheKey = conv.accountScriptKey(x)
            val pk = setScriptTxSenderPublicKeys.getOrElse(
              x.address.toAddress(chainId),
              throw new RuntimeException("Impossible: there is a script, but no a corresponding transaction")
            )
            val v = RemoteData.loaded(toVanillaScript(x.after).map(toWeightedAccountScriptInfo(pk, _)))
            diskCaches.accountScripts.set(atHeight, cacheKey, v)
            r ++ updateCacheIfExists("append.accountScript", cacheKey)(v)
          } ++
          // We have to do this, otherwise:
          // 1. A transaction could be moved to a new block during NG process
          // 2. We couldn't observe it, e.g. comes in a next micro block or even a block
          // 3. So a script returns a wrong result until the next height, when we re-evaluate all scripts forcefully
          evt.transactionIds.foldLeft(empty) { case (r, txId) =>
            val cacheKey = conv.transactionIdKey(txId)
            val v        = RemoteData.loaded(atHeight)
            diskCaches.transactions.updateHeightIfExist(cacheKey, v)
            r ++ updateCacheIfExists("append.transaction", cacheKey)(v)
          }
      }
    }
  }

  private def rollback(toHeight: Height, rollback: BlockchainUpdated.Rollback)(implicit ctx: ReadWrite): AffectedTags[TagT] =
    RideRunnerStats.rollbackProcessingTime.measure {
      removeAllFromCtx(Height(toHeight + 1))

      val stateUpdate = rollback.getRollbackStateUpdate
      logIfTagPresent("rollback.height", MemCacheKey.Height) ++
        stateUpdate.assets.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.assetKey(x)
          val v        = RemoteData.loaded(conv.assetValueAfter(cacheKey.asset, x).map(toWeightedAssetDescription))
          r ++ updateCacheIfExists("rollback.asset", cacheKey)(v)
        } ++
        stateUpdate.balances.foldLeft(empty) { case (r, x) =>
          val (cacheKey, rawValue) = conv.accountBalanceKeyAndValueAfter(x)
          r ++ updateCacheIfExists("rollback.accountBalance", cacheKey)(RemoteData.loaded(rawValue))
        } ++
        stateUpdate.leasingForAddress.foldLeft(empty) { case (r, x) =>
          val (cacheKey, rawValue) = conv.accountLeaseBalanceKeyAndValueAfter(x)
          r ++ updateCacheIfExists("rollback.accountLeaseBalance", cacheKey)(RemoteData.loaded(rawValue))
        } ++
        stateUpdate.dataEntries.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.accountDataKey(x)
          r ++ updateCacheIfExists("rollback.data", cacheKey)(RemoteData.loaded(conv.accountDataValueAfter(x)))
        } ++
        stateUpdate.scripts.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.accountScriptKey(x)
          // Just a removing eliminates a complex logic of restoring the old script here. In rare cases this could lead to a slow response (once).
          r ++ removeCache("rollback.accountScript", cacheKey)
        } ++
        stateUpdate.deletedAliases.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.aliasKey(x)
          r ++ updateCacheIfExists("rollback.alias", cacheKey)(RemoteData.Absence) // Because there is only one such alias
        } ++
        rollback.removedTransactionIds.foldLeft(empty) { case (r, txId) =>
          val cacheKey = conv.transactionIdKey(txId)
          r ++ updateCacheIfExists("rollback.transaction", cacheKey)(RemoteData.Absence)
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
        events.foldLeft(logIfTagPresent("undo.height", MemCacheKey.Height)) { case (r, event) =>
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
              r ++ updateCacheIfExists("undo.alias", cacheKey)(RemoteData.Absence)

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
        r ++ updateCacheIfExists("undo.asset", cacheKey)(v)
      } ++
      stateUpdate.flatMap(_.balances).foldLeft(empty) { case (r, x) =>
        val (cacheKey, rawValue) = conv.accountBalanceKeyAndValueBefore(x)
        r ++ updateCacheIfExists("undo.accountBalance", cacheKey)(RemoteData.loaded(rawValue))
      } ++
      stateUpdate.flatMap(_.leasingForAddress).foldLeft(empty) { case (r, x) =>
        val (cacheKey, rawValue) = conv.accountLeaseBalanceKeyAndValueBefore(x)
        r ++ updateCacheIfExists("undo.accountLeaseBalance", cacheKey)(RemoteData.loaded(rawValue))
      } ++
      stateUpdate.flatMap(_.dataEntries).foldLeft(empty) { case (r, x) =>
        val cacheKey = conv.accountDataKey(x)
        r ++ updateCacheIfExists("undo.accountLeaseBalance", cacheKey)(RemoteData.loaded(conv.accountDataValueBefore(x)))
      } ++
      stateUpdate.flatMap(_.scripts).foldLeft(empty) { case (r, x) =>
        val cacheKey = conv.accountScriptKey(x)
        val pk = setScriptTxSenderPublicKeys.getOrElse(
          x.address.toAddress(chainId),
          throw new RuntimeException("Impossible: there is a script, but no a corresponding transaction")
        )
        val v = RemoteData.loaded(toVanillaScript(x.before).map(toWeightedAccountScriptInfo(pk, _)))
        r ++ updateCacheIfExists("undo.accountScript", cacheKey)(v)
      } ++
      append.transactionIds.foldLeft(empty) { case (r, txId) =>
        val cacheKey = conv.transactionIdKey(txId)
        r ++ updateCacheIfExists("undo.transaction", cacheKey)(RemoteData.Absence)
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

object SharedBlockchainStorage {
  def apply[TagT](
      settings: Settings,
      allTags: CacheKeyTags[TagT],
      db: RideDbAccess,
      diskCaches: DiskCaches,
      blockchainApi: BlockchainApi
  )(implicit ctx: ReadOnly): SharedBlockchainStorage[TagT] =
    new SharedBlockchainStorage[TagT](settings, allTags, db, diskCaches, blockchainApi).tap(_.load())

  case class Settings(blockchain: BlockchainSettings, memBlockchainDataCache: MemBlockchainDataCache.Settings)
}
