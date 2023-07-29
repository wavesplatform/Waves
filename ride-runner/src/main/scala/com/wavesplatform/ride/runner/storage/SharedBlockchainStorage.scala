package com.wavesplatform.ride.runner.storage

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
import com.wavesplatform.protobuf.transaction.PBTransactions.toVanillaScript
import com.wavesplatform.protobuf.transaction.SignedTransaction.Transaction
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite, RideDbAccess}
import com.wavesplatform.ride.runner.estimate
import com.wavesplatform.ride.runner.stats.RideRunnerStats
import com.wavesplatform.ride.runner.stats.RideRunnerStats.*
import com.wavesplatform.ride.runner.storage.SharedBlockchainStorage.Settings
import com.wavesplatform.ride.runner.storage.persistent.PersistentCaches
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.{AssetDescription, Height, LeaseBalance}
import com.wavesplatform.transaction.TxValidationError.AliasDoesNotExist
import com.wavesplatform.utils.ScorexLogging
import scalapb.json4s.JsonFormat

import java.util.concurrent.atomic.AtomicReference
import scala.util.chaining.scalaUtilChainingOps

class SharedBlockchainStorage[TagT] private (
    settings: Settings,
    allTags: CacheKeyTags[TagT],
    db: RideDbAccess,
    persistentCaches: PersistentCaches,
    blockchainApi: BlockchainApi
) extends ScorexLogging {
  private val blockHeaders = new BlockHeaderStorage(blockchainApi, persistentCaches.blockHeaders)

  def load()(implicit ctx: ReadOnly): Unit = blockHeaders.load()

  private val commonCache = new BlockchainDataCache(settings.blockchainDataCache)

  def getOrFetchBlock(atHeight: Height): Option[SignedBlockHeaderWithVrf] = db.directReadWrite { implicit ctx =>
    blockHeaders.getOrFetch(atHeight)
  }

  // TODO Bad name, we have two caches
  //  Only for tests
  def getCached[T <: CacheKey](key: T): RemoteData[T#ValueT] = commonCache.get(key)

  def getOrFetch[T <: CacheKey](key: T): Option[T#ValueT] = db.directReadWrite { implicit ctx =>
    getLatestInternal(heightUntagged, key)
  }

  private def getLatestInternal[T <: CacheKey](atMaxHeight: Height, key: T)(implicit ctx: ReadWrite): Option[T#ValueT] = {
    val r = key match {
      case key: CacheKey.AccountData =>
        // If a key doesn't exist, the lambda will block other commonCache updates.
        // For any blockchain update use commonCache.set first and then update a persistent cache
        //   to guarantee overwrite of fetched from gRPC API data.
        commonCache.getOrLoad(key) { key =>
          val cached = persistentCaches.accountDataEntries.get(atMaxHeight, key)
          if (cached.loaded) cached
          else // TODO #36 This could be updated in a different thread
            RemoteData
              .loaded(blockchainApi.getAccountDataEntry(key.address, key.dataKey))
              .tap { r =>
                persistentCaches.accountDataEntries.set(atMaxHeight, key, r)
              // numberCounter.increment()
              }
        }

      case key: CacheKey.Transaction =>
        commonCache.getOrLoad(key) { key =>
          val cached = persistentCaches.transactions.getHeight(key.id)
          if (cached.loaded) cached
          else
            RemoteData
              .loaded(blockchainApi.getTransactionHeight(key.id))
              .tap { r =>
                persistentCaches.transactions.setHeight(key.id, r)
              // numberCounter.increment()
              }
        }

      case key: CacheKey.Alias =>
        commonCache.getOrLoad(key) { key =>
          val cached = persistentCaches.aliases.getAddress(key)
          if (cached.loaded) cached
          else
            RemoteData
              .loaded(blockchainApi.resolveAlias(key.alias))
              .tap { r =>
                persistentCaches.aliases.setAddress(atMaxHeight, key, r)
              // numberCounter.increment()
              }
        }

      case key: CacheKey.Asset =>
        commonCache.getOrLoad(key) { key =>
          val cached = persistentCaches.assetDescriptions.get(atMaxHeight, key.asset)
          if (cached.loaded) cached
          else
            RemoteData
              .loaded(blockchainApi.getAssetDescription(key.asset))
              .map(toWeightedAssetDescription)
              .tap { r =>
                persistentCaches.assetDescriptions.set(atMaxHeight, key.asset, r)
              // numberCounter.increment()
              }
        }

      case key: CacheKey.AccountBalance =>
        commonCache.getOrLoad(key) { key =>
          val cached = persistentCaches.accountBalances.get(atMaxHeight, (key.address, key.asset))
          if (cached.loaded) cached
          else
            RemoteData
              .loaded(blockchainApi.getBalance(key.address, key.asset))
              .tap { r =>
                persistentCaches.accountBalances.set(atMaxHeight, (key.address, key.asset), r)
              // numberCounter.increment()
              }
        }

      case key: CacheKey.AccountLeaseBalance =>
        commonCache.getOrLoad(key) { key =>
          val cached = persistentCaches.accountLeaseBalances.get(atMaxHeight, key.address)
          if (cached.loaded) cached
          else
            RemoteData
              .loaded(blockchainApi.getLeaseBalance(key.address))
              .map(r => LeaseBalance(r.leaseIn, r.leaseOut))
              .tap { r =>
                persistentCaches.accountLeaseBalances.set(atMaxHeight, key.address, r)
              // numberCounter.increment()
              }
        }

      case key: CacheKey.AccountScript =>
        commonCache
          .getOrLoad(key) { key =>
            val cached = persistentCaches.accountScripts.get(atMaxHeight, key.address)
            if (cached.loaded) cached
            else
              RemoteData
                .loaded(blockchainApi.getAccountScript(key.address))
                .map(Function.tupled(toWeightedAccountScriptInfo))
                .tap { r =>
                  persistentCaches.accountScripts.set(atMaxHeight, key.address, r)
                // numberCounter.increment()
                }
          }

      case CacheKey.Height => RemoteData.loaded(Some(height)) // TODO remove this
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
    persistentCaches
      .getActivatedFeatures()
      .orElse(RemoteData.loaded(blockchainApi.getActivatedFeatures(heightUntagged).tap(persistentCaches.setActivatedFeatures)))
      .mayBeValue
      .getOrElse(throw new RuntimeException("Impossible: activated features are empty"))
  )

  private def updateFeatures(xs: Map[Short, Height]): Unit = {
    activatedFeatures_.set(xs)
    persistentCaches.setActivatedFeatures(xs)
  }

  def activatedFeatures = activatedFeatures_.get()

  def resolveAlias(a: Alias): Either[ValidationError, Address] =
    getOrFetch(CacheKey.Alias(a)).toRight(AliasDoesNotExist(a): ValidationError)

  private def estimator: ScriptEstimator =
    EstimatorProvider.byActivatedFeatures(blockchainSettings.functionalitySettings, activatedFeatures, heightUntagged)

  def removeAllFrom(height: Height): Unit = db.batchedReadWrite { implicit ctx =>
    removeAllFromCtx(height)
  }

  private def removeAllFromCtx(height: Height)(implicit ctx: ReadWrite): Unit = {
    blockHeaders
      .removeFrom(height)
//      .tap { x => log.trace(s"removedBlocks: $x") }
    persistentCaches.accountDataEntries
      .removeAllFrom(height)
//      .tap { x => log.trace(s"removed data entries: $x") }
      .foreach(commonCache.remove)
    persistentCaches.accountScripts
      .removeAllFrom(height)
//      .tap { x => log.trace(s"removed : $x") }
      .foreach(x => commonCache.remove(CacheKey.AccountScript(x)))
    persistentCaches.assetDescriptions
      .removeAllFrom(height)
//      .tap { x => log.trace(s"removed : $x") }
      .foreach(x => commonCache.remove(CacheKey.Asset(x)))
    persistentCaches.aliases
      .removeAllFrom(height)
//      .tap { x => log.trace(s"removed : $x") }
      .foreach(commonCache.remove)
    persistentCaches.accountBalances
      .removeAllFrom(height)
//      .tap { x => log.trace(s"removed : $x") }
      .foreach(x => commonCache.remove(CacheKey.AccountBalance(x._1, x._2)))
    persistentCaches.accountLeaseBalances
      .removeAllFrom(height)
//      .tap { x => log.trace(s"removed : $x") }
      .foreach(x => commonCache.remove(CacheKey.AccountLeaseBalance(x)))
    persistentCaches.transactions
      .removeAllFrom(height)
//      .tap { x => log.trace(s"removed transactions: $x") }
      .foreach(x => commonCache.remove(CacheKey.Transaction(x)))

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

  private def logIfTagPresent(hint: String, key: CacheKey): AffectedTags[TagT] =
    runIfTagPresent(s"log $hint", key) { () }

  private def updateCacheIfPresent[CacheKeyT <: CacheKey](hint: String, key: CacheKeyT)(v: RemoteData[CacheKeyT#ValueT]): AffectedTags[TagT] =
    runIfTagPresent(s"update $hint", key) { commonCache.set(key, v) }

  private def removeCacheIfPresent[CacheKeyT <: CacheKey](hint: String, key: CacheKeyT): AffectedTags[TagT] =
    runIfTagPresent(s"remove $hint", key) { commonCache.remove(key) }

  private def runIfTagPresent(hint: => String, key: CacheKey)(f: => Unit): AffectedTags[TagT] = allTags.get(key) match {
    case None => empty
    case Some(tags) =>
      f
      // log.trace(s"dep $hint: ${self.xs.toList.map(_.toString).sorted.mkString("; ")}")
      AffectedTags(tags)
  }

  private def append(atHeight: Height, evt: BlockchainUpdated.Append)(implicit ctx: ReadWrite): AffectedTags[TagT] = {
    val (initialAffectedTags, txs, timer) = evt.body match {
      case Body.Block(block) =>
        (logIfTagPresent("append.height", CacheKey.Height), block.getBlock.transactions, RideRunnerStats.blockProcessingTime.some)
      case Body.MicroBlock(microBlock) => (empty, microBlock.getMicroBlock.getMicroBlock.transactions, RideRunnerStats.microBlockProcessingTime.some)
      case Body.Empty                  => (empty, Seq.empty, none)
    }

    timer.fold(empty) { timer =>
      timer.measure {
        if (evt.getBlock.activatedFeatures.nonEmpty)
          updateFeatures(activatedFeatures ++ evt.getBlock.activatedFeatures.map(featureId => featureId.toShort -> atHeight))

        val stateUpdate = (evt.getStateUpdate +: evt.transactionStateUpdates).view
        val txsView     = txs.view.map(_.transaction)

        initialAffectedTags ++
          stateUpdate.flatMap(_.assets).foldLeft(empty) { case (r, x) =>
            val cacheKey = conv.assetKey(x)
            val v        = RemoteData.loaded(conv.assetValueAfter(cacheKey.asset, x).map(toWeightedAssetDescription))
            persistentCaches.assetDescriptions.set(atHeight, cacheKey.asset, v)
            r ++ updateCacheIfPresent("append.asset", cacheKey)(v)
          } ++
          stateUpdate.flatMap(_.balances).foldLeft(empty) { case (r, x) =>
            val (cacheKey, rawValue) = conv.accountBalanceKeyAndValueAfter(x)
            val v                    = RemoteData.loaded(rawValue)
            persistentCaches.accountBalances.set(atHeight, (cacheKey.address, cacheKey.asset), v)
            r ++ updateCacheIfPresent("append.accountBalance", cacheKey)(v)
          } ++
          stateUpdate.flatMap(_.leasingForAddress).foldLeft(empty) { case (r, x) =>
            val (cacheKey, rawValue) = conv.accountLeaseBalanceKeyAndValueAfter(x)
            val v                    = RemoteData.loaded(rawValue)
            persistentCaches.accountLeaseBalances.set(atHeight, cacheKey.address, v)
            r ++ updateCacheIfPresent("append.accountLeaseBalance", cacheKey)(v)
          } ++
          stateUpdate.flatMap(_.dataEntries).foldLeft(empty) { case (r, x) =>
            val cacheKey = conv.accountDataKey(x)
            val v        = RemoteData.loaded(conv.accountDataValueAfter(x))
            persistentCaches.accountDataEntries.set(atHeight, cacheKey, v)
            r ++ updateCacheIfPresent("append.data", cacheKey)(v)
          } ++
          stateUpdate.flatMap(_.scripts).foldLeft(empty) { case (r, x) =>
            // We don't add new account scripts, because we haven't a public key here, only by a request
            if (x.before.isEmpty) r
            else {
              val cacheKey = conv.accountScriptKey(x)
              // We have a public key if we've loaded a script before
              val orig = commonCache.getOrLoad(cacheKey) { _ => persistentCaches.accountScripts.getLatest(cacheKey.address) }
              orig match {
                case RemoteData.Cached(orig) =>
                  val v = RemoteData.loaded(toVanillaScript(x.after).map(toWeightedAccountScriptInfo(orig.publicKey, _)))
                  if (v.mayBeValue.isEmpty) log.warn(s"There was a script, but the new value is empty: ${JsonFormat.toJson(x)}")
                  persistentCaches.accountScripts.set(atHeight, cacheKey.address, v)
                  r ++ updateCacheIfPresent("append.accountScript", cacheKey)(v)

                case RemoteData.Absence =>
                  // Removing, because we have to make a request to get the public key
                  commonCache.remove(cacheKey)
                  persistentCaches.accountScripts.removeAll(cacheKey.address)
                  r

                case RemoteData.Unknown => r
              }
            }
          } ++
          txsView
            .flatMap {
              case Transaction.WavesTransaction(tx) =>
                tx.data match {
                  case Data.CreateAlias(txData) => (txData.alias, tx.senderPublicKey).some
                  case _                        => none
                }
              case _ => none
            }
            .foldLeft(empty) { case (r, (alias, pk)) =>
              // TODO store id to use it in rollbacks
              val cacheKey = conv.aliasKey(alias)
              val v        = RemoteData.loaded(conv.aliasValue(pk).some)
              persistentCaches.aliases.setAddress(atHeight, cacheKey, v)
              r ++ updateCacheIfPresent("append.alias", cacheKey)(v)
            } ++
          // We have to do this, otherwise:
          // 1. A transaction could be moved to a new block during NG process
          // 2. We couldn't observe it, e.g. comes in a next micro block or even a block
          // 3. So a script returns a wrong result until the next height, when we re-evaluate all scripts forcefully
          evt.transactionIds.foldLeft(empty) { case (r, txId) =>
            val cacheKey = conv.transactionIdKey(txId)
            val v        = RemoteData.loaded(atHeight)
            persistentCaches.transactions.setHeight(cacheKey.id, v)
            r ++ updateCacheIfPresent("append.transaction", cacheKey)(v)
          }
      }
    }
  }

  private def rollback(toHeight: Height, rollback: BlockchainUpdated.Rollback)(implicit ctx: ReadWrite): AffectedTags[TagT] =
    RideRunnerStats.rollbackProcessingTime.measure {
      removeAllFromCtx(Height(toHeight + 1))

      val stateUpdate = rollback.getRollbackStateUpdate
      logIfTagPresent("rollback.height", CacheKey.Height) ++
        stateUpdate.assets.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.assetKey(x)
          val v        = RemoteData.loaded(conv.assetValueAfter(cacheKey.asset, x).map(toWeightedAssetDescription))
          r ++ updateCacheIfPresent("rollback.asset", cacheKey)(v)
        } ++
        stateUpdate.balances.foldLeft(empty) { case (r, x) =>
          val (cacheKey, rawValue) = conv.accountBalanceKeyAndValueAfter(x)
          r ++ updateCacheIfPresent("rollback.accountBalance", cacheKey)(RemoteData.loaded(rawValue))
        } ++
        stateUpdate.leasingForAddress.foldLeft(empty) { case (r, x) =>
          val (cacheKey, rawValue) = conv.accountLeaseBalanceKeyAndValueAfter(x)
          r ++ updateCacheIfPresent("rollback.accountLeaseBalance", cacheKey)(RemoteData.loaded(rawValue))
        } ++
        stateUpdate.dataEntries.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.accountDataKey(x)
          r ++ updateCacheIfPresent("rollback.data", cacheKey)(RemoteData.loaded(conv.accountDataValueAfter(x)))
        } ++
        stateUpdate.scripts.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.accountScriptKey(x)
          // Just a removing eliminates a complex logic here. In rare cases this could lead to a slow response (once).
          r ++ removeCacheIfPresent("rollback.accountScript", cacheKey)
        } ++
        stateUpdate.deletedAliases.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.aliasKey(x)
          r ++ removeCacheIfPresent("rollback.alias", cacheKey)
        } ++
        rollback.removedTransactionIds.foldLeft(empty) { case (r, txId) =>
          val cacheKey = conv.transactionIdKey(txId)
          r ++ removeCacheIfPresent("rollback.transaction", cacheKey)
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
        events.foldLeft(empty) { case (r, event) =>
          val updates = event.update match {
            case Update.Append(append) =>
              log.debug(s"Undo id=${ByteStr(event.id.toByteArray)}")
              // Update a memory cache to eliminate stale reads from persistent caches,
              //  when getOrFetch doesn't find a key and tries to read from the disk.
              undoCaches(append)

            case _ => empty
          }

          r ++ updates
        }

      case Nil => empty
    }
  }

  private def undoCaches(append: BlockchainUpdated.Append)(implicit ctx: ReadOnly): AffectedTags[TagT] = {
    val txs = append.body match {
      case Body.Block(block)           => block.getBlock.transactions
      case Body.MicroBlock(microBlock) => microBlock.getMicroBlock.getMicroBlock.transactions
      case Body.Empty                  => Seq.empty
    }

    val stateUpdate = (append.getStateUpdate +: append.transactionStateUpdates).view
    val txsView     = txs.view.map(_.transaction)

    logIfTagPresent("undo.height", CacheKey.Height) ++
      stateUpdate.flatMap(_.assets).foldLeft(empty) { case (r, x) =>
        val cacheKey = conv.assetKey(x)
        val v        = RemoteData.loaded(conv.assetValueBefore(cacheKey.asset, x).map(toWeightedAssetDescription))
        r ++ updateCacheIfPresent("undo.asset", cacheKey)(v)
      } ++
      stateUpdate.flatMap(_.balances).foldLeft(empty) { case (r, x) =>
        val (cacheKey, rawValue) = conv.accountBalanceKeyAndValueBefore(x)
        r ++ updateCacheIfPresent("undo.accountBalance", cacheKey)(RemoteData.loaded(rawValue))
      } ++
      stateUpdate.flatMap(_.leasingForAddress).foldLeft(empty) { case (r, x) =>
        val (cacheKey, rawValue) = conv.accountLeaseBalanceKeyAndValueBefore(x)
        r ++ updateCacheIfPresent("undo.accountLeaseBalance", cacheKey)(RemoteData.loaded(rawValue))
      } ++
      stateUpdate.flatMap(_.dataEntries).foldLeft(empty) { case (r, x) =>
        val cacheKey = conv.accountDataKey(x)
        r ++ updateCacheIfPresent("undo.accountLeaseBalance", cacheKey)(RemoteData.loaded(conv.accountDataValueBefore(x)))
      } ++
      stateUpdate.flatMap(_.scripts).foldLeft(empty) { case (r, x) =>
        val cacheKey = conv.accountScriptKey(x)
        // Just a removing eliminates a complex logic here. In rare cases this could lead to a slow response (once).
        r ++ removeCacheIfPresent("undo.accountScript", cacheKey)
      } ++
      txsView
        .flatMap {
          case Transaction.WavesTransaction(tx) =>
            tx.data match {
              case Data.CreateAlias(txData) => txData.alias.some
              case _                        => none
            }
          case _ => none
        }
        .foldLeft(empty) { case (r, alias) =>
          val cacheKey = conv.aliasKey(alias)
          r ++ removeCacheIfPresent("undo.alias", cacheKey)
        } ++
      append.transactionIds.foldLeft(empty) { case (r, txId) =>
        val cacheKey = conv.transactionIdKey(txId)
        r ++ removeCacheIfPresent("undo.transaction", cacheKey)
      }
  }

  private def toWeightedAssetDescription(x: AssetDescription): WeighedAssetDescription =
    WeighedAssetDescription(x.script.fold(0)(CacheWeights.ofAssetScriptInfo), x)

  private def toWeightedAccountScriptInfo(pk: PublicKey, script: Script): WeighedAccountScriptInfo = {
    val estimated = Map(estimator.version -> estimate(heightUntagged, activatedFeatures, estimator, script, isAsset = false))
    WeighedAccountScriptInfo(
      publicKey = pk,
      scriptInfoWeight = CacheWeights.ofScript(script),
      script = script, // See WavesEnvironment.accountScript
      verifierComplexity = estimated.maxBy { case (version, _) => version }._2.verifierComplexity,
      complexitiesByEstimator = estimated.map { case (version, x) => version -> x.callableComplexities } // "Cannot find complexity storage" if empty
    )
  }
}

object SharedBlockchainStorage {
  def apply[TagT](
      settings: Settings,
      allTags: CacheKeyTags[TagT],
      db: RideDbAccess,
      persistentCaches: PersistentCaches,
      blockchainApi: BlockchainApi
  )(implicit ctx: ReadOnly): SharedBlockchainStorage[TagT] =
    new SharedBlockchainStorage[TagT](settings, allTags, db, persistentCaches, blockchainApi).tap(_.load())

  case class Settings(blockchain: BlockchainSettings, blockchainDataCache: BlockchainDataCache.Settings)
}
