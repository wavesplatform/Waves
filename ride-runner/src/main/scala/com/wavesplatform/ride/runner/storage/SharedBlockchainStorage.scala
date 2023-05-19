package com.wavesplatform.ride.runner.storage

import cats.syntax.option.*
import com.wavesplatform.account.{Address, Alias}
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
import com.wavesplatform.ride.runner.db.Heights.SafeHeightOffset
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite, RideDbAccess}
import com.wavesplatform.ride.runner.estimate
import com.wavesplatform.ride.runner.stats.RideRunnerStats
import com.wavesplatform.ride.runner.stats.RideRunnerStats.*
import com.wavesplatform.ride.runner.storage.SharedBlockchainStorage.Settings
import com.wavesplatform.ride.runner.storage.persistent.PersistentCaches
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.{AssetDescription, DataEntry, Height, LeaseBalance}
import com.wavesplatform.transaction.TxValidationError.AliasDoesNotExist
import com.wavesplatform.utils.ScorexLogging
import org.openjdk.jol.info.GraphLayout

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference
import scala.util.chaining.scalaUtilChainingOps

class SharedBlockchainStorage[TagT] private (
    settings: Settings,
    db: RideDbAccess,
    persistentCaches: PersistentCaches,
    blockchainApi: BlockchainApi
) extends ScorexLogging {
  private val blockHeaders = new BlockHeaderStorage(blockchainApi, persistentCaches.blockHeaders)

  // We can look up tags to determine if a key has been known, because tags are always in RAM
  //   not exist - we don't known this key
  //   exist, but empty - we known this key, but doesn't remember why
  //   exist and non-empty - we know, why do we need this key (but there are probably more tags)
  private val tags = new ConcurrentHashMap[CacheKey, Set[TagT]]()

  def removeTags(xs: collection.Set[TagT]): Unit = tags.replaceAll { (_: CacheKey, orig: Set[TagT]) =>
    if (orig.size >= xs.size) orig -- xs
    else orig.diff(xs)
  }

  def load()(implicit ctx: ReadOnly): Unit = {
    blockHeaders.load()

    // Guarantees, that Alias / Transaction don't change their address / height
    val firstSafeHeight = blockHeaders.latestHeight.fold(Height(0))(lastKnownHeight => Height(lastKnownHeight - SafeHeightOffset))

    persistentCaches.accountDataEntries.getAllKeys().foreach(tags.put(_, Set.empty))
    persistentCaches.accountScripts.getAllKeys().foreach(x => tags.put(CacheKey.AccountScript(x), Set.empty))
    persistentCaches.assetDescriptions.getAllKeys().foreach(x => tags.put(CacheKey.Asset(x), Set.empty))
    persistentCaches.aliases.getAllKeys(firstSafeHeight).foreach(tags.put(_, Set.empty))
    persistentCaches.accountBalances.getAllKeys().foreach(x => tags.put(CacheKey.AccountBalance(x._1, x._2), Set.empty))
    persistentCaches.accountLeaseBalances.getAllKeys().foreach(x => tags.put(CacheKey.AccountLeaseBalance(x), Set.empty))
    persistentCaches.transactions.getAllKeys(firstSafeHeight).foreach(x => tags.put(CacheKey.Transaction(x), Set.empty))
    log.info(s"Found ${tags.size()} keys")
  }

  def addDependent(key: CacheKey, tag: TagT): Unit = tags.compute(key, (_, origTags) => Option(origTags).getOrElse(Set.empty) + tag)

  private val commonCache = new CommonCache(settings.commonCache)

  def getOrFetchBlock(atHeight: Height): Option[SignedBlockHeaderWithVrf] = db.directReadWrite { implicit ctx =>
    blockHeaders.getOrFetch(atHeight)
  }

  // Only for tests
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
        commonCache.getOrLoad(key) { key =>
          val cached = persistentCaches.accountScripts.get(atMaxHeight, key.address)
          if (cached.loaded) cached
          else
            RemoteData
              .loaded(blockchainApi.getAccountScript(key.address))
              .map(toWeightedAccountScriptInfo)
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

  private def logIfTagPresent(key: CacheKey, hint: String): AffectedTags[TagT] = Option(tags.get(key)) match {
    case None       => empty
    case Some(tags) =>
      // log.trace(s"dep $hint: ${self.xs.toList.map(_.toString).sorted.mkString("; ")}")
      AffectedTags(tags)
  }

  private def runAndLogIfTagPresent(key: CacheKey, hint: String)(f: => Unit): AffectedTags[TagT] = Option(tags.get(key)) match {
    case None => empty
    case Some(tags) =>
      f
      // log.trace(s"dep $hint: ${self.xs.toList.map(_.toString).sorted.mkString("; ")}")
      AffectedTags(tags)
  }

  def append(atHeight: Height, evt: BlockchainUpdated.Append)(implicit ctx: ReadWrite): AffectedTags[TagT] = {
    val (initialAffectedTags, txs, timer) = evt.body match {
      case Body.Block(block) =>
        (logIfTagPresent(CacheKey.Height, "append.height"), block.getBlock.transactions, RideRunnerStats.blockProcessingTime.some)
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
            r ++ runAndLogIfTagPresent(cacheKey, "append.asset") {
              val v = RemoteData.loaded(conv.assetValue(cacheKey.asset, x).map(toWeightedAssetDescription))
              commonCache.set(cacheKey, v)
              persistentCaches.assetDescriptions.set(atHeight, cacheKey.asset, v)
            }
          } ++
          stateUpdate.flatMap(_.balances).foldLeft(empty) { case (r, x) =>
            val (cacheKey, rawValue) = conv.accountBalanceKeyAndValue(x)
            r ++ runAndLogIfTagPresent(cacheKey, "append.accountBalance") {
              val v = RemoteData.loaded(rawValue)
              commonCache.set(cacheKey, v)
              persistentCaches.accountBalances.set(atHeight, (cacheKey.address, cacheKey.asset), v)
            }
          } ++
          stateUpdate.flatMap(_.leasingForAddress).foldLeft(empty) { case (r, x) =>
            val (cacheKey, rawValue) = conv.accountLeaseBalanceKeyAndValue(x)
            r ++ runAndLogIfTagPresent(cacheKey, "append.accountLeaseBalance") {
              val v = RemoteData.loaded(rawValue)
              commonCache.set(cacheKey, v)
              persistentCaches.accountLeaseBalances.set(atHeight, cacheKey.address, v)
            }
          } ++
          stateUpdate.flatMap(_.dataEntries).foldLeft(empty) { case (r, x) =>
            val cacheKey = conv.accountDataKey(x)
            r ++ runAndLogIfTagPresent(cacheKey, "append.data") {
              val v = RemoteData.loaded(conv.accountDataValue(x))
              commonCache.set(cacheKey, v)
              persistentCaches.accountDataEntries.set(atHeight, cacheKey, v)
            }
          } ++
          stateUpdate.flatMap(_.scripts).foldLeft(empty) { case (r, x) =>
            val cacheKey = conv.accountScriptKey(x)
            r ++ runAndLogIfTagPresent(cacheKey, "append.accountScript") {
              val v = RemoteData.loaded(toVanillaScript(x.after).map(toWeightedAccountScriptInfo))
              commonCache.set(cacheKey, v)
              persistentCaches.accountScripts.set(atHeight, cacheKey.address, v)
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
              r ++ runAndLogIfTagPresent(cacheKey, "append.alias") {
                val v = RemoteData.loaded(conv.aliasValue(pk).some)
                commonCache.set(cacheKey, v)
                persistentCaches.aliases.setAddress(atHeight, cacheKey, v)
              }
            } ++
          // We have to do this, otherwise:
          // 1. A transaction could be moved to a new block during NG process
          // 2. We couldn't observe it, e.g. comes in a next micro block or even a block
          // 3. So a script returns a wrong result until the next height, when we re-evaluate all scripts forcefully
          evt.transactionIds.foldLeft(empty) { case (r, txId) =>
            val cacheKey = conv.transactionIdKey(txId)
            r ++ runAndLogIfTagPresent(cacheKey, "append.transaction") {
              val v = RemoteData.loaded(atHeight)
              commonCache.set(cacheKey, v)
              persistentCaches.transactions.setHeight(cacheKey.id, v)
            }
          }
      }
    }
  }

  private def rollback(toHeight: Height, rollback: BlockchainUpdated.Rollback)(implicit ctx: ReadWrite): AffectedTags[TagT] =
    RideRunnerStats.rollbackProcessingTime.measure {
      removeAllFrom(Height(toHeight + 1))

      val stateUpdate = rollback.getRollbackStateUpdate
      logIfTagPresent(CacheKey.Height, "rollback.height") ++
        stateUpdate.assets.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.assetKey(x)
          r ++ runAndLogIfTagPresent(cacheKey, "rollback.asset") {
            val v = RemoteData.loaded(conv.assetValue(cacheKey.asset, x).map(toWeightedAssetDescription))
            commonCache.set(cacheKey, v)
            persistentCaches.assetDescriptions.set(toHeight, cacheKey.asset, v)
          }
        } ++
        stateUpdate.balances.foldLeft(empty) { case (r, x) =>
          val (cacheKey, rawValue) = conv.accountBalanceKeyAndValue(x)
          r ++ runAndLogIfTagPresent(cacheKey, "rollback.accountBalance") {
            val v = RemoteData.loaded(rawValue)
            commonCache.set(cacheKey, v)
            persistentCaches.accountBalances.set(toHeight, (cacheKey.address, cacheKey.asset), v)
          }
        } ++
        stateUpdate.leasingForAddress.foldLeft(empty) { case (r, x) =>
          val (cacheKey, rawValue) = conv.accountLeaseBalanceKeyAndValue(x)
          r ++ runAndLogIfTagPresent(cacheKey, "rollback.accountLeaseBalance") {
            val v = RemoteData.loaded(rawValue)
            commonCache.set(cacheKey, v)
            persistentCaches.accountLeaseBalances.set(toHeight, cacheKey.address, v)
          }
        } ++
        stateUpdate.dataEntries.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.accountDataKey(x)
          r ++ runAndLogIfTagPresent(cacheKey, "rollback.data") {
            val v = RemoteData.loaded(conv.accountDataValue(x))
            commonCache.set(cacheKey, v)
            persistentCaches.accountDataEntries.set(toHeight, cacheKey, v)
          }
        } ++
        stateUpdate.deletedAliases.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.aliasKey(x)
          r ++ runAndLogIfTagPresent(cacheKey, "rollback.alias") {
            persistentCaches.aliases.setAddress(toHeight, cacheKey, RemoteData.Unknown)
            commonCache.remove(cacheKey)
          }
        } ++
        rollback.removedTransactionIds.foldLeft(empty) { case (r, txId) =>
          val cacheKey = conv.transactionIdKey(txId)
          r ++ runAndLogIfTagPresent(cacheKey, "rollback.transaction") {
            persistentCaches.transactions.setHeight(cacheKey.id, RemoteData.Unknown)
            commonCache.remove(cacheKey)
          }
        } ++
        stateUpdate.scripts.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.accountScriptKey(x)
          r ++ runAndLogIfTagPresent(cacheKey, "rollback.accountScript") {
            val v = RemoteData.loaded(toVanillaScript(x.after).map(toWeightedAccountScriptInfo))
            commonCache.set(cacheKey, v)
            persistentCaches.accountScripts.set(toHeight, cacheKey.address, v)
          }
        }
    }

  def undo(events: List[BlockchainUpdated]): AffectedTags[TagT] = db.batchedReadWrite { implicit ctx =>
    val filteredEvents = events.collect { case x if x.update.isAppend => x }
    filteredEvents match {
      case last :: _ =>
        log.info(s"Undo to ${last.height - 1}")
        // Remove from persistent caches, then remove from a memory cache to eliminate stale reads from persistent caches,
        //   when getOrFetch doesn't find a key and tries to read from the disk.
        removeAllFrom(Height(last.height))
        filteredEvents.foldLeft(empty) { case (r, event) =>
          val updates = event.update match {
            case Update.Append(append) =>
              log.debug(s"Undo id=${ByteStr(event.id.toByteArray)}")
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

    val stateUpdate = (append.getStateUpdate +: append.transactionStateUpdates).view
    val txsView     = txs.view.map(_.transaction)

    logIfTagPresent(CacheKey.Height, "undo.height") ++
      stateUpdate.flatMap(_.assets).foldLeft(empty) { case (r, x) =>
        val cacheKey = conv.assetKey(x)
        r ++ runAndLogIfTagPresent(cacheKey, "undo.assets") {
          x.before match {
            case Some(before) => commonCache.set(cacheKey, RemoteData.loaded(toWeightedAssetDescription(conv.assetValue(cacheKey.asset, before))))
            case None         => commonCache.remove(cacheKey)
          }
        }
      } ++
      stateUpdate.flatMap(_.balances).foldLeft(empty) { case (r, x) =>
        val (cacheKey, _) = conv.accountBalanceKeyAndValue(x)
        r ++ runAndLogIfTagPresent(cacheKey, "undo.accountBalance") {
          commonCache.set(cacheKey, RemoteData.loaded(x.amountBefore))
        }
      } ++
      stateUpdate.flatMap(_.leasingForAddress).foldLeft(empty) { case (r, x) =>
        val (cacheKey, _) = conv.accountLeaseBalanceKeyAndValue(x)
        r ++ runAndLogIfTagPresent(cacheKey, "undo.accountLeaseBalance") {
          commonCache.set(cacheKey, RemoteData.loaded(LeaseBalance(x.inBefore, x.outBefore)))
        }
      } ++
      stateUpdate.flatMap(_.dataEntries).foldLeft(empty) { case (r, x) =>
        val cacheKey = conv.accountDataKey(x)
        r ++ runAndLogIfTagPresent(cacheKey, "undo.data") {
          x.dataEntryBefore match {
            case Some(before) => commonCache.set[CacheKey.AccountData, DataEntry[?]](cacheKey, RemoteData.loaded(conv.accountDataValue(before)))
            case None         => commonCache.remove(cacheKey)
          }
        }
      } ++
      stateUpdate.flatMap(_.scripts).foldLeft(empty) { case (r, x) =>
        val cacheKey = conv.accountScriptKey(x)
        r ++ runAndLogIfTagPresent(cacheKey, "undo.script") {
          if (x.before.isEmpty) commonCache.remove(cacheKey)
          else {
            val v = toVanillaScript(x.before).map(toWeightedAccountScriptInfo)
            commonCache.set(cacheKey, RemoteData.loaded(v))
          }
        }
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
          r ++ runAndLogIfTagPresent(cacheKey, "undo.alias") {
            commonCache.remove(cacheKey)
          }
        } ++
      append.transactionIds.foldLeft(empty) { case (r, txId) =>
        val cacheKey = conv.transactionIdKey(txId)
        r ++ runAndLogIfTagPresent(cacheKey, "undo.transaction") {
          commonCache.remove(cacheKey)
        }
      }
  }

  private def toWeightedAssetDescription(x: AssetDescription): WeighedAssetDescription =
    WeighedAssetDescription(
      x.script.fold(0) { x =>
        val r = GraphLayout.parseInstance(x).totalSize()
        if (r.isValidInt) r.toInt
        else throw new ArithmeticException(s"Weight of Script overflow: $r")
      },
      x
    )

  private def toWeightedAccountScriptInfo(x: Script): WeighedAccountScriptInfo = {
    val longWeight = GraphLayout.parseInstance(x).totalSize()
    val weight =
      if (longWeight.isValidInt) longWeight.toInt
      else throw new ArithmeticException(s"Weight of AccountScriptInfo overflow: $longWeight")

    val estimated = Map(estimator.version -> estimate(heightUntagged, activatedFeatures, estimator, x, isAsset = false))
    WeighedAccountScriptInfo(
      scriptInfoWeight = weight,
      script = x, // See WavesEnvironment.accountScript
      verifierComplexity = estimated.maxBy { case (version, _) => version }._2.verifierComplexity,
      complexitiesByEstimator = estimated.map { case (version, x) => version -> x.callableComplexities } // "Cannot find complexity storage" if empty
    )
  }
}

object SharedBlockchainStorage {
  def apply[TagT](
      settings: Settings,
      db: RideDbAccess,
      persistentCaches: PersistentCaches,
      blockchainApi: BlockchainApi
  )(implicit ctx: ReadWrite): SharedBlockchainStorage[TagT] =
    new SharedBlockchainStorage[TagT](settings, db, persistentCaches, blockchainApi).tap(_.load())

  case class Settings(blockchain: BlockchainSettings, commonCache: CommonCache.Settings)
}
