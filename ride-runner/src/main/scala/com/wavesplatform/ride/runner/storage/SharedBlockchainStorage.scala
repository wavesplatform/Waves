package com.wavesplatform.ride.runner.storage

import cats.syntax.option.*
import com.wavesplatform.account.PublicKeys.EmptyPublicKey
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
import com.wavesplatform.ride.runner.stats.RideRunnerStats.*
import com.wavesplatform.ride.runner.storage.SharedBlockchainStorage.Settings
import com.wavesplatform.ride.runner.storage.persistent.PersistentCaches
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.{AccountScriptInfo, DataEntry, Height, LeaseBalance}
import com.wavesplatform.transaction.TxValidationError.AliasDoesNotExist
import com.wavesplatform.utils.ScorexLogging

import java.util.concurrent.ConcurrentHashMap
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
  def load()(implicit ctx: ReadOnly): Unit = {
    blockHeaders.load()

    // Guarantees, that Alias / Transaction don't change their address / height
    val firstSafeHeight = blockHeaders.latestHeight.fold(Height(0))(lastKnownHeight => Height(lastKnownHeight - SafeHeightOffset))

    persistentCaches.accountDataEntries.getAllKeys().foreach(tags.put(_, Set.empty))
    persistentCaches.transactions.getAllKeys(firstSafeHeight).foreach(x => tags.put(CacheKey.Transaction(x), Set.empty))
    persistentCaches.aliases.getAllKeys(firstSafeHeight).foreach(tags.put(_, Set.empty))
    persistentCaches.assetDescriptions.getAllKeys().foreach(x => tags.put(CacheKey.Asset(x), Set.empty))
    persistentCaches.accountBalances.getAllKeys().foreach(x => tags.put(CacheKey.AccountBalance(x._1, x._2), Set.empty))
    persistentCaches.accountLeaseBalances.getAllKeys().foreach(x => tags.put(CacheKey.AccountLeaseBalance(x), Set.empty))
    persistentCaches.accountScripts.getAllKeys().foreach(x => tags.put(CacheKey.AccountScript(x), Set.empty))
    log.info(s"Found ${tags.size()} keys")
  }

  def addDependent(key: CacheKey, tag: TagT): Unit = tags.compute(key, (_, origTags) => Option(origTags).getOrElse(Set.empty) + tag)

  private val commonCache = new CommonCache

  def getOrFetchBlock(atHeight: Height): Option[SignedBlockHeaderWithVrf] = db.readWrite { implicit ctx =>
    blockHeaders.getOrFetch(atHeight)
  }

  // Only for tests
  def getCached[T <: CacheKey](key: T): RemoteData[T#ValueT] = commonCache.get(key)

  def getOrFetch[T <: CacheKey](key: T): Option[T#ValueT] = db.readWrite { implicit ctx =>
    getOrFetch(heightUntagged, key)
  }

  private def getOrFetch[T <: CacheKey](atMaxHeight: Height, key: T)(implicit ctx: ReadWrite): Option[T#ValueT] =
    getLatestInternal(atMaxHeight, key)

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
              .map(toAccountScriptInfo)
              .tap { r =>
                persistentCaches.accountScripts.set(atMaxHeight, key.address, r)
              // numberCounter.increment()
              }
        }

      case _ => ???
    }

    // TODO types!
    r.asInstanceOf[RemoteData[T#ValueT]].mayBeValue
  }

  val blockchainSettings: BlockchainSettings = settings.blockchain

  val chainId = blockchainSettings.addressSchemeCharacter.toByte

  private val conv = new GrpcCacheKeyConverters(chainId)

//  val data = new AccountDataStorage[TagT](settings.caches.accountData, chainId, blockchainApi, persistentCaches.accountDataEntries)

//  val accountScripts = new AccountScriptStorage[TagT](
//    ExactWithHeightStorage.Settings(1000),
//    chainId,
//    script => Map(estimator.version -> estimate(heightUntagged, activatedFeatures, estimator, script, isAsset = false)),
//    blockchainApi,
//    persistentCaches.accountScripts
//  )

  val heightTagged = new HeightTagsStorage[TagT](heightUntagged)

  def height: Int = heightUntagged

  // Ride: wavesBalance, height, lastBlock
  def heightUntaggedOpt: Option[Height] = blockHeaders.latestHeight
  def heightUntagged: Height            = blockHeaders.latestHeight.getOrElse(blockchainApi.getCurrentBlockchainHeight())

  def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean] =
    db.readWrite { implicit ctx =>
      blockHeaders.getLocal(height).map(_.header.id() == id)
    }

  // TODO Get from blockchain updates, var
  private lazy val activatedFeatures_ =
    load[Unit, Map[Short, Height]](
      _ => persistentCaches.getActivatedFeatures(),
      _ => blockchainApi.getActivatedFeatures(heightUntagged).some,
      (_, xs) => xs.mayBeValue.foreach(persistentCaches.setActivatedFeatures)
    )(())
      .getOrElse(throw new RuntimeException("Impossible: activated features are empty"))

  def activatedFeatures = activatedFeatures_

//  val assets = new AssetStorage[TagT](ExactWithHeightStorage.Settings(1000), blockchainApi, persistentCaches.assetDescriptions)

//  val aliases = new AliasStorage[TagT](ExactWithHeightStorage.Settings(1000), chainId, blockchainApi, persistentCaches.aliases)

  def resolveAlias(a: Alias): Either[ValidationError, Address] =
    // TODO Remove readWrite!
    db.readWrite { implicit rw =>
      getOrFetch(heightUntagged, CacheKey.Alias(a)).toRight(AliasDoesNotExist(a): ValidationError)
    }

//  val accountBalances =
//    new AccountBalanceStorage[TagT](ExactWithHeightStorage.Settings(1000), chainId, blockchainApi, persistentCaches.accountBalances)

//  val accountLeaseBalances =
//    new AccountLeaseBalanceStorage[TagT](ExactWithHeightStorage.Settings(1000), chainId, blockchainApi, persistentCaches.accountLeaseBalances)

//  val transactions = new TransactionStorage[TagT](ExactWithHeightStorage.Settings(1000), blockchainApi, persistentCaches.transactions)

  private def estimator: ScriptEstimator =
    EstimatorProvider.byActivatedFeatures(blockchainSettings.functionalitySettings, activatedFeatures, heightUntagged)

  private def load[KeyT, ValueT](
      fromCache: KeyT => RemoteData[ValueT],
      fromBlockchain: KeyT => Option[ValueT],
      updateCache: (KeyT, RemoteData[ValueT]) => Unit
  )(key: KeyT): Option[ValueT] =
    fromCache(key)
      .orElse(RemoteData.loaded(fromBlockchain(key)).tap(updateCache(key, _)))
      .mayBeValue

  def removeAllFrom(height: Height): Unit = db.readWrite { implicit ctx =>
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
  }

  private val empty = AffectedTags[TagT](Set.empty)
  def process(event: BlockchainUpdated): AffectedTags[TagT] =
    db.readWrite { implicit ctx =>
      val toHeight = Height(event.height)
      val affected = event.update match {
        case Update.Empty         => empty // Ignore
        case Update.Append(evt)   => append(toHeight, evt)
        case Update.Rollback(evt) => rollback(toHeight, evt)
      }

      blockHeaders.update(event)
      affected
    }

  private def tagsOf(key: CacheKey): Option[Set[TagT]] = Option(tags.get(key))

  def append(atHeight: Height, evt: BlockchainUpdated.Append)(implicit ctx: ReadWrite): AffectedTags[TagT] = {
    val (txs, timer) = evt.body match {
      case Body.Block(block)           => (block.getBlock.transactions, blockProcessingTime.some)
      case Body.MicroBlock(microBlock) => (microBlock.getMicroBlock.getMicroBlock.transactions, microBlockProcessingTime.some)
      case Body.Empty                  => (Seq.empty, none)
    }

    timer.fold(empty) { timer =>
      timer.measure {
        val stateUpdate = (evt.getStateUpdate +: evt.transactionStateUpdates).view
        val txsView     = txs.view.map(_.transaction)

        AffectedTags(tagsOf(CacheKey.Height).getOrElse(Set.empty)).logDep("height") ++
          stateUpdate.flatMap(_.assets).foldLeft(empty) { case (r, x) =>
            val cacheKey = conv.assetKey(x)
            r ++ tagsOf(cacheKey).fold(empty) { tags =>
              val v = RemoteData.loaded(conv.assetValue(cacheKey.asset, x))
              commonCache.set(cacheKey, v)
              persistentCaches.assetDescriptions.set(atHeight, cacheKey.asset, v)
              AffectedTags(tags).logDep("asset")
            }
          } ++
          stateUpdate.flatMap(_.balances).foldLeft(empty) { case (r, x) =>
            val (cacheKey, rawValue) = conv.accountBalanceKeyAndValue(x)
            r ++ tagsOf(cacheKey).fold(empty) { tags =>
              val v = RemoteData.loaded(rawValue)
              commonCache.set(cacheKey, v)
              persistentCaches.accountBalances.set(atHeight, (cacheKey.address, cacheKey.asset), v)
              AffectedTags(tags).logDep("accountBalance")
            }
          } ++
          stateUpdate.flatMap(_.leasingForAddress).foldLeft(empty) { case (r, x) =>
            val (cacheKey, rawValue) = conv.accountLeaseBalanceKeyAndValue(x)
            r ++ tagsOf(cacheKey).fold(empty) { tags =>
              val v = RemoteData.loaded(rawValue)
              commonCache.set(cacheKey, v)
              persistentCaches.accountLeaseBalances.set(atHeight, cacheKey.address, v)
              AffectedTags(tags).logDep("accountLeaseBalance")
            }
          } ++
          stateUpdate.flatMap(_.dataEntries).foldLeft(empty) { case (r, x) =>
            val cacheKey = conv.accountDataKey(x)
            r ++ tagsOf(cacheKey).fold(empty) { tags =>
              val v = RemoteData.loaded(conv.accountDataValue(x))
              commonCache.set(cacheKey, v)
              persistentCaches.accountDataEntries.set(atHeight, cacheKey, v)
              AffectedTags(tags).logDep("data")
            }
          } ++
          stateUpdate.flatMap(_.scripts).foldLeft(empty) { case (r, x) =>
            val cacheKey = conv.accountScriptKey(x)
            r ++ tagsOf(cacheKey).fold(empty) { tags =>
              val v = RemoteData.loaded(toVanillaScript(x.after).map(toAccountScriptInfo))
              commonCache.set(cacheKey, v)
              persistentCaches.accountScripts.set(atHeight, cacheKey.address, v)
              AffectedTags(tags).logDep("accountScript")
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
              r ++ tagsOf(cacheKey).fold(empty) { tags =>
                val v = RemoteData.loaded(conv.aliasValue(pk).some)
                commonCache.set(cacheKey, v)
                persistentCaches.aliases.setAddress(atHeight, cacheKey, v)
                AffectedTags(tags).logDep("alias")
              }
            } ++
          // We have to do this, otherwise:
          // 1. A transaction could be moved to a new block during NG process
          // 2. We couldn't observe it, e.g. comes in a next micro block or even a block
          // 3. So a script returns a wrong result until the next height, when we re-evaluate all scripts forcefully
          evt.transactionIds.foldLeft(empty) { case (r, txId) =>
            val cacheKey = conv.transactionIdKey(txId)
            r ++ tagsOf(cacheKey).fold(empty) { tags =>
              val v = RemoteData.loaded(atHeight)
              commonCache.set(cacheKey, v)
              persistentCaches.transactions.setHeight(cacheKey.id, v)
              AffectedTags(tags).logDep("transaction")
            }
          }
      }
    }
  }

  private def rollback(toHeight: Height, rollback: BlockchainUpdated.Rollback)(implicit ctx: ReadWrite): AffectedTags[TagT] =
    rollbackProcessingTime.measure {
      removeAllFrom(Height(toHeight + 1))

      val stateUpdate = rollback.getRollbackStateUpdate
      AffectedTags(tagsOf(CacheKey.Height).getOrElse(Set.empty)).logDep("height") ++
        stateUpdate.assets.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.assetKey(x)
          r ++ tagsOf(cacheKey).fold(empty) { tags =>
            val v = RemoteData.loaded(conv.assetValue(cacheKey.asset, x))
            commonCache.set(cacheKey, v)
            persistentCaches.assetDescriptions.set(toHeight, cacheKey.asset, v)
            AffectedTags(tags).logDep("asset")
          }
        } ++
        stateUpdate.balances.foldLeft(empty) { case (r, x) =>
          val (cacheKey, rawValue) = conv.accountBalanceKeyAndValue(x)
          r ++ tagsOf(cacheKey).fold(empty) { tags =>
            val v = RemoteData.loaded(rawValue)
            commonCache.set(cacheKey, v)
            persistentCaches.accountBalances.set(toHeight, (cacheKey.address, cacheKey.asset), v)
            AffectedTags(tags).logDep("accountBalance")
          }
        } ++
        stateUpdate.leasingForAddress.foldLeft(empty) { case (r, x) =>
          val (cacheKey, rawValue) = conv.accountLeaseBalanceKeyAndValue(x)
          r ++ tagsOf(cacheKey).fold(empty) { tags =>
            val v = RemoteData.loaded(rawValue)
            commonCache.set(cacheKey, v)
            persistentCaches.accountLeaseBalances.set(toHeight, cacheKey.address, v)
            AffectedTags(tags).logDep("accountLeaseBalance")
          }
        } ++
        stateUpdate.dataEntries.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.accountDataKey(x)
          r ++ tagsOf(cacheKey).fold(empty) { tags =>
            val v = RemoteData.loaded(conv.accountDataValue(x))
            commonCache.set(cacheKey, v)
            persistentCaches.accountDataEntries.set(toHeight, cacheKey, v)
            AffectedTags(tags).logDep("data")
          }
        } ++
        stateUpdate.deletedAliases.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.aliasKey(x)
          r ++ tagsOf(cacheKey).fold(empty) { tags =>
            persistentCaches.aliases.setAddress(toHeight, cacheKey, RemoteData.Unknown)
            commonCache.remove(cacheKey)
            AffectedTags(tags).logDep("alias")
          }
        } ++
        rollback.removedTransactionIds.foldLeft(empty) { case (r, txId) =>
          val cacheKey = conv.transactionIdKey(txId)
          r ++ tagsOf(cacheKey).fold(empty) { tags =>
            persistentCaches.transactions.setHeight(cacheKey.id, RemoteData.Unknown)
            commonCache.remove(cacheKey)
            AffectedTags(tags).logDep("transaction")
          }
        } ++
        stateUpdate.scripts.foldLeft(empty) { case (r, x) =>
          val cacheKey = conv.accountScriptKey(x)
          r ++ tagsOf(cacheKey).fold(empty) { tags =>
            val v = RemoteData.loaded(toVanillaScript(x.after).map(toAccountScriptInfo))
            commonCache.set(cacheKey, v)
            persistentCaches.accountScripts.set(toHeight, cacheKey.address, v)
            AffectedTags(tags).logDep("accountScript")
          }
        }
    }

  def undo(events: List[BlockchainUpdated]): AffectedTags[TagT] = db.readWrite { implicit ctx =>
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

    AffectedTags(tagsOf(CacheKey.Height).getOrElse(Set.empty)).logDep("height") ++
      stateUpdate.flatMap(_.assets).foldLeft(empty) { case (r, x) =>
        val cacheKey = conv.assetKey(x)
        r ++ tagsOf(cacheKey).fold(empty) { tags =>
          x.before match {
            case Some(before) => commonCache.set(cacheKey, RemoteData.loaded(conv.assetValue(cacheKey.asset, before)))
            case None         => commonCache.remove(cacheKey)
          }
          AffectedTags(tags).logDep("asset")
        }
      } ++
      stateUpdate.flatMap(_.balances).foldLeft(empty) { case (r, x) =>
        val (cacheKey, _) = conv.accountBalanceKeyAndValue(x)
        r ++ tagsOf(cacheKey).fold(empty) { tags =>
          commonCache.set(cacheKey, RemoteData.loaded(x.amountBefore))
          AffectedTags(tags).logDep("accountBalance")
        }
      } ++
      stateUpdate.flatMap(_.leasingForAddress).foldLeft(empty) { case (r, x) =>
        val (cacheKey, _) = conv.accountLeaseBalanceKeyAndValue(x)
        r ++ tagsOf(cacheKey).fold(empty) { tags =>
          commonCache.set(cacheKey, RemoteData.loaded(LeaseBalance(x.inBefore, x.outBefore)))
          AffectedTags(tags).logDep("accountLeaseBalance")
        }
      } ++
      stateUpdate.flatMap(_.dataEntries).foldLeft(empty) { case (r, x) =>
        val cacheKey = conv.accountDataKey(x)
        r ++ tagsOf(cacheKey).fold(empty) { tags =>
          x.dataEntryBefore match {
            case Some(before) => commonCache.set[CacheKey.AccountData, DataEntry[?]](cacheKey, RemoteData.loaded(conv.accountDataValue(before)))
            case None         => commonCache.remove(cacheKey)
          }
          AffectedTags(tags).logDep("data")
        }
      } ++
      stateUpdate.flatMap(_.scripts).foldLeft(empty) { case (r, x) =>
        val cacheKey = conv.accountScriptKey(x)
        r ++ tagsOf(cacheKey).fold(empty) { tags =>
          if (x.before.isEmpty) commonCache.remove(cacheKey)
          else {
            val v = toVanillaScript(x.before).map(toAccountScriptInfo)
            commonCache.set(cacheKey, RemoteData.loaded(v))
          }
          AffectedTags(tags).logDep("script")
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
          r ++ tagsOf(cacheKey).fold(empty) { tags =>
            commonCache.remove(cacheKey)
            AffectedTags(tags).logDep("alias")
          }
        } ++
      append.transactionIds.foldLeft(empty) { case (r, txId) =>
        val cacheKey = conv.transactionIdKey(txId)
        r ++ tagsOf(cacheKey).fold(empty) { tags =>
          commonCache.remove(cacheKey)
          AffectedTags(tags).logDep("transaction")
        }
      }
  }

  private final implicit class AffectedTagsOps(self: AffectedTags[TagT]) {
    def logDep(name: String): AffectedTags[TagT] = {
      // if (!self.isEmpty) log.trace(s"$name deps: ${self.xs.toList.map(_.toString).sorted.mkString("; ")}")
      self
    }
  }

  private def toAccountScriptInfo(script: Script): AccountScriptInfo = {
    val estimated = Map(estimator.version -> estimate(heightUntagged, activatedFeatures, estimator, script, isAsset = false))
    AccountScriptInfo(
      publicKey = EmptyPublicKey,
      script = script, // See WavesEnvironment.accountScript
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

  case class Settings(blockchain: BlockchainSettings, cacheSize: Int)
}
