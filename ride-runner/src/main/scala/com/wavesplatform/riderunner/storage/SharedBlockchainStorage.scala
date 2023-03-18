package com.wavesplatform.riderunner.storage

import cats.syntax.option.*
import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.features.EstimatorProvider
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.SignedTransaction.Transaction
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.riderunner.app.RideRunnerMetrics.*
import com.wavesplatform.riderunner.estimate
import com.wavesplatform.riderunner.storage.SharedBlockchainStorage.Settings
import com.wavesplatform.riderunner.storage.StorageContext.ReadWrite
import com.wavesplatform.riderunner.storage.persistent.PersistentCaches
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.Height
import com.wavesplatform.utils.ScorexLogging

import scala.util.chaining.scalaUtilChainingOps

class SharedBlockchainStorage[TagT] private (
    settings: Settings,
    storage: Storage,
    persistentCaches: PersistentCaches,
    blockchainApi: BlockchainApi,
    val blockHeaders: BlockHeaderStorage
) extends ScorexLogging {
  val blockchainSettings: BlockchainSettings = settings.blockchain

  private val chainId = blockchainSettings.addressSchemeCharacter.toByte

  val data = new AccountDataStorage[TagT](settings.caches.accountData, chainId, blockchainApi, persistentCaches.accountDataEntries)

  val accountScripts = new AccountScriptStorage[TagT](
    settings.caches.accountScript,
    chainId,
    script => Map(estimator.version -> estimate(height, activatedFeatures, estimator, script, isAsset = false)),
    blockchainApi,
    persistentCaches.accountScripts
  )

  val vrf = new VrfStorage(settings.caches.vrf, blockchainApi, persistentCaches.vrf, height)

  // Ride: wavesBalance, height, lastBlock
  def height: Int = blockHeaders.latest.height

  def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean] =
    storage.readWrite { implicit ctx =>
      blockHeaders.getLocal(height).map(_.id() == id)
    }

  // No way to get this from blockchain updates
  var activatedFeatures =
    load[Unit, Map[Short, Int]](
      _ => persistentCaches.getActivatedFeatures(),
      _ => blockchainApi.getActivatedFeatures(height).some,
      (_, xs) => xs.mayBeValue.foreach(persistentCaches.setActivatedFeatures)
    )(())
      .getOrElse(throw new RuntimeException("Impossible: activated features are empty"))

  val assets = new AssetStorage[TagT](settings.caches.asset, blockchainApi, persistentCaches.assetDescriptions)

  val aliases = new AliasStorage[TagT](settings.caches.alias, chainId, blockchainApi, persistentCaches.aliases)

  val accountBalances = new AccountBalanceStorage[TagT](settings.caches.accountBalance, chainId, blockchainApi, persistentCaches.accountBalances)

  val accountLeaseBalances =
    new AccountLeaseBalanceStorage[TagT](settings.caches.accountLease, chainId, blockchainApi, persistentCaches.accountLeaseBalances)

  val transactions = new TransactionStorage[TagT](settings.caches.transaction, blockchainApi, persistentCaches.transactions)

  private def estimator: ScriptEstimator = EstimatorProvider.byActivatedFeatures(blockchainSettings.functionalitySettings, activatedFeatures, height)

  private def load[KeyT, ValueT](
      fromCache: KeyT => RemoteData[ValueT],
      fromBlockchain: KeyT => Option[ValueT],
      updateCache: (KeyT, RemoteData[ValueT]) => Unit
  )(key: KeyT): Option[ValueT] =
    fromCache(key)
      .orElse(RemoteData.loaded(fromBlockchain(key)).tap(updateCache(key, _)))
      .mayBeValue

  def removeAllFrom(height: Height): Unit = storage.readWrite { implicit ctx =>
    // TODO #99 Remove all keys from height
    blockHeaders.removeFrom(height)
  }

  private val empty = AffectedTags[TagT](Set.empty)
  def process(event: BlockchainUpdated): AffectedTags[TagT] =
    storage.readWrite { implicit ctx =>
      val toHeight = Height(event.height)
      val affected = event.update match {
        case Update.Empty         => AffectedTags.empty[TagT] // Ignore
        case Update.Append(evt)   => append(toHeight, evt)
        case Update.Rollback(evt) => rollback(Height(height), toHeight, evt)
      }

      blockHeaders.update(event)
      log.info(s"Processed ${event.height}")

      affected
    }

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

        stateUpdate.flatMap(_.assets).foldLeft(empty) { case (r, x) =>
          r ++ assets.append(atHeight, x)
        } ++
          stateUpdate.flatMap(_.balances).foldLeft(empty) { case (r, x) =>
            r ++ accountBalances.append(atHeight, x)
          } ++
          stateUpdate.flatMap(_.leasingForAddress).foldLeft(empty) { case (r, x) =>
            r ++ accountLeaseBalances.append(atHeight, x)
          } ++
          stateUpdate.flatMap(_.dataEntries).foldLeft(empty) { case (r, x) =>
            r ++ data.append(atHeight, x)
          } ++
          txsView
            .flatMap {
              case Transaction.WavesTransaction(tx) =>
                tx.data match {
                  case Data.SetScript(txData) => (tx.senderPublicKey.toPublicKey, txData.script).some
                  case _                      => none
                }
              case _ => none
            }
            .foldLeft(empty) { case (r, (pk, script)) =>
              r ++ accountScripts.append(atHeight, pk, script)
            } ++
          txsView
            .flatMap {
              case Transaction.WavesTransaction(tx) =>
                tx.data match {
                  case Data.CreateAlias(txData) => (txData.alias, tx.senderPublicKey.toPublicKey).some
                  case _                        => none
                }
              case _ => none
            }
            .foldLeft(empty) { case (r, (alias, pk)) =>
              r ++ aliases.append(atHeight, alias, pk)
            } ++
          // We have to do this, otherwise:
          // 1. A transaction could be moved to a new block during NG process
          // 2. We couldn't observe it, e.g. comes in a next micro block or even a block
          // 3. So a script returns a wrong result until the next height, when we re-evaluate all scripts forcefully
          evt.transactionIds.foldLeft(empty) { case (r, txId) =>
            r ++ transactions.setHeight(txId, atHeight)
          }
      }
    }
  }

  private def rollback(fromHeight: Height, toHeight: Height, rollback: BlockchainUpdated.Rollback)(implicit ctx: ReadWrite): AffectedTags[TagT] =
    rollbackProcessingTime.measure {
      vrf.remove((toHeight + 1) to fromHeight)

      val stateUpdate = rollback.getRollbackStateUpdate
      stateUpdate.assets.foldLeft(empty) { case (r, x) =>
        r ++ assets.rollback(toHeight, x)
      } ++
        stateUpdate.balances.foldLeft(empty) { case (r, x) =>
          r ++ accountBalances.rollbackTo(toHeight, x)
        } ++
        stateUpdate.leasingForAddress.foldLeft(empty) { case (r, x) =>
          r ++ accountLeaseBalances.rollbackTo(toHeight, x)
        } ++
        stateUpdate.dataEntries.foldLeft(empty) { case (r, x) =>
          r ++ data.rollbackTo(toHeight, x)
        } ++
        rollback.removedTransactionIds.foldLeft(empty) { case (r, txId) =>
          r ++ transactions.remove(txId)
        }
      /* TODO #29: Will be fixed (or not) soon with a new BlockchainUpdates API
       NOTE: Ignoring, because 1) almost impossible 2) transactions return to blockchain eventually
      .pipe(stateUpdate.aliases.foldLeft(_) { case (r, x) =>
        r.withAffectedTags(blockchainStorage.aliases.rollback(h, x))
      })
      .pipe(stateUpdate.accountScripts.foldLeft(_) { case (r, x) =>
        r.withAffectedTags(blockchainStorage.accountScripts.rollback(h, x))
      }) */
    }

  def undo(events: List[BlockchainUpdated]): AffectedTags[TagT] =
    storage.readWrite { implicit ctx =>
      events.foldLeft(empty) { case (r, event) =>
        val rollbackToHeight = Height(event.height - 1) // -1 because we undo the lastEvent
        val updates = event.update match {
          case Update.Append(append) =>
            log.info(s"Undo id=${ByteStr(event.id.toByteArray)} to $rollbackToHeight")
            undoAppend(Height(event.height), append)

          case _ => empty
        }

        r ++ updates
      }
    }

  private def undoAppend(toHeight: Height, append: BlockchainUpdated.Append)(implicit ctx: ReadWrite): AffectedTags[TagT] = {
    val (txs, timer) = append.body match {
      case Body.Block(block)           => (block.getBlock.transactions, blockProcessingTime.some)
      case Body.MicroBlock(microBlock) => (microBlock.getMicroBlock.getMicroBlock.transactions, microBlockProcessingTime.some)
      case Body.Empty                  => (Seq.empty, none)
    }

    timer.fold(empty) { timer =>
      timer.measure {
        val stateUpdate = (append.getStateUpdate +: append.transactionStateUpdates).view
        val txsView     = txs.view.map(_.transaction)
        stateUpdate.flatMap(_.assets).foldLeft(empty) { case (r, x) =>
          r ++ assets.undoAppend(toHeight, x)
        } ++
          stateUpdate.flatMap(_.balances).foldLeft(empty) { case (r, x) =>
            r ++ accountBalances.undoAppend(toHeight, x)
          } ++
          stateUpdate.flatMap(_.leasingForAddress).foldLeft(empty) { case (r, x) =>
            r ++ accountLeaseBalances.undoAppend(toHeight, x)
          } ++
          stateUpdate.flatMap(_.dataEntries).foldLeft(empty) { case (r, x) =>
            r ++ data.undoAppend(toHeight, x)
          } ++
          txsView
            .flatMap {
              case Transaction.WavesTransaction(tx) =>
                tx.data match {
                  case Data.SetScript(txData) => (tx.senderPublicKey.toPublicKey, txData.script).some
                  case _                      => none
                }
              case _ => none
            }
            .foldLeft(empty) { case (r, (pk, _)) =>
              r ++ accountScripts.undoAppend(toHeight, pk)
            } ++
          txsView
            .flatMap {
              case Transaction.WavesTransaction(tx) =>
                tx.data match {
                  case Data.CreateAlias(txData) => (txData.alias, tx.senderPublicKey.toPublicKey).some
                  case _                        => none
                }
              case _ => none
            }
            .foldLeft(empty) { case (r, (alias, _)) =>
              r ++ aliases.undoAppend(toHeight, alias)
            } ++
          append.transactionIds.foldLeft(empty) { case (r, txId) =>
            r ++ transactions.remove(txId)
          }
      }
    }
  }
}

object SharedBlockchainStorage {
  def apply[TagT](
      settings: Settings,
      storage: Storage,
      persistentCaches: PersistentCaches,
      blockchainApi: BlockchainApi
  )(implicit ctx: ReadWrite): SharedBlockchainStorage[TagT] =
    new SharedBlockchainStorage[TagT](
      settings,
      storage,
      persistentCaches,
      blockchainApi,
      BlockHeaderStorage(blockchainApi, persistentCaches.blockHeaders)
    ).tap { r =>
      r.data.load()
      r.accountScripts.load()
      r.assets.load()
      r.aliases.load() // TODO ???
      r.accountBalances.load()
      r.accountLeaseBalances.load()
    }

  case class Settings(blockchain: BlockchainSettings, caches: CachesSettings)

  case class CachesSettings(
      accountData: ExactWithHeightStorage.Settings,
      vrf: ExactWithHeightStorage.Settings,
      accountScript: ExactWithHeightStorage.Settings,
      asset: ExactWithHeightStorage.Settings,
      alias: ExactWithHeightStorage.Settings,
      accountBalance: ExactWithHeightStorage.Settings,
      accountLease: ExactWithHeightStorage.Settings,
      transaction: ExactWithHeightStorage.Settings
  )
}
