package com.wavesplatform.blockchain

import cats.syntax.option.*
import com.wavesplatform.account.Address
import com.wavesplatform.api.http.ApiError.CustomValidationError
import com.wavesplatform.api.http.ApiException
import com.wavesplatform.api.http.utils.UtilsApiRoute
import com.wavesplatform.blockchain.BlockchainProcessor.{ProcessResult, Settings}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.SignedTransaction.Transaction
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.ride.app.RideRunnerMetrics.*
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.storage.RequestsStorage.RequestKey
import com.wavesplatform.storage.actions.{AppendResult, RollbackResult}
import com.wavesplatform.storage.{DataKey, RequestsStorage}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import monix.execution.Scheduler
import play.api.libs.json.JsObject

import scala.collection.concurrent.TrieMap
import scala.util.chaining.scalaUtilChainingOps

trait Processor {

  /** @return
    *   None if has no block at this height
    */
  def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean]

  def removeBlocksFrom(height: Height): Unit

  /**
    * Includes removeBlocksFrom
    * @param toHeight
    */
  //def rollbackAll(toHeight: Height): Unit

  def process(event: BlockchainUpdated): Unit

  def runScripts(forceAll: Boolean = false): Task[Unit]

  def getCachedResultOrRun(address: Address, request: JsObject): Task[JsObject]
}

class BlockchainProcessor(
    settings: Settings,
    blockchainStorage: SharedBlockchainData[RequestKey],
    requestsStorage: RequestsStorage,
    runScriptsScheduler: Scheduler
) extends Processor
    with ScorexLogging {
  private val storage: TrieMap[RequestKey, RestApiScript] = {
    TrieMap.from(requestsStorage.all().map(k => (k, RestApiScript(k._1, blockchainStorage, k._2))))
  }

  @volatile private var accumulatedChanges = new ProcessResult[RequestKey]()

  override def process(event: BlockchainUpdated): Unit = {
    val height = Height(event.height)
    accumulatedChanges = event.update match {
      case Update.Empty              => accumulatedChanges // Ignore
      case Update.Append(append)     => process(height, append)
      case Update.Rollback(rollback) => process(height, rollback)
    }

    // Update this in the end, because a service could be suddenly turned off and we won't know, that we should re-read this block
    blockchainStorage.blockHeaders.update(event)
  }

  private def process(h: Height, append: BlockchainUpdated.Append): ProcessResult[RequestKey] = {
    // Almost all scripts use the height, so we can run all of them
    val withUpdatedHeight = accumulatedChanges.copy(newHeight = h) // TODO #31 Affect all scripts if height is increased

    val (txs, timer) = append.body match {
      // PBBlocks.vanilla(block.getBlock.getHeader)
      case Body.Block(block)           => (block.getBlock.transactions, blockProcessingTime.some)
      case Body.MicroBlock(microBlock) => (microBlock.getMicroBlock.getMicroBlock.transactions, microBlockProcessingTime.some)
      case Body.Empty                  => (Seq.empty, none)
    }

    timer match {
      case None => withUpdatedHeight
      case Some(timer) =>
        timer.measure {
          val stateUpdate = (append.getStateUpdate +: append.transactionStateUpdates).view
          val txsView     = txs.view.map(_.transaction)
          withUpdatedHeight
            .pipe(stateUpdate.flatMap(_.assets).foldLeft(_) { case (r, x) =>
              r.withAppendResult(blockchainStorage.assets.append(h, x))
            })
            .pipe(stateUpdate.flatMap(_.balances).foldLeft(_) { case (r, x) =>
              r.withAppendResult(blockchainStorage.accountBalances.append(h, x))
            })
            .pipe(stateUpdate.flatMap(_.leasingForAddress).foldLeft(_) { case (r, x) =>
              r.withAppendResult(blockchainStorage.accountLeaseBalances.append(h, x))
            })
            .pipe(stateUpdate.flatMap(_.dataEntries).foldLeft(_) { case (r, x) =>
              r.withAppendResult(blockchainStorage.data.append(h, x))
            })
            .pipe(
              txsView
                .flatMap {
                  case Transaction.WavesTransaction(tx) =>
                    tx.data match {
                      case Data.SetScript(txData) => (tx.senderPublicKey.toPublicKey, txData.script).some
                      case _                      => none
                    }
                  case _ => none
                }
                .foldLeft(_) { case (r, (pk, script)) =>
                  r.withAppendResult(blockchainStorage.accountScripts.append(h, pk, script))
                }
            )
            .pipe(
              txsView
                .flatMap {
                  case Transaction.WavesTransaction(tx) =>
                    tx.data match {
                      case Data.CreateAlias(txData) => (txData.alias, tx.senderPublicKey.toPublicKey).some
                      case _                        => none
                    }
                  case _ => none
                }
                .foldLeft(_) { case (r, (alias, pk)) =>
                  r.withAppendResult(blockchainStorage.aliases.append(h, alias, pk))
                }
            )
            .pipe(append.transactionIds.foldLeft(_) { case (r, txId) =>
              r.withAppendResult(blockchainStorage.transactions.setHeight(txId, h))
            })
        }
    }
  }

  private def process(h: Height, rollback: BlockchainUpdated.Rollback): ProcessResult[RequestKey] = rollbackProcessingTime.measure {
    // TODO #20 The height will be eventually > if this is a rollback, so we need to run all scripts
    // Almost all scripts use the height
    val withUpdatedHeight = accumulatedChanges.copy(newHeight = h)

    blockchainStorage.vrf.removeFrom(h + 1)

    val stateUpdate = rollback.getRollbackStateUpdate
    withUpdatedHeight
      .pipe(stateUpdate.assets.foldLeft(_) { case (r, x) =>
        r.withRollbackResult(blockchainStorage.assets.rollback(h, x))
      })
      .pipe(stateUpdate.balances.foldLeft(_) { case (r, x) =>
        r.withRollbackResult(blockchainStorage.accountBalances.rollback(h, x))
      })
      .pipe(stateUpdate.leasingForAddress.foldLeft(_) { case (r, x) =>
        r.withRollbackResult(blockchainStorage.accountLeaseBalances.rollback(h, x))
      })
      .pipe(stateUpdate.dataEntries.foldLeft(_) { case (r, x) =>
        r.withRollbackResult(blockchainStorage.data.rollback(h, x))
      })
      .pipe(rollback.removedTransactionIds.foldLeft(_) { case (r, txId) =>
        r.withRollbackResult(blockchainStorage.transactions.remove(txId))
      })
    /* TODO #29: Will be fixed (or not) soon with a new BlockchainUpdates API
       NOTE: Ignoring, because 1) almost impossible 2) transactions return to blockchain eventually
      .pipe(stateUpdate.aliases.foldLeft(_) { case (r, x) =>
        r.withRollbackResult(blockchainStorage.aliases.rollback(h, x))
      })
      .pipe(stateUpdate.accountScripts.foldLeft(_) { case (r, x) =>
        r.withRollbackResult(blockchainStorage.accountScripts.rollback(h, x))
      }) */
  }

  override def runScripts(forceAll: Boolean = false): Task[Unit] = {
    val height = accumulatedChanges.newHeight
    if (accumulatedChanges.uncertainKeys.nonEmpty) {
      log.debug(s"Getting data for keys: ${accumulatedChanges.uncertainKeys.toVector.map(_.toString).sorted.mkString(", ")}")
      accumulatedChanges.uncertainKeys.foreach(_.reload(height))
    }

    // log.info(
    //   s"==> $h >= $lastHeightAtStart, started: $started, affectedScripts: ${curr.affectedScripts}, batchedEvents for heights: {${batchedEvents
    //     .map(_.getUpdate.height)
    //     .mkString(", ")}}"
    // )

    val xs =
      if (forceAll) storage.values
      else if (accumulatedChanges.affectedScripts.isEmpty) {
        log.debug(s"[$height] Not updated")
        Nil
      } else accumulatedChanges.affectedScripts.flatMap(storage.get)

    // Don't clean all affected scripts, because not all scripts could be added to the storage on the moment of runScripts.
    // See getCachedResultOrRun: it takes some time to run a script and later add it to the storage.
    accumulatedChanges = ProcessResult(affectedScripts = accumulatedChanges.affectedScripts -- xs.map(_.key))

    Task
      .parTraverseUnordered(xs)(runScript(_, hasCaches = true))
      .as(())
      .executeOn(runScriptsScheduler)
  }

  override def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean] = blockchainStorage.blockHeaders.getLocal(height).map(_.id() == id)

  override def removeBlocksFrom(height: Height): Unit = blockchainStorage.blockHeaders.removeFrom(height)

  private def runScript(script: RestApiScript, hasCaches: Boolean): Task[JsObject] = Task {
    val refreshed = rideScriptRunTime.withTag("has-caches", hasCaches).measure {
      script.refreshed(settings.enableTraces, settings.evaluateScriptComplexityLimit, settings.maxTxErrorLogSize)
    }
    val key = script.key
    storage.put(key, refreshed)

    val lastResult = refreshed.lastResult
    if ((lastResult \ "error").isEmpty) {
      val complexity = lastResult.value("complexity").as[Int]
      val result     = lastResult.value("result").as[JsObject].value("value")
      log.info(f"[$key] complexity: $complexity, apiResult: $result")
    } else {
      log.info(f"[$key] failed: $lastResult")
    }
    lastResult
  }
    .tapError { e => Task(log.error(s"An error during running ${script.key}", e)) }
    .executeOn(runScriptsScheduler)

  override def getCachedResultOrRun(address: Address, request: JsObject): Task[JsObject] = {
    val key = (address, request)
    storage.get(key) match {
      case Some(r) =>
        rideScriptCacheHits.increment()
        Task.now(r.lastResult)
      case None =>
        Task {
          rideScriptCacheMisses.increment()
          requestsStorage.append(key)
          blockchainStorage.accountScripts.getUntagged(blockchainStorage.height, address)
        }.flatMap {
          case None =>
            // TODO #19 Change/move an error to an appropriate layer
            Task.raiseError(ApiException(CustomValidationError(s"Address $address is not dApp")))

          case _ => runScript(RestApiScript(address, blockchainStorage, request), hasCaches = false)
        }
    }
  }
}

object BlockchainProcessor {
  case class Settings(enableTraces: Boolean, evaluateScriptComplexityLimit: Int, maxTxErrorLogSize: Int)

  // TODO #18: don't calculate affectedScripts if all scripts are affected
  private case class ProcessResult[TagT](
      /*allScripts: Set[Int], */ newHeight: Int = 0,
      affectedScripts: Set[TagT] = Set.empty[TagT],
      uncertainKeys: Set[DataKey] = Set.empty[DataKey]
  ) {
    def withAppendResult(x: AppendResult[TagT]): ProcessResult[TagT] =
      copy(
        affectedScripts = affectedScripts ++ x.affectedTags,
        uncertainKeys = x.mayBeChangedKey.foldLeft(uncertainKeys)(_ - _)
      )

    def withRollbackResult(x: RollbackResult[TagT]): ProcessResult[TagT] =
      copy(
        affectedScripts = affectedScripts ++ x.affectedTags,
        uncertainKeys = uncertainKeys ++ x.mayBeUncertainKey
      )
  }
}

case class RestApiScript(address: Address, blockchain: Blockchain, request: JsObject, lastResult: JsObject) {
  def key: RequestKey = (address, request)

  def refreshed(trace: Boolean, evaluateScriptComplexityLimit: Int, maxTxErrorLogSize: Int): RestApiScript = {
    val result = UtilsApiRoute.evaluate(evaluateScriptComplexityLimit, blockchain, address, request, trace, maxTxErrorLogSize)
    copy(lastResult = result)
  }
}

object RestApiScript {
  def apply(address: Address, blockchainStorage: SharedBlockchainData[RequestKey], request: JsObject): RestApiScript = {
    new RestApiScript(address, new ScriptBlockchain[RequestKey](blockchainStorage, (address, request)), request, JsObject.empty)
  }
}
