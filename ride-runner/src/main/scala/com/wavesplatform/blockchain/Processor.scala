package com.wavesplatform.blockchain

import cats.syntax.option.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.SignedTransaction.Transaction
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.ride.RequestsService
import com.wavesplatform.ride.app.RideRunnerMetrics.*
import com.wavesplatform.state.Height
import com.wavesplatform.storage.RequestKey
import com.wavesplatform.storage.actions.AffectedTags
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task

import java.util.concurrent.atomic.AtomicReference
import scala.util.chaining.scalaUtilChainingOps

trait Processor {

  /** @return
    *   None if has no block at this height
    */
  def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean]

  def removeBlocksFrom(height: Height): Unit

  /** Includes removeBlocksFrom
    * @param toHeight
    */
  def forceRollbackOne(): Unit

  def process(event: BlockchainUpdated): Unit

  def runAffectedScripts(): Task[Unit]
}

class BlockchainProcessor(blockchainStorage: SharedBlockchainData[RequestKey], requestsService: RequestsService)
    extends Processor
    with ScorexLogging {

  private val accumulatedChanges   = new AtomicReference(new ProcessResult[RequestKey]())
  @volatile private var lastEvents = List.empty[BlockchainUpdated]

  override def process(event: BlockchainUpdated): Unit = {
    val height = Height(event.height)
    accumulatedChanges.set(event.update match {
      case Update.Empty => accumulatedChanges // Ignore
      case Update.Append(append) =>
        append.body match {
          case Body.Empty         =>
          case _: Body.Block      => lastEvents = List(event)
          case _: Body.MicroBlock => lastEvents = event :: lastEvents
        }
        process(height, append)

      case Update.Rollback(rollback) =>
        lastEvents = lastEvents.dropWhile(x => x.height >= height && x.id != event.id)
        // It wasn't a micro fork, so we have a useful information about changed keys
        if (lastEvents.isEmpty) lastEvents = List(event)
        process(height, rollback)
    })

    // Update this in the end, because a service could be suddenly turned off and we won't know, that we should re-read this block
    blockchainStorage.blockHeaders.update(event)
    log.info(s"Processed $height")
  }

  private def process(h: Height, append: BlockchainUpdated.Append): ProcessResult[RequestKey] = {
    val withUpdatedHeight = {
      // Almost all scripts use the height, so we can run all of them
      // ^ It's not true, I see that unnecessary calls more than regular 10x
      val r =
        if (accumulatedChanges.newHeight == h) accumulatedChanges
        else accumulatedChanges.withAffectedTags(AffectedTags(storage.keySet.toSet))

      // TODO Affect scripts if height is increased
//      val r = accumulatedChanges.withAffectedTags(blockchainStorage.taggedHeight.affectedTags)

      r.copy(newHeight = h)
    }

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
          // TODO parallelize?
          withUpdatedHeight
            .pipe(stateUpdate.flatMap(_.assets).foldLeft(_) { case (r, x) =>
              r.withAffectedTags(blockchainStorage.assets.append(h, x))
            })
            .pipe(stateUpdate.flatMap(_.balances).foldLeft(_) { case (r, x) =>
              r.withAffectedTags(blockchainStorage.accountBalances.append(h, x))
            })
            .pipe(stateUpdate.flatMap(_.leasingForAddress).foldLeft(_) { case (r, x) =>
              r.withAffectedTags(blockchainStorage.accountLeaseBalances.append(h, x))
            })
            .pipe(stateUpdate.flatMap(_.dataEntries).foldLeft(_) { case (r, x) =>
              r.withAffectedTags(blockchainStorage.data.append(h, x))
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
                  r.withAffectedTags(blockchainStorage.accountScripts.append(h, pk, script))
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
                  r.withAffectedTags(blockchainStorage.aliases.append(h, alias, pk))
                }
            )
            // We have to do this, otherwise:
            // 1. A transaction could be moved to a new block during NG process
            // 2. We couldn't observe it, e.g. comes in a next micro block or even a block
            // 3. So a script returns a wrong result until the next height, when we re-evaluate all scripts forcefully
            .pipe(append.transactionIds.foldLeft(_) { case (r, txId) =>
              r.withAffectedTags(blockchainStorage.transactions.setHeight(txId, h))
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
        r.withAffectedTags(blockchainStorage.assets.rollback(h, x))
      })
      .pipe(stateUpdate.balances.foldLeft(_) { case (r, x) =>
        r.withAffectedTags(blockchainStorage.accountBalances.rollback(h, x))
      })
      .pipe(stateUpdate.leasingForAddress.foldLeft(_) { case (r, x) =>
        r.withAffectedTags(blockchainStorage.accountLeaseBalances.rollback(h, x))
      })
      .pipe(stateUpdate.dataEntries.foldLeft(_) { case (r, x) =>
        r.withAffectedTags(blockchainStorage.data.rollback(h, x))
      })
      .pipe(rollback.removedTransactionIds.foldLeft(_) { case (r, txId) =>
        r.withAffectedTags(blockchainStorage.transactions.remove(txId))
      })
    /* TODO #29: Will be fixed (or not) soon with a new BlockchainUpdates API
       NOTE: Ignoring, because 1) almost impossible 2) transactions return to blockchain eventually
      .pipe(stateUpdate.aliases.foldLeft(_) { case (r, x) =>
        r.withAffectedTags(blockchainStorage.aliases.rollback(h, x))
      })
      .pipe(stateUpdate.accountScripts.foldLeft(_) { case (r, x) =>
        r.withAffectedTags(blockchainStorage.accountScripts.rollback(h, x))
      }) */
  }

  override def runAffectedScripts(): Task[Unit] = {
    val height = accumulatedChanges.newHeight

    if (accumulatedChanges.affectedScripts.isEmpty) Task.now(log.debug(s"[$height] Not updated"))
    else {
      val affected = accumulatedChanges.affectedScripts
      accumulatedChanges = ProcessResult(affectedScripts = accumulatedChanges.affectedScripts -- affected)
      requestsService.runAffected(height, affected)
    }
  }

  override def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean] = blockchainStorage.blockHeaders.getLocal(height).map(_.id() == id)

  /** Includes removeBlocksFrom
    */
  override def forceRollbackOne(): Unit =
    lastEvents match {
      case Nil => throw new RuntimeException("Can't force rollback one")
      case last :: _ => // a liquid block with same height
        val rollbackToHeight = Height(last.height - 1) // -1 because we undo the lastEvent
        lastEvents.foreach { lastEvent =>
          accumulatedChanges = lastEvent.update match {
            case Update.Append(append) => undo(rollbackToHeight, ByteStr(lastEvent.id.toByteArray), append)
            case _                     => accumulatedChanges
          }
        }

        removeBlocksFrom(Height(last.height))
    }

  private def undo(h: Height, id: ByteStr, append: BlockchainUpdated.Append): ProcessResult[RequestKey] = {
    log.info(s"Undo id=$id to $h")

    // Almost all scripts use the height, so we can run all of them
    val withUpdatedHeight = accumulatedChanges.copy(newHeight = h) // TODO #31 Affect all scripts if height is increased

    val (txs, timer) = append.body match {
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
              r.withAffectedTags(blockchainStorage.assets.undoAppend(h, x))
            })
            .pipe(stateUpdate.flatMap(_.balances).foldLeft(_) { case (r, x) =>
              r.withAffectedTags(blockchainStorage.accountBalances.undoAppend(h, x))
            })
            .pipe(stateUpdate.flatMap(_.leasingForAddress).foldLeft(_) { case (r, x) =>
              r.withAffectedTags(blockchainStorage.accountLeaseBalances.undoAppend(h, x))
            })
            .pipe(stateUpdate.flatMap(_.dataEntries).foldLeft(_) { case (r, x) =>
              r.withAffectedTags(blockchainStorage.data.undoAppend(h, x))
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
                .foldLeft(_) { case (r, (pk, _)) =>
                  r.withAffectedTags(blockchainStorage.accountScripts.undoAppend(h, pk))
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
                .foldLeft(_) { case (r, (alias, _)) =>
                  r.withAffectedTags(blockchainStorage.aliases.undoAppend(h, alias))
                }
            )
            .pipe(append.transactionIds.foldLeft(_) { case (r, txId) =>
              r.withAffectedTags(blockchainStorage.transactions.remove(txId))
            })
        }
    }
  }

  override def removeBlocksFrom(height: Height): Unit = blockchainStorage.blockHeaders.removeFrom(height)
}

object BlockchainProcessor {}
// TODO #18: don't calculate affectedScripts if all scripts are affected
case class ProcessResult[TagT](
    /*allScripts: Set[Int], */
    newHeight: Int = 0,
    affectedScripts: Set[TagT] = Set.empty[TagT]
) {
  def withAffectedTags(x: AffectedTags[TagT]): ProcessResult[TagT] = copy(affectedScripts = affectedScripts ++ x.xs)
}
