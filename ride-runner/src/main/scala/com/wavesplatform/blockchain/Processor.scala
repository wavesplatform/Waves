package com.wavesplatform.blockchain

import cats.syntax.option.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.SignedTransaction.Transaction
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.riderunner.RequestsService
import com.wavesplatform.riderunner.app.RideRunnerMetrics.*
import com.wavesplatform.riderunner.storage.RequestKey
import com.wavesplatform.riderunner.storage.actions.AffectedTags
import com.wavesplatform.state.Height
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task

import java.util.concurrent.atomic.AtomicReference

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

    // TODO ProcessResult here is better, because we don't need to combine sets on new Block
    val affected = event.update match {
      case Update.Empty => AffectedTags.empty[RequestKey] // Ignore
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

        // TODO #20 The height will be eventually > if this is a rollback, so we need to run all scripts
        // Almost all scripts use the height
        //    val withUpdatedHeight = accumulatedChanges.copy(newHeight = h)

        process(height, rollback)
    }

    // Almost all scripts use the height, so we can run all of them
    // ^ It's not true, I see that unnecessary calls more than regular 10x
    accumulatedChanges.accumulateAndGet(
      ProcessResult(height, affected),
      { (orig, update) =>
        if (orig.newHeight < update.newHeight) orig.withAll(update.newHeight)
        else orig.combine(update)
      }
    )

    // Update this in the end, because a service could be suddenly turned off and we won't know, that we should re-read this block
    blockchainStorage.blockHeaders.update(event)
    log.info(s"Processed $height")
  }

  private val empty = AffectedTags[RequestKey](Set.empty)

  private def process(h: Height, append: BlockchainUpdated.Append): AffectedTags[RequestKey] = {
    val (txs, timer) = append.body match {
      // PBBlocks.vanilla(block.getBlock.getHeader)
      case Body.Block(block)           => (block.getBlock.transactions, blockProcessingTime.some)
      case Body.MicroBlock(microBlock) => (microBlock.getMicroBlock.getMicroBlock.transactions, microBlockProcessingTime.some)
      case Body.Empty                  => (Seq.empty, none)
    }

    timer.fold(empty) { timer =>
      timer.measure {
        val stateUpdate = (append.getStateUpdate +: append.transactionStateUpdates).view
        val txsView     = txs.view.map(_.transaction)
        // TODO parallelize?
        stateUpdate.flatMap(_.assets).foldLeft(empty) { case (r, x) =>
          r ++ blockchainStorage.assets.append(h, x)
        } ++
          stateUpdate.flatMap(_.balances).foldLeft(empty) { case (r, x) =>
            r ++ blockchainStorage.accountBalances.append(h, x)
          } ++
          stateUpdate.flatMap(_.leasingForAddress).foldLeft(empty) { case (r, x) =>
            r ++ blockchainStorage.accountLeaseBalances.append(h, x)
          } ++
          stateUpdate.flatMap(_.dataEntries).foldLeft(empty) { case (r, x) =>
            r ++ blockchainStorage.data.append(h, x)
          } ++
          (
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
                r ++ blockchainStorage.accountScripts.append(h, pk, script)
              }
          ) ++
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
              r ++ blockchainStorage.aliases.append(h, alias, pk)
            } ++
          // We have to do this, otherwise:
          // 1. A transaction could be moved to a new block during NG process
          // 2. We couldn't observe it, e.g. comes in a next micro block or even a block
          // 3. So a script returns a wrong result until the next height, when we re-evaluate all scripts forcefully
          append.transactionIds.foldLeft(empty) { case (r, txId) =>
            r ++ blockchainStorage.transactions.setHeight(txId, h)
          }
      }
    }
  }

  private def process(h: Height, rollback: BlockchainUpdated.Rollback): AffectedTags[RequestKey] = rollbackProcessingTime.measure {
    blockchainStorage.vrf.removeFrom(h + 1)

    val stateUpdate = rollback.getRollbackStateUpdate
    stateUpdate.assets.foldLeft(empty) { case (r, x) =>
      r ++ blockchainStorage.assets.rollback(h, x)
    } ++
      stateUpdate.balances.foldLeft(empty) { case (r, x) =>
        r ++ blockchainStorage.accountBalances.rollback(h, x)
      } ++
      stateUpdate.leasingForAddress.foldLeft(empty) { case (r, x) =>
        r ++ blockchainStorage.accountLeaseBalances.rollback(h, x)
      } ++
      stateUpdate.dataEntries.foldLeft(empty) { case (r, x) =>
        r ++ blockchainStorage.data.rollback(h, x)
      } ++
      rollback.removedTransactionIds.foldLeft(empty) { case (r, txId) =>
        r ++ blockchainStorage.transactions.remove(txId)
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

  override def runAffectedScripts(): Task[Unit] = {
    val last = accumulatedChanges.getAndUpdate(orig => orig.withoutAffectedTags)
    if (last.isEmpty) Task(log.info(s"[${last.newHeight}] No changes"))
    else requestsService.runAffected(last.newHeight, last.affected)
  }

  override def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean] =
    blockchainStorage.blockHeaders.getLocal(height).map(_.id() == id)

  /** Includes removeBlocksFrom
    */
  override def forceRollbackOne(): Unit =
    lastEvents match {
      case Nil => throw new RuntimeException("Can't force rollback one")
      case last :: _ => // a liquid block with same height
        val rollbackToHeight = Height(last.height - 1) // -1 because we undo the lastEvent
        val affected = lastEvents.foldLeft(empty) { case (r, lastEvent) =>
          val updates = lastEvent.update match {
            case Update.Append(append) => undo(rollbackToHeight, ByteStr(lastEvent.id.toByteArray), append)
            case _                     => empty
          }

          r ++ updates
        }

        if (!affected.isEmpty)
          accumulatedChanges.getAndAccumulate(
            ProcessResult(rollbackToHeight, affected),
            (orig, update) => update.combine(orig)
          )
        removeBlocksFrom(Height(last.height))
    }

  private def undo(h: Height, id: ByteStr, append: BlockchainUpdated.Append): AffectedTags[RequestKey] = {
    log.info(s"Undo id=$id to $h")

    // Almost all scripts use the height, so we can run all of them
//    val withUpdatedHeight = accumulatedChanges.copy(newHeight = h) // TODO #31 Affect all scripts if height is increased

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
          r ++ blockchainStorage.assets.undoAppend(h, x)
        } ++
          stateUpdate.flatMap(_.balances).foldLeft(empty) { case (r, x) =>
            r ++ blockchainStorage.accountBalances.undoAppend(h, x)
          } ++
          stateUpdate.flatMap(_.leasingForAddress).foldLeft(empty) { case (r, x) =>
            r ++ blockchainStorage.accountLeaseBalances.undoAppend(h, x)
          } ++
          stateUpdate.flatMap(_.dataEntries).foldLeft(empty) { case (r, x) =>
            r ++ blockchainStorage.data.undoAppend(h, x)
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
              r ++ blockchainStorage.accountScripts.undoAppend(h, pk)
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
              r ++ blockchainStorage.aliases.undoAppend(h, alias)
            } ++
          append.transactionIds.foldLeft(empty) { case (r, txId) =>
            r ++ blockchainStorage.transactions.remove(txId)
          }
      }
    }
  }

  override def removeBlocksFrom(height: Height): Unit = blockchainStorage.blockHeaders.removeFrom(height)
}

// TODO #18: don't calculate affectedScripts if all scripts are affected
// Use totalScripts: Int = 0, but not all scripts are affected! Probably cancel?
case class ProcessResult[TagT](
    newHeight: Int = 0,
    affectedScripts: AffectedTags[TagT] = AffectedTags(Set.empty[TagT]),
    all: Boolean = false
) {
  def withAffectedTags(xs: AffectedTags[TagT]): ProcessResult[TagT] = if (all) this else copy(affectedScripts = affectedScripts ++ xs)
  def combine(x: ProcessResult[TagT]): ProcessResult[TagT] =
    if (all || x.all) ProcessResult[TagT](newHeight = math.max(newHeight, x.newHeight), all = true)
    else copy(newHeight = math.max(newHeight, x.newHeight), affectedScripts = affectedScripts ++ x.affectedScripts)

  def withoutAffectedTags: ProcessResult[TagT] = copy(affectedScripts = AffectedTags.empty[TagT], all = false)
  def withAll(atHeight: Int): ProcessResult[TagT] = ProcessResult[TagT](
    newHeight = atHeight,
    all = true
  )

  def isEmpty: Boolean    = affectedScripts.isEmpty && !all
  def affected: Set[TagT] = affectedScripts.xs
}
