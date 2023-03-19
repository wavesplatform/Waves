package com.wavesplatform.ride.runner

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.meta.getSimpleName
import com.wavesplatform.ride.runner.storage.{AffectedTags, ScriptRequest, SharedBlockchainStorage}
import com.wavesplatform.state.Height
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task

import java.util.concurrent.atomic.AtomicReference

trait Processor {

  /** @return
    *   None if has no block at this height
    */
  def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean]

  def removeAllFrom(height: Height): Unit

  /** Includes removeBlocksFrom
    * @param toHeight
    */
  def forceRollbackLiquid(): Unit

  def process(event: BlockchainUpdated): Unit

  def runAffectedScripts(): Task[Unit]
}

class BlockchainProcessor(blockchainStorage: SharedBlockchainStorage[ScriptRequest], requestsService: RequestService)
    extends Processor
    with ScorexLogging {

  private val accumulatedChanges   = new AtomicReference(new ProcessResult[ScriptRequest]())
  @volatile private var lastEvents = List.empty[BlockchainUpdated]

  override def process(event: BlockchainUpdated): Unit = {
    val height = Height(event.height)
    log.info(s"Processing ${getSimpleName(event.update)} at $height")

    event.update match {
      case Update.Append(append) =>
        append.body match {
          case _: Body.Block      => lastEvents = List(event)
          case _: Body.MicroBlock => lastEvents = event :: lastEvents
          case Body.Empty         =>
        }

      case _: Update.Rollback =>
        lastEvents = lastEvents.dropWhile(x => x.height >= height && x.id != event.id)
        // It wasn't a micro fork, so we have a useful information about changed keys
        if (lastEvents.isEmpty) lastEvents = List(event)

      case Update.Empty => // Ignore
    }

    val affected = blockchainStorage.process(event)
    accumulatedChanges.accumulateAndGet(
      ProcessResult(height, affected),
      { (orig, update) => orig.combine(update) }
    )

    log.info(s"Processed $height")
  }

  override def runAffectedScripts(): Task[Unit] = {
    val last = accumulatedChanges.getAndUpdate(orig => orig.withoutAffectedTags)
    if (last.isEmpty) Task(log.info(s"[${last.newHeight}] No changes"))
    else requestsService.runAffected(last.newHeight, last.affected)
  }

  override def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean] =
    blockchainStorage.hasLocalBlockAt(height, id)

  /** Includes removeBlocksFrom
    */
  override def forceRollbackLiquid(): Unit =
    lastEvents match {
      case Nil => throw new RuntimeException("Can't force rollback one")
      case last :: _ => // a liquid block with same height
        val rollbackToHeight = Height(last.height - 1)
        val affected         = blockchainStorage.undo(lastEvents)
        if (!affected.isEmpty)
          accumulatedChanges.getAndAccumulate(
            ProcessResult(rollbackToHeight, affected),
            (orig, update) => update.combine(orig)
          )
        removeAllFrom(Height(last.height))
    }

  override def removeAllFrom(height: Height): Unit = blockchainStorage.removeAllFrom(height)
}

case class ProcessResult[TagT](
    newHeight: Int = 0,
    affectedScripts: AffectedTags[TagT] = AffectedTags(Set.empty[TagT]),
    all: Boolean = false // TODO #94 Remove
) {
  def withAffectedTags(xs: AffectedTags[TagT]): ProcessResult[TagT] = if (all) this else copy(affectedScripts = affectedScripts ++ xs)
  def combine(x: ProcessResult[TagT]): ProcessResult[TagT] =
    if (all || x.all) ProcessResult[TagT](newHeight = math.max(newHeight, x.newHeight), all = true)
    else copy(newHeight = math.max(newHeight, x.newHeight), affectedScripts = affectedScripts ++ x.affectedScripts)

  def withoutAffectedTags: ProcessResult[TagT] = copy(affectedScripts = AffectedTags.empty[TagT], all = false)

  def isEmpty: Boolean    = affectedScripts.isEmpty && !all
  def affected: Set[TagT] = affectedScripts.xs
}
