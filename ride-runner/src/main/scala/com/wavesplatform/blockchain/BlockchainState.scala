package com.wavesplatform.blockchain

import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.state.Height
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task

// TODO #8: move. Doesn't relate to blockchain itself, move to the business domain
sealed trait BlockchainState extends Product with Serializable

object BlockchainState extends ScorexLogging {
  case class Starting(workingHeight: Height, foundDifferentBlocks: Boolean = false) extends BlockchainState {
    def withDifferentBlocks: Starting = copy(foundDifferentBlocks = true)
  }

  case class Working(height: Height) extends BlockchainState {
    def withHeight(height: Height): Working = copy(height = height)

    override def toString: String = s"Working($height)"
  }

  case class ResolvingFork(origHeight: Height, currHeight: Height, microBlockNumber: Int) extends BlockchainState {
    def withAction(action: SubscribeEvent): ResolvingFork =
      copy(
        currHeight = Height(action.getUpdate.height),
        microBlockNumber = action.getUpdate.update match {
          case Update.Empty       => microBlockNumber
          case _: Update.Rollback => 0
          case Update.Append(append) =>
            append.body match {
              case Body.Empty         => microBlockNumber
              case _: Body.Block      => 0
              case _: Body.MicroBlock => microBlockNumber + 1
            }
        }
      )

    def withHeight(currHeight: Height): ResolvingFork = copy(currHeight = currHeight, microBlockNumber = 0)

    def isRollbackResolved: Boolean = {
      val resolveHeight = origHeight + 1
      // Rollback is resolved, when the new fork height > an old fork height. But we will wait a micro block or a next block.
      currHeight > resolveHeight ||
      currHeight == resolveHeight && microBlockNumber >= 1
    }

    override def toString: String = s"Rollback($origHeight->$currHeight, mbn: $microBlockNumber)"
  }

  object ResolvingFork {
    def from(origHeight: Height, event: SubscribeEvent): ResolvingFork =
      new ResolvingFork(
        origHeight = origHeight,
        currHeight = Height(event.getUpdate.height),
        microBlockNumber = 0
      )
  }

  // Why do we require the processor instead of returning a list of actions?
  // 1. We have to know about blocks at height, so we need at least 1 dependency
  // 2. Side effects is the only reason for being of BlockchainState
  def apply(processor: Processor, orig: BlockchainState, event: SubscribeEvent): Task[BlockchainState] = {
    val start  = System.nanoTime()
    val update = event.getUpdate.update
    val h      = Height(event.getUpdate.height)

    val tpe = update match {
      case Update.Append(append) =>
        append.body match {
          case Body.Empty         => "unknown body"
          case _: Body.Block      => "append b"
          case _: Body.MicroBlock => "append mb"
        }
      case _: Update.Rollback => "rollback to"
      case Update.Empty       => "unknown append"
    }
    val currBlockId = event.getUpdate.id.toByteStr
    log.info(s"$orig + $tpe(id=${currBlockId.take(5)}, h=$h)")

    val ignore = Task.now(orig)
    val r = orig match {
      case orig: Starting =>
        update match {
          case _: Update.Append =>
            val comparedBlocks =
              if (orig.foundDifferentBlocks) orig
              else
                processor.hasLocalBlockAt(h, currBlockId) match {
                  case Some(true) | None => orig // true - same blocks
                  case _ =>
                    processor.removeFrom(h)
                    orig.withDifferentBlocks
                }

            processor.process(event.getUpdate)
            if (h >= comparedBlocks.workingHeight) {
              log.debug(s"[$h] Reached the current height, run all scripts")
              processor.runScripts().as(Working(h))
            } else Task.now(comparedBlocks)

          case _: Update.Rollback =>
            processor.removeFrom(Height(h + 1))
            processor.process(event.getUpdate)
            ignore

          case Update.Empty => ignore
        }

      case orig: Working =>
        update match {
          case _: Update.Append =>
            processor.process(event.getUpdate)
            processor.runScripts().as(orig.withHeight(h))

          case _: Update.Rollback =>
            processor.removeFrom(Height(h + 1))
            processor.process(event.getUpdate)
            Task.now(ResolvingFork.from(orig.height, event))

          case Update.Empty => Task.now(orig.withHeight(h))
        }

      case orig: ResolvingFork =>
        update match {
          case _: Update.Append =>
            processor.process(event.getUpdate)
            val updated = orig.withAction(event)
            if (updated.isRollbackResolved) processor.runScripts().as(Working(updated.currHeight))
            else Task.now(updated)

          case _: Update.Rollback =>
            processor.removeFrom(Height(h + 1))
            processor.process(event.getUpdate)
            Task.now(orig.withAction(event))

          case Update.Empty => Task.now(orig.withHeight(h))
        }
    }

    log.debug(f"Processed in ${(System.nanoTime() - start) / 1e9d}%5f s")
    r
  }
}
