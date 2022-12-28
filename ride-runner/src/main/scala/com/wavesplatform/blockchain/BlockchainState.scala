package com.wavesplatform.blockchain

import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.grpc.BlockchainApi.BlockchainUpdatesStream
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.state.Height
import com.wavesplatform.utils.ScorexLogging
import io.grpc.{Status, StatusRuntimeException}
import monix.eval.Task
import monix.execution.exceptions.UpstreamTimeoutException

// TODO #8: move. Doesn't relate to blockchain itself, move to the business domain
sealed trait BlockchainState extends Product with Serializable

object BlockchainState extends ScorexLogging {
  case class Starting(currHeight: Height, workingHeight: Height, foundDifferentBlocks: Boolean = false) extends BlockchainState {
    def withDifferentBlocks: Starting = copy(foundDifferentBlocks = true)
  }

  case class Working(height: Height) extends BlockchainState {
    def withHeight(height: Height): Working = copy(height = height)
    override def toString: String           = s"Working($height)"
  }

  case class ResolvingFork(origHeight: Height, currHeight: Height, microBlockNumber: Int) extends BlockchainState {
    def apply(event: SubscribeEvent): ResolvingFork =
      copy(
        currHeight = Height(event.getUpdate.height),
        microBlockNumber = event.getUpdate.update match {
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
      // Rollback is resolved, when the new fork height > an old fork height.
      // But we will wait a micro block or a next block, because lost transactions will highly likely appear in the next micro block
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

  def apply(
      processor: Processor,
      blockchainUpdatesStream: BlockchainUpdatesStream,
      orig: BlockchainState,
      event: WrappedEvent[SubscribeEvent]
  ): Task[BlockchainState] = {
    def forceRestart(): BlockchainState = {
      val currHeight = orig match {
        case orig: Starting      => orig.currHeight
        case Working(height)     => height
        case orig: ResolvingFork => orig.currHeight
      }
      require(currHeight > 1, "Uncaught case") // TODO

      processor.forceRollbackOne()
      blockchainUpdatesStream.start(currHeight)

      val startingHeight = Height(currHeight - 1)
      log.warn(s"Closed by a remote part, restarting from $startingHeight. Reason: $event")
      ResolvingFork(startingHeight, currHeight, microBlockNumber = 0)
    }

    event match {
      case WrappedEvent.Next(event) => apply(processor, orig, event)
      case WrappedEvent.Closed      => Task(forceRestart())
      case WrappedEvent.Failed(e) =>
        Task {
          e match {
            // RST_STREAM closed stream. HTTP/2 error code: INTERNAL_ERROR
            case e: StatusRuntimeException if e.getStatus.getCode == Status.Code.INTERNAL => forceRestart()
            case _: UpstreamTimeoutException                                              => forceRestart()

            case _ =>
              val message = e match {
                case e: StatusRuntimeException => s"Got an unhandled StatusRuntimeException, ${e.getStatus}, ${e.getTrailers.toString}"
                case _                         => "Got an unexpected error"
              }
              log.error(s"$message. Contact with developers", e)
              blockchainUpdatesStream.close()
              orig
          }
        }
    }
  }

  // Why do we require the processor instead of returning a list of actions?
  // 1. We have to know about blocks at height, so we need at least 1 dependency
  // 2. Side effects is the only reason for being of BlockchainState
  def apply(processor: Processor, orig: BlockchainState, event: SubscribeEvent): Task[BlockchainState] = {
    val start  = System.nanoTime()
    val update = event.getUpdate.update
    val h      = Height(event.getUpdate.height)

    val currBlockId = event.getUpdate.id.toByteStr
    log.info(s"$orig + ${getUpdateType(update)}(id=${currBlockId.take(5)}, h=$h)")

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
                    processor.removeBlocksFrom(h)
                    orig.withDifferentBlocks
                }

            processor.process(event.getUpdate)
            if (h >= comparedBlocks.workingHeight) {
              log.debug(s"[$h] Reached the current height, run all scripts")
              processor.runScripts().as(Working(h))
            } else Task.now(comparedBlocks.copy(currHeight = h))

          case _: Update.Rollback =>
            // It works even for micro blocks, because we have a restored version of data in event
            processor.removeBlocksFrom(Height(h + 1))
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
            processor.removeBlocksFrom(Height(h + 1))
            processor.process(event.getUpdate)
            Task.now(ResolvingFork.from(orig.height, event))

          case Update.Empty => Task.now(orig.withHeight(h))
        }

      case orig: ResolvingFork =>
        update match {
          case _: Update.Append =>
            processor.process(event.getUpdate)
            val updated = orig.apply(event)
            if (updated.isRollbackResolved) processor.runScripts().as(Working(updated.currHeight))
            else Task.now(updated)

          case _: Update.Rollback =>
            processor.removeBlocksFrom(Height(h + 1))
            processor.process(event.getUpdate)
            Task.now(orig.apply(event))

          case Update.Empty => Task.now(orig.withHeight(h))
        }
    }

    log.debug(f"Processed in ${(System.nanoTime() - start) / 1e9d}%5f s")
    r
  }

  private def getUpdateType(update: BlockchainUpdated.Update): String = update match {
    case Update.Append(append) =>
      append.body match {
        case Body.Empty         => "unknown body"
        case _: Body.Block      => "append b"
        case _: Body.MicroBlock => "append mb"
      }
    case _: Update.Rollback => "rollback to"
    case Update.Empty       => "unknown append"
  }
}
