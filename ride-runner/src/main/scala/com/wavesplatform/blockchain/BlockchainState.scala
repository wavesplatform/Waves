package com.wavesplatform.blockchain

import com.wavesplatform.api.BlockchainApi.BlockchainUpdatesStream
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.state.Height
import com.wavesplatform.utils.ScorexLogging
import io.grpc.{Status, StatusRuntimeException}
import monix.eval.Task
import monix.execution.exceptions.UpstreamTimeoutException

// TODO #8: move. Doesn't relate to blockchain itself, move to the business domain
sealed trait BlockchainState extends Product with Serializable {
  def processedHeight: Height
}

object BlockchainState extends ScorexLogging {
  case class Starting(processedHeight: Height, workingHeight: Height, foundDifferentBlocks: Boolean = false) extends BlockchainState {
    def withDifferentBlocks: Starting = copy(foundDifferentBlocks = true)
  }

  case class Working(processedHeight: Height) extends BlockchainState {
    def withHeight(height: Height): Working = copy(processedHeight = height)
    override def toString: String           = s"Working($processedHeight)"
  }

  case class ResolvingFork(processedHeight: Height, processedMicroBlockNumber: Int, minResolveHeight: Height) extends BlockchainState {
    def apply(event: SubscribeEvent): ResolvingFork =
      copy(
        processedHeight = Height(event.getUpdate.height),
        processedMicroBlockNumber = event.getUpdate.update match {
          case Update.Empty       => processedMicroBlockNumber
          case _: Update.Rollback => 0
          case Update.Append(append) =>
            append.body match {
              case Body.Empty         => processedMicroBlockNumber
              case _: Body.Block      => 0
              case _: Body.MicroBlock => processedMicroBlockNumber + 1
            }
        }
      )

    def withProcessedHeight(height: Height): ResolvingFork = copy(processedHeight = height, processedMicroBlockNumber = 0)

    def isRollbackResolved: Boolean = {
      // Rollback is resolved, when the new fork height > an old fork height.
      // But we will wait a micro block or a next block, because lost transactions will highly likely appear in the next micro block
      processedHeight > minResolveHeight ||
      processedHeight == minResolveHeight && processedMicroBlockNumber >= 1
    }

    override def toString: String = s"ResolvingFork($processedHeight-..->$minResolveHeight, mbn: $processedMicroBlockNumber)"
  }

  object ResolvingFork {
    def from(rollbackHeight: Height, origForkHeight: Height): ResolvingFork =
      ResolvingFork(
        processedHeight = rollbackHeight,
        processedMicroBlockNumber = 0,
        minResolveHeight = Height(origForkHeight + 1) // A new fork can't have a lesser height
      )

    def forceRollBackOne(processedHeight: Height, workingStateHeight: Height): ResolvingFork =
      ResolvingFork(
        processedHeight = Height(processedHeight - 1),
        processedMicroBlockNumber = 0,
        minResolveHeight = workingStateHeight
      )
  }

  private val grpcStatusesToRestart = Set(
    Status.Code.INTERNAL, // RST_STREAM closed stream. HTTP/2 error code: INTERNAL_ERROR
    Status.Code.UNKNOWN // Probably it is an issue with a balancer
  )

  def apply(
      processor: Processor,
      blockchainUpdatesStream: BlockchainUpdatesStream,
      orig: BlockchainState,
      event: WrappedEvent[SubscribeEvent]
  ): Task[BlockchainState] = {
    def forceRestart(): BlockchainState = {
      val (currHeight, workingStateHeight) = orig match {
        // -1 because of how ResolvingFork.resolveFork works. TODO #62 replace origHeight in ResolvingFork by resolveHeight
        case orig: Starting      => (orig.processedHeight, orig.workingHeight)
        case Working(height)     => (height, height)
        case orig: ResolvingFork => (orig.processedHeight, orig.minResolveHeight)
      }

      // Almost impossible on MainNet and TestNet, so we can neglect this
      require(currHeight > 1, "Uncaught case. Check a connectivity to gRPC servers, remove all caches and restart the service")

      processor.forceRollbackOne()
      val r = ResolvingFork.forceRollBackOne(currHeight, workingStateHeight)

      blockchainUpdatesStream.start(r.processedHeight + 1)
      log.warn(s"Closed by a remote part, restarting. Reason: $event")
      r
    }

    event match {
      case WrappedEvent.Next(event) => apply(processor, orig, event)
      case WrappedEvent.Closed      => Task(forceRestart())
      case WrappedEvent.Failed(e) =>
        Task {
          e match {
            case e: StatusRuntimeException if grpcStatusesToRestart.contains(e.getStatus.getCode) => forceRestart()
            case _: UpstreamTimeoutException                                                      => forceRestart()

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
    val update = event.getUpdate.update
    val h      = Height(event.getUpdate.height)

    val currBlockId = event.getUpdate.id.toByteStr
    log.info(s"$orig + ${getUpdateType(update)}(id=$currBlockId, h=$h)")

    val ignore = Task.now(orig)
    orig match {
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
            } else Task.now(comparedBlocks.copy(processedHeight = h))

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
            Task.now(ResolvingFork.from(Height(event.getUpdate.height), orig.processedHeight))

          case Update.Empty => Task.now(orig.withHeight(h))
        }

      case orig: ResolvingFork =>
        update match {
          case _: Update.Append =>
            processor.process(event.getUpdate)
            val updated = orig.apply(event)
            if (updated.isRollbackResolved) processor.runScripts().as(Working(updated.processedHeight))
            else Task.now(updated)

          case _: Update.Rollback =>
            processor.removeBlocksFrom(Height(h + 1))
            processor.process(event.getUpdate)
            Task.now(orig.apply(event))

          case Update.Empty => Task.now(orig.withProcessedHeight(h))
        }
    }
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
