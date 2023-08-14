package com.wavesplatform.ride.runner

import com.wavesplatform.api.BlockchainApi.BlockchainUpdatesStream
import com.wavesplatform.api.UpdateType
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.ride.runner.stats.RideRunnerStats
import com.wavesplatform.state.Height
import com.wavesplatform.utils.ScorexLogging
import io.grpc.{Status, StatusRuntimeException}
import monix.eval.Task
import monix.execution.exceptions.UpstreamTimeoutException

import scala.concurrent.duration.FiniteDuration

sealed trait BlockchainState extends Product with Serializable {
  def processedHeight: Height
}

object BlockchainState extends ScorexLogging {
  case class Settings(delayBeforeForceRestartBlockchainUpdates: FiniteDuration)

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
        processedHeight = Height(processedHeight - 1), // Drop one block, because it could be updated (liquid)
        processedMicroBlockNumber = 0,
        minResolveHeight = workingStateHeight
      )
  }

  private val grpcStatusesToRestart = Set(
    Status.Code.INTERNAL,   // RST_STREAM closed stream. HTTP/2 error code: INTERNAL_ERROR
    Status.Code.UNKNOWN,    // Probably it is an issue with a balancer
    Status.Code.UNAVAILABLE // Failed keepalives or something, let's try again
  )

  def applyWithRestarts(
      settings: Settings,
      processor: Processor,
      blockchainUpdatesStream: BlockchainUpdatesStream,
      orig: BlockchainState,
      event: WrappedEvent[SubscribeEvent]
  ): Task[BlockchainState] = {
    def forceRestart(): Task[BlockchainState] = {
      val (currHeight, workingStateHeight) = orig match {
        // TODO #62 replace origHeight in ResolvingFork by resolveHeight
        case orig: Starting      => (orig.processedHeight, orig.workingHeight)
        case Working(height)     => (height, height)
        case orig: ResolvingFork => (orig.processedHeight, orig.minResolveHeight)
      }

      // Almost impossible on MainNet and TestNet, so we can neglect this
      require(currHeight > 1, "Uncaught case. Check a connectivity to gRPC servers, remove all caches and restart the service")

      processor.forceRollbackLiquid()
      val r = ResolvingFork.forceRollBackOne(currHeight, workingStateHeight)

      log.warn(s"Closed by a remote part, restarting. Reason: $event")

      Task {
        val startHeight = Height(r.processedHeight + 1)
        blockchainUpdatesStream.start(startHeight)
        r
      }.delayExecution(settings.delayBeforeForceRestartBlockchainUpdates)
    }

    event match {
      case WrappedEvent.Next(event) => Task(apply(processor, orig, event))
      case WrappedEvent.Closed      => forceRestart()
      case WrappedEvent.Failed(e) =>
        e match {
          case _: UpstreamTimeoutException => forceRestart()
          case e: StatusRuntimeException
              if grpcStatusesToRestart.contains(e.getStatus.getCode) ||
                // During Node rollbacks
                e.getStatus.getCode == Status.Code.INVALID_ARGUMENT
                && e.getMessage.contains("Requested start height exceeds current blockchain height") =>
            forceRestart()

          case _ =>
            Task {
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

  def apply(processor: Processor, orig: BlockchainState, event: SubscribeEvent): BlockchainState = {
    val update = event.getUpdate.update
    val h      = Height(event.getUpdate.height)
    RideRunnerStats.lastKnownHeight.update(h)

    val currBlockId = event.getUpdate.id.toByteStr
    val updateType  = UpdateType.from(update)
    log.info(s"$orig + $updateType(id=$currBlockId, h=$h)")
    def logStatusChanged(updated: BlockchainState): Unit =
      log.info(s"Status changed: ${orig.getClass.getSimpleName} -> ${updated.getClass.getSimpleName}")

    orig match {
      case orig: Starting =>
        update match {
          case _: Update.Append =>
            val comparedBlocks =
              if (orig.foundDifferentBlocks) orig
              else
                processor.hasLocalBlockAt(h, currBlockId) match {
                  case Some(true) => orig
                  case _ =>
                    log.info(s"Blocks on $h are different")
                    processor.removeAllFrom(h)
                    orig.withDifferentBlocks
                }

            if (comparedBlocks.foundDifferentBlocks) processor.process(event.getUpdate)

            if (h >= comparedBlocks.workingHeight) {
              log.info(s"[$h] Reached current height")
              val r = Working(h)
              logStatusChanged(r)
              processor.startScripts()
              r
            } else comparedBlocks.copy(processedHeight = h)

          case _: Update.Rollback =>
            // It works even for micro blocks, because we have a restored version of data in event
            processor.process(event.getUpdate)
            orig

          case Update.Empty => orig
        }

      case orig: Working =>
        update match {
          case _: Update.Append =>
            processor.process(event.getUpdate)
            processor.scheduleAffectedScripts(updateType)
            orig.withHeight(h)

          case _: Update.Rollback =>
            processor.process(event.getUpdate)
            val r = ResolvingFork.from(Height(event.getUpdate.height), orig.processedHeight)
            logStatusChanged(r)
            r

          case Update.Empty => orig.withHeight(h)
        }

      case orig: ResolvingFork =>
        update match {
          case _: Update.Append =>
            processor.process(event.getUpdate)
            val updated = orig.apply(event)
            if (updated.isRollbackResolved) {
              val r = Working(updated.processedHeight)
              processor.scheduleAffectedScripts(updateType)
              logStatusChanged(r)
              r
            } else updated

          case _: Update.Rollback =>
            processor.process(event.getUpdate)
            orig.apply(event)

          case Update.Empty => orig.withProcessedHeight(h)
        }
    }
  }
}
