package com.wavesplatform.network

import java.util.concurrent.Executors
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import com.wavesplatform.metrics.Metrics
import com.wavesplatform.mining.Miner
import com.wavesplatform.network.CoordinatorHandler.EventType
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.{Coordinator, UtxPool}
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.group.ChannelGroup
import io.netty.channel.{Channel, ChannelHandlerContext, ChannelInboundHandlerAdapter}
import kamon.Kamon
import org.influxdb.dto.Point
import scorex.block.{Block, MicroBlock}
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

import scala.concurrent.{ExecutionContext, Future}

@Sharable
class CoordinatorHandler(
                            checkpointService: CheckpointService,
                            history: History,
                            blockchainUpdater: BlockchainUpdater,
                            time: Time,
                            stateReader: StateReader,
                            utxStorage: UtxPool,
                            blockchainReadiness:
                            AtomicBoolean,
                            miner: Miner,
                            settings: WavesSettings,
                            peerDatabase: PeerDatabase,
                            allChannels: ChannelGroup)
  extends ChannelInboundHandlerAdapter with ScorexLogging {

  private val counter = new AtomicInteger
  private implicit val ec = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor { r =>
    val t = new Thread(r)
    t.setName(s"coordinator-handler-${counter.incrementAndGet()}")
    t.setDaemon(true)
    t
  })

  private val processCheckpoint = Coordinator.processCheckpoint(checkpointService, history, blockchainUpdater) _
  private val processFork = Coordinator.processFork(checkpointService, history, blockchainUpdater, stateReader, utxStorage, time, settings, miner, blockchainReadiness) _
  private val processBlock = Coordinator.processSingleBlock(checkpointService, history, blockchainUpdater, time, stateReader, utxStorage, blockchainReadiness, settings, miner) _

  private def broadcastingScore(
                                   src: Channel,
                                   start: => String,
                                   success: => String,
                                   failure: => String,
                                   f: => Either[_, BigInt]): Unit = Future {
    log.debug(s"${id(src)} $start")
    f match {
      case Right(newLocalScore) =>
        log.info(s"${id(src)} $success, new local score is $newLocalScore")
        allChannels.broadcast(LocalScoreChanged(newLocalScore))
      case Left(e) =>
        log.warn(s"${id(src)} $failure: $e")
    }
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {

    case c: Checkpoint => broadcastingScore(ctx.channel,
      "Attempting to process checkpoint",
      "Successfully processed checkpoint",
      s"Error processing checkpoint",
      processCheckpoint(c))

    case ExtensionBlocks(blocks) => broadcastingScore(ctx.channel(),
      s"Attempting to append extension ${formatBlocks(blocks)}",
      s"Successfully appended extension ${formatBlocks(blocks)}",
      s"Error appending extension ${formatBlocks(blocks)}",
      processFork(blocks))

    case b: Block =>
      blockExternalStats(b, EventType.Receive)
      CoordinatorHandler.blockReceivingLag.record(System.currentTimeMillis() - b.timestamp)
      Signed.validateSignatures(b) match {
        case Left(err) => peerDatabase.blacklistAndClose(ctx.channel(), err.toString)
        case Right(_) => broadcastingScore(ctx.channel(),
          s"Attempting to append block ${b.uniqueId}",
          s"Successfully appended block ${b.uniqueId}",
          s"Could not append block ${b.uniqueId}",
          processBlock(b, false)
            .right.map { x =>
              blockExternalStats(b, EventType.Apply)
              x
            }
            .left.map { x =>
              blockExternalStats(b, EventType.Reject)
              x
            }
        )
      }
    case MicroBlockResponse(m) =>
      microExternalStats(m, EventType.Receive)
      val r: Either[Any, Any] = Signed.validateSignatures(m) match {
        case Right(_) =>
          Coordinator.processMicroBlock(checkpointService, history, blockchainUpdater, utxStorage)(m)
            .map { _ =>
              allChannels.broadcast(MicroBlockInv(m.totalResBlockSig, m.prevResBlockSig), Some(ctx.channel()))
            }
        case Left(err) =>
          peerDatabase.blacklistAndClose(ctx.channel(), err.toString)
          Left(())
      }
      r
        .right.map { x =>
          microExternalStats(m, EventType.Apply)
          x
        }
        .left.map { x =>
          microExternalStats(m, EventType.Reject)
          x
        }
  }

  private def blockExternalStats(b: Block, eventType: EventType): Unit = Metrics.write(
    Point
      .measurement("block-external")
      .addField("id", b.uniqueId.toString)
      .addField("type", eventType.value)
  )

  private def microExternalStats(m: MicroBlock, eventType: EventType): Unit = Metrics.write(
    Point
      .measurement("micro-external")
      .addField("id", m.uniqueId.toString)
      .addField("type", eventType.value)
  )
}

object CoordinatorHandler extends ScorexLogging {
  private val blockReceivingLag = Kamon.metrics.histogram("block-receiving-lag")

  def loggingResult[R](idCtx: String, msg: String, f: => Either[ValidationError, R]): Either[ValidationError, R] = {
    log.debug(s"$idCtx Starting $msg processing")
    val result = f
    result match {
      case Left(error) => log.warn(s"$idCtx Error processing $msg: $error")
      case Right(newScore) => log.debug(s"$idCtx Finished $msg processing, new local score is $newScore")
    }
    result
  }

  sealed abstract class EventType {
    val value: String = {
      val className = getClass.getName
      className.slice(className.lastIndexOf('$', className.length - 2) + 1, className.length - 1)
    }
  }
  object EventType {
    case object Receive extends EventType
    case object Apply extends EventType
    case object Reject extends EventType
  }
}
