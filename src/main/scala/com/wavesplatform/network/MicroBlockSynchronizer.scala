package com.wavesplatform.network

import com.wavesplatform.network.MicroBlockSynchronizer._
import com.wavesplatform.state2.ByteStr
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import monix.eval.Task
import scorex.transaction.NgHistory
import scorex.utils.ScorexLogging

import scala.collection.mutable.{Map => MMap, Set => MSet}
import scala.concurrent.duration.FiniteDuration

@Sharable
class MicroBlockSynchronizer(settings: Settings, history: NgHistory)
  extends ChannelInboundHandlerAdapter with ScorexLogging {

  private implicit val scheduler = monix.execution.Scheduler.singleThread("microblock-synchronizer")

  def requestMicroblock(microBlockSig: MicroBlockSignature, attemptsAllowed: Int): Unit = if (attemptsAllowed > 0) {
    val knownChannels = knownMicroblockOwners.getOrElse(microBlockSig, MSet.empty)
    random(knownChannels) match {
      case None => ()
      case Some(ctx) =>
        knownChannels -= ctx
        ctx.writeAndFlush(MicroBlockRequest(microBlockSig))
        awaitingMicroblocks += microBlockSig
        Task { // still not received the microBlock
          if (awaitingMicroblocks.contains(microBlockSig))
            requestMicroblock(microBlockSig, attemptsAllowed - 1)
        }.delayExecution(settings.waitResponseTimeout)
          .runAsync
    }
  }

  val knownMicroblockOwners: MMap[MicroBlockSignature, MSet[ChannelHandlerContext]] = MMap.empty
  val awaitingMicroblocks: MSet[MicroBlockSignature] = MSet.empty

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case MicroBlockResponse(mb) => Task {
      log.trace(id(ctx) + "Received MicroBlockResponse " + mb)
      knownMicroblockOwners -= mb.totalResBlockSig
      awaitingMicroblocks -= mb.totalResBlockSig
    }.runAsync
    case mi@MicroBlockInv(totalResBlockSig, prevResBlockSig) => Task {
      log.trace(id(ctx) + "Received " + mi)
      history.lastBlockId() match {
        case Some(lastBlockId) =>
          if (lastBlockId == prevResBlockSig) {
            knownMicroblockOwners.get(totalResBlockSig) match {
              case None => knownMicroblockOwners += (totalResBlockSig -> MSet(ctx))
              case Some(set) => set += ctx
            }
            if (!awaitingMicroblocks.contains(totalResBlockSig)) {
              requestMicroblock(totalResBlockSig, 2)
            }
          } else {
            log.trace(s"Discarding $mi because it doesn't match last (micro)block")
          }
        case None =>
          log.warn("History does not contain the last block!")
      }
    }.runAsync
    case _ => super.channelRead(ctx, msg)
  }
}

object MicroBlockSynchronizer {

  type MicroBlockSignature = ByteStr

  case class Settings(waitResponseTimeout: FiniteDuration,
                      processedMicroBlocksCacheTimeout: FiniteDuration,
                      invCacheTimeout: FiniteDuration)

  def random[T](s: MSet[T]): Option[T] = {
    val n = util.Random.nextInt(s.size)
    val ts = s.iterator.drop(n)
    if (ts.hasNext) Some(ts.next)
    else None
  }
}
