package com.wavesplatform.network

import com.wavesplatform.network.MircoBlockSynchronizer.QueuedRequests
import com.wavesplatform.state2.ByteStr
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import monix.execution.Cancelable
import scorex.transaction.NgHistory
import scorex.utils.ScorexLogging

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.{Failure, Success}

@Sharable
class MircoBlockSynchronizer(history: NgHistory) extends ChannelInboundHandlerAdapter with ScorexLogging {

  private val requests = new QueuedRequests()

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case MicroBlockResponse(mb) =>
      log.trace(id(ctx) + "Received MicroBlockResponse " + mb)
      requests.complete(mb.signature)

    case mi @ MicroBlockInv(totalResBlockSig, prevResBlockSig) =>
      log.trace(id(ctx) + "Received " + mi)
      history.lastBlockId() match {
        case Some(lastBlockId) =>
          if (lastBlockId == prevResBlockSig) {
            requests.ask(ctx, totalResBlockSig)
          } else {
            log.trace(s"Discarding $mi because it doesn't match last (micro)block")
          }

        case None =>
          log.warn("History does not contain the last block!")
      }

    case _ => super.channelRead(ctx, msg)
  }
}

object MircoBlockSynchronizer {

  private type MicroBlockSignature = ByteStr

  // microblock interval
  private[network] class QueuedRequests(waitResponseTimeout: FiniteDuration = 10.seconds) extends ScorexLogging {

    // Possible issue: it has unbounded queue size
    private val scheduler = monix.execution.Scheduler.singleThread("r")
    private var info: Map[MicroBlockSignature, MicroBlockInfo] = Map.empty

    def ask(ownerCtx: ChannelHandlerContext, sig: MicroBlockSignature): Unit = runInQueue {
      val orig = info.getOrElse(sig, RequestsInfo.empty)
      val updated = if (orig.isBusy) {
        orig.copy(freeOwnerCtxs = ownerCtx :: orig.freeOwnerCtxs)
      } else {
        orig.copy(madeRequests = newTask(ownerCtx, sig) :: orig.madeRequests)
      }

      info += sig -> updated
    }

    def complete(sig: MicroBlockSignature): Unit = runInQueue {
      info(sig).cancel()
      info -= sig
    }

    protected def newRequest(ownerCtx: ChannelHandlerContext, sig: MicroBlockSignature): ChannelFuture = {
      ownerCtx.writeAndFlush(MicroBlockRequest(sig))
    }

    private def newTask(ownerCtx: ChannelHandlerContext, sig: MicroBlockSignature) = Request(
      ctx = ownerCtx,
      worker = newRequest(ownerCtx, sig),
      timeoutTimer = scheduleReRequest(sig)
    )

    private def scheduleReRequest(sig: MicroBlockSignature): Cancelable = {
      def reRequest(sig: MicroBlockSignature): Unit = info.get(sig).foreach { orig =>
        val updated = orig.freeOwnerCtxs match {
          case owner :: restOwners =>
            orig.copy(
              freeOwnerCtxs = restOwners,
              madeRequests = newTask(owner.ctx, sig) :: orig.madeRequests
            )

          case _ => orig
        }

        info += sig -> updated
      }

      scheduler.scheduleOnce(waitResponseTimeout)(reRequest(sig))
    }

    private def runInQueue(f: => Unit): Unit = monix.eval.Task(f).runOnComplete {
      case Failure(e) => log.error("Failed to run in queue", e)
      case Success(_) => log.info("test")
    }(scheduler)
  }

  private case class MicroBlockInfo(freeOwnerCtxs: List[ChannelHandlerContext],
                                    madeRequests: List[Request]) {
    def isBusy: Boolean = madeRequests.exists(!_.worker.isDone)
    def cancel(): Unit = madeRequests.foreach(_.cancel())
  }

  private object RequestsInfo {
    val empty = MicroBlockInfo(
      freeOwnerCtxs = List.empty,
      madeRequests = List.empty
    )
  }

  private case class Request(ctx: ChannelHandlerContext,
                             worker: ChannelFuture,
                             timeoutTimer: Cancelable) {
    def cancel(): Unit = {
      timeoutTimer.cancel()
      worker.cancel(false)
    }
  }

}
