package com.wavesplatform.network

import com.wavesplatform.network.MircoBlockSynchronizer.QueuedRequests
import com.wavesplatform.state2.ByteStr
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import monix.execution.Cancelable
import scorex.transaction.NgHistory
import scorex.utils.{ScorexLogging, SynchronizedOne}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

@Sharable
class MircoBlockSynchronizer(history: NgHistory)
  extends ChannelInboundHandlerAdapter with ScorexLogging with SynchronizedOne {

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
  class QueuedRequests(waitResponseTimeout: FiniteDuration = 10.seconds) {

    // Possible issue: it has unbounded queue size
    private val scheduler = monix.execution.Scheduler.singleThread("r")

    private var info: Map[MicroBlockSignature, RequestsInfo] = Map.empty

    def ask(ctx: ChannelHandlerContext, sig: MicroBlockSignature): Unit = runInQueue {
      val orig = info.getOrElse(sig, RequestsInfo.empty)
      val updated = if (orig.isBusy) {
        orig.copy(owners = ctx :: orig.owners)
      } else {
        orig.copy(tasks = newTask(ctx, sig) :: orig.tasks)
      }

      info += sig -> updated
    }

    def complete(sig: MicroBlockSignature): Unit = runInQueue {
      info(sig).cancel()
      info -= sig
    }

    protected def request(ctx: ChannelHandlerContext, sig: MicroBlockSignature): ChannelFuture = {
      ctx.writeAndFlush(MicroBlockRequest(sig))
    }

    private def newTask(ctx: ChannelHandlerContext, sig: MicroBlockSignature) = Task(
      ctx = ctx,
      request = request(ctx, sig),
      timeoutTimer = scheduleReRequest(sig)
    )

    private def scheduleReRequest(sig: MicroBlockSignature): Cancelable = {
      def reRequest(sig: MicroBlockSignature): Unit = info.get(sig).foreach { orig =>
        val updated = orig.owners match {
          case owner :: restOwners =>
            orig.copy(
              owners = restOwners,
              tasks = newTask(owner.ctx, sig) :: orig.tasks
            )

          case _ => orig
        }

        info += sig -> updated
      }

      scheduler.scheduleOnce(waitResponseTimeout)(reRequest(sig))
    }

    private def runInQueue(f: => Unit): Unit = monix.eval.Task(f).runAsync(scheduler)
  }

  private case class RequestsInfo(isDone: Boolean,
                                  owners: List[ChannelHandlerContext],
                                  tasks: List[Task]) {
    def isBusy: Boolean = tasks.exists(!_.request.isDone)
    def cancel(): Unit = tasks.foreach(_.cancel())
  }

  private object RequestsInfo {
    val empty = RequestsInfo(
      isDone = false,
      owners = List.empty,
      tasks = List.empty
    )
  }

  private case class Task(ctx: ChannelHandlerContext,
                          request: ChannelFuture,
                          timeoutTimer: Cancelable) {
    def cancel(): Unit = {
      timeoutTimer.cancel()
      if (!request.isDone) {
        request.cancel(true)
      }
    }
  }

}
