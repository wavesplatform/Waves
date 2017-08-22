package com.wavesplatform.network

import java.util.concurrent.{ConcurrentHashMap, _}

import com.google.common.cache.{Cache, CacheBuilder}
import com.wavesplatform.network.MicroBlockSynchronizer.Settings
import com.wavesplatform.network.MicroBlockSynchronizerNew.{CachedJobs, Item, MyRequests}
import com.wavesplatform.state2.ByteStr
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import monix.execution.Cancelable
import monix.execution.schedulers.SchedulerService
import scorex.transaction.NgHistory
import scorex.utils.ScorexLogging

import scala.collection.immutable.{SortedSet, TreeMap}
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

@Sharable
class MicroBlockSynchronizer(settings: Settings, history: NgHistory)
  extends ChannelInboundHandlerAdapter with ScorexLogging {

  private val requests = new CachedJobs(new MyRequests(settings.waitResponseTimeout))

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case MicroBlockResponse(mb) =>
      log.trace(id(ctx) + "Received MicroBlockResponse " + mb)
      requests.complete(Item(ctx, mb.signature))

    case mi @ MicroBlockInv(totalResBlockSig, prevResBlockSig) =>
      log.trace(id(ctx) + "Received " + mi)
      history.lastBlockId() match {
        case Some(lastBlockId) =>
          if (lastBlockId == prevResBlockSig) {
            requests.ask(Item(ctx, totalResBlockSig))
          } else {
            log.trace(s"Discarding $mi because it doesn't match last (micro)block")
          }

        case None =>
          log.warn("History does not contain the last block!")
      }

    case _ => super.channelRead(ctx, msg)
  }
}

private object MicroBlockSynchronizerNew {
  case class Item(ctx: ChannelHandlerContext,
                  microBlockSig: ByteStr)

  case class TaskRequest(item: Item) extends Runnable {
    override def run(): Unit = item.ctx.writeAndFlush(MicroBlockRequest(item.microBlockSig))
  }

  class MyRequests(waitResponseTimeout: FiniteDuration) extends GroupedJobs[Item, ByteStr, TaskRequest] {
    private val scheduler = monix.execution.Scheduler.singleThread("queued-requests")

    override def groupId(item: Item): ByteStr = item.microBlockSig
    override def newJob(item: Item): TaskRequest = TaskRequest(item)
    override def newJobQueue: JobQueue[TaskRequest] = {
      val orig = new DelayedJobQueue[TaskRequest](waitResponseTimeout)(scheduler)
      new CachedQueuedJobs(orig, { job =>
        s"${job.item.microBlockSig}.${job.item.ctx.channel().id().asLongText()}"
      })
    }
  }

  class CachedJobs[ItemT, JobGroupIdT <: Object, JobT <: Runnable](val orig: GroupedJobs[ItemT, JobGroupIdT, JobT])
    extends GroupedJobs[ItemT, JobGroupIdT, JobT] {

    private val dummy = new Object
    private val processed: Cache[JobGroupIdT, Object] = CacheBuilder.newBuilder()
      .expireAfterWrite(1, TimeUnit.MINUTES)
      .build[JobGroupIdT, Object]()

    override def ask(item: ItemT): Unit = {
      if (isFresh(item)) {
        orig.ask(item)
      }
    }

    override def complete(item: ItemT): Unit = {
      val id = groupId(item)
      processed.put(id, dummy)
      orig.complete(item)
    }

    override def groupId(item: ItemT): JobGroupIdT = orig.groupId(item)
    override def newJob(item: ItemT): JobT = orig.newJob(item)
    override def newJobQueue: JobQueue[JobT] = orig.newJobQueue

    def isFresh(item: ItemT): Boolean = {
      val id = groupId(item)
      Option(processed.getIfPresent(id)).isEmpty
    }
  }

  trait GroupedJobs[ItemT, JobGroupIdT, JobT] {
    private val jobs = new ConcurrentHashMap[JobGroupIdT, JobQueue[JobT]]()

    def ask(item: ItemT): Unit = {
      val id = groupId(item)
      jobs
        .computeIfAbsent(id, _ => newJobQueue)
        .enqueue(newJob(item))
    }

    def complete(item: ItemT): Unit = {
      val id = groupId(item)
      Option(jobs.remove(id)).foreach { queue =>
        queue.shutdown()
      }
    }

    def groupId(item: ItemT): JobGroupIdT
    def newJob(item: ItemT): JobT
    def newJobQueue: JobQueue[JobT]
  }

  trait JobQueue[JobT] {
    def enqueue(job: JobT): Unit
    def shutdown(): Unit
  }

  class CachedQueuedJobs[JobT](orig: JobQueue[JobT], id: JobT => String) extends JobQueue[JobT] {
    private val cache = new ConcurrentHashMap[String, Boolean]()

    override def enqueue(job: JobT): Unit = {
      cache.computeIfAbsent(id(job), { _ =>
        orig.enqueue(job)
        true
      })
    }

    override def shutdown(): Unit = orig.shutdown()
  }

  class DelayedJobQueue[JobT <: Runnable](delay: FiniteDuration)(implicit val scheduler: SchedulerService)
    extends JobQueue[JobT] with ScorexLogging {

    private var isReady = true
    private var pending = List.empty[JobT]
    private var isScheduled = false
    private var timer = Cancelable.empty

    override def enqueue(job: JobT): Unit = runInQueue {
      if (isReady) {
        if (isScheduled) pending ::= job
        else {
          isScheduled = true
          job.run()
          schedule()
        }
      }
    }

    override def shutdown(): Unit = runInQueue {
      isReady = false
      pending = List.empty
      timer.cancel()
    }

    private def schedule(): Unit = {
      timer = scheduler.scheduleOnce(delay) {
        pending match {
          case job :: rest =>
            pending = rest
            job.run()
            schedule()

          case _ =>
            isScheduled = false
        }
      }
    }

    private def runInQueue(f: => Unit): Unit = monix.eval.Task(f).runOnComplete {
      case Failure(e) => log.error("Failed to run in queue", e)
      case Success(_) => log.info("test")
    }(scheduler)
  }
}

object MicroBlockSynchronizer {

  type MicroBlockSignature = ByteStr

  case class Settings(waitResponseTimeout: FiniteDuration)

  private[network] class QueuedRequests(waitResponseTimeout: FiniteDuration) extends ScorexLogging {

    // Possible issue: it has unbounded queue size
    private val scheduler = monix.execution.Scheduler.singleThread("queued-requests")
    private var info: Map[MicroBlockSignature, MicroBlockInfo] = Map.empty

    private val processed = new ProcessedMicroblocks

    def ask(ownerCtx: ChannelHandlerContext, sig: MicroBlockSignature): Unit = runInQueue {
      if (!processed.has(sig)) {
        val orig = info.getOrElse(sig, MicroBlockInfo.empty)
        if (!orig.contains(ownerCtx)) {
          val updated = if (orig.canDoNextRequest) {
            orig.withNewMadeRequest(ownerCtx, newTask(ownerCtx, sig))
          } else {
            orig.withNewFreeCtx(ownerCtx)
          }

          info += sig -> updated
        }
      }
    }

    def complete(sig: MicroBlockSignature): Unit = runInQueue {
      processed.add(sig)
      info(sig).cancel()
      info -= sig
    }

    protected def newRequest(ownerCtx: ChannelHandlerContext, sig: MicroBlockSignature): ChannelFuture = {
      ownerCtx.writeAndFlush(MicroBlockRequest(sig))
    }

    private def newTask(ownerCtx: ChannelHandlerContext, sig: MicroBlockSignature) = Request(
      sender = newRequest(ownerCtx, sig),
      timeoutTimer = scheduleReRequest(ownerCtx, sig)
    )

    private def scheduleReRequest(ownerCtx: ChannelHandlerContext, sig: MicroBlockSignature): Cancelable = {
      def reRequest(sig: MicroBlockSignature): Unit = info.get(sig).foreach { orig =>
        val newMadeRequests = orig.madeRequests.updated(
          ownerCtx,
          orig.madeRequests(ownerCtx).copy(timedOut = true)
        )

        val updated = orig.freeOwnerCtxs.headOption match {
          case Some(owner) =>
            orig.copy(
              freeOwnerCtxs = orig.freeOwnerCtxs - owner,
              madeRequests = newMadeRequests.updated(owner.ctx, newTask(owner.ctx, sig))
            )

          case _ => orig.copy(
            madeRequests = newMadeRequests
          )
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

  private case class MicroBlockInfo(freeOwnerCtxs: SortedSet[ChannelHandlerContext],
                                    madeRequests: Map[ChannelHandlerContext, Request]) {
    def canDoNextRequest: Boolean = madeRequests.values.forall(_.timedOut)
    def cancel(): Unit = madeRequests.values.foreach(_.cancel())

    // make tests for it
    def contains(x: ChannelHandlerContext): Boolean = freeOwnerCtxs.contains(x) || madeRequests.contains(x)

    def withNewMadeRequest(ownerCtx: ChannelHandlerContext, request: Request): MicroBlockInfo = {
      copy(madeRequests = madeRequests.updated(ownerCtx, request))
    }

    def withNewFreeCtx(ownerCtx: ChannelHandlerContext): MicroBlockInfo = {
      copy(freeOwnerCtxs = freeOwnerCtxs + ownerCtx)
    }
  }

  private object MicroBlockInfo {
    val empty = MicroBlockInfo(
      freeOwnerCtxs = SortedSet.empty(Ordering.by[ChannelHandlerContext, String](_.name())),
      madeRequests = TreeMap.empty(Ordering.by[ChannelHandlerContext, String](_.name()))
    )
  }

  private case class Request(sender: ChannelFuture,
                             timeoutTimer: Cancelable,
                             timedOut: Boolean = false) {
    def cancel(): Unit = {
      timeoutTimer.cancel()
      sender.cancel(false)
    }
  }

  private class ProcessedMicroblocks {
    private val dummy = new Object
    private val processed: Cache[MicroBlockSignature, Object] = CacheBuilder.newBuilder()
      .expireAfterWrite(1, TimeUnit.MINUTES)
      .build[MicroBlockSignature, Object]()

    def add(sig: MicroBlockSignature): Unit = processed.put(sig, dummy)
    def has(sig: MicroBlockSignature): Boolean = Option(processed.getIfPresent(sig)).isDefined
  }

}
