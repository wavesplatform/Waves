package com.wavesplatform.events

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.api.grpc.protobuf.{SubscribeEvent, SubscribeRequest}
import com.wavesplatform.events.protobuf.BlockchainUpdated as PBBlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Rollback.RollbackType
import com.wavesplatform.events.protobuf.serde.BlockchainUpdatedVanilla
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.state.Blockchain
import io.grpc.stub.{CallStreamObserver, StreamObserver}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.duration.*

trait FakeObserver[T] extends StreamObserver[T] {
  def values: Seq[T]
  def completed: Boolean
  def error: Option[Throwable]
}

object FakeObserver {
  def apply[T]: FakeObserver[T] = new CallStreamObserver[T] with FakeObserver[T] {
    @volatile var values    = Seq.empty[T]
    @volatile var error     = Option.empty[Throwable]
    @volatile var completed = false

    override def isReady: Boolean                                  = true
    override def setOnReadyHandler(onReadyHandler: Runnable): Unit = ()
    override def disableAutoInboundFlowControl(): Unit             = ()
    override def request(count: Int): Unit                         = ()
    override def setMessageCompression(enable: Boolean): Unit      = ()

    def onNext(value: T): Unit =
      values :+= value

    def onError(t: Throwable): Unit =
      error = Some(t)

    def onCompleted(): Unit =
      completed = true
  }

  implicit class FakeObserverOps[T](fo: FakeObserver[T]) {
    def fetchUntil(conditionF: Seq[T] => Boolean, timeout: Duration = 1.minute): Seq[T] = {
      def waitRecTask: Task[Unit] = {
        if (fo.error.isDefined) Task.raiseError(fo.error.get)
        else if (conditionF(fo.values)) Task.unit
        else Task.defer(waitRecTask).delayExecution(50 millis)
      }

      waitRecTask
        .map(_ => fo.values)
        .runSyncUnsafe(timeout)
    }
  }
  implicit class EventsFakeObserverOps(fo: FakeObserver[SubscribeEvent]) {
    def fetchAllEvents(bc: Blockchain, maxHeight: Int = Int.MaxValue): Seq[SubscribeEvent] =
      fo.fetchUntil(_.exists { u =>
        u.update.map(_.id.toByteStr) == bc.lastBlockId || u.getUpdate.height >= maxHeight
      })

    def lastAppendEvent(bc: Blockchain): BlockAppended =
      fetchAllEvents(bc).last.getUpdate.vanillaAppend
  }

  implicit class UpdatesRepoExt(ur: Repo) {
    def createFakeObserver(request: SubscribeRequest): FakeObserver[SubscribeEvent] = {
      val obs = FakeObserver[SubscribeEvent]
      ur.subscribe(request, obs)
      obs
    }
  }

  // Matchers
  object E {
    object Block {
      def unapply(bu: PBBlockchainUpdated): Option[(Int, ByteStr)] =
        for {
          a <- bu.update.append
          _ <- a.body.block
        } yield bu.height -> ByteStr(bu.id.toByteArray)
    }

    object Micro {
      def unapply(bu: PBBlockchainUpdated): Option[(Int, ByteStr)] =
        for {
          a <- bu.update.append
          _ <- a.body.microBlock
        } yield bu.height -> ByteStr(bu.id.toByteArray)
    }

    object Rollback {
      def unapply(bu: PBBlockchainUpdated): Option[(Int, ByteStr)] =
        for {
          r <- bu.update.rollback
          if r.`type` == RollbackType.BLOCK
        } yield bu.height -> ByteStr(bu.id.toByteArray)
    }

    object MicroRollback {
      def unapply(bu: PBBlockchainUpdated): Option[(Int, ByteStr)] =
        for {
          r <- bu.update.rollback
          if r.`type` == RollbackType.MICROBLOCK
        } yield bu.height -> ByteStr(bu.id.toByteArray)
    }
  }
}
