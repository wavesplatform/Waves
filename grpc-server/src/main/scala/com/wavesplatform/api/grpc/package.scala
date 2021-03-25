package com.wavesplatform.api

import com.wavesplatform.account.{Address, AddressOrAlias}
import com.wavesplatform.api.http.ApiError
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.block.{PBBlock, PBBlocks}
import com.wavesplatform.protobuf.transaction.{PBSignedTransaction, PBTransactions, VanillaTransaction}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.{block => vb}
import io.grpc.stub.{CallStreamObserver, ServerCallStreamObserver, StreamObserver}
import monix.execution.{Ack, AsyncQueue, Scheduler}
import monix.reactive.Observable

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

package object grpc extends ScorexLogging {
  implicit class VanillaTransactionConversions(val tx: VanillaTransaction) extends AnyVal {
    def toPB: PBSignedTransaction = PBTransactions.protobuf(tx)
  }

  implicit class PBSignedTransactionConversions(val tx: PBSignedTransaction) extends AnyVal {
    def toVanilla: Either[ValidationError, VanillaTransaction] = PBTransactions.vanilla(tx)
  }

  implicit class VanillaHeaderConversionOps(val header: vb.BlockHeader) extends AnyVal {
    def toPBHeader: PBBlock.Header = PBBlocks.protobuf(header)
  }

  implicit class StreamObserverMonixOps[T](val streamObserver: StreamObserver[T]) extends AnyVal {
    private[grpc] def id: String =
      Integer.toHexString(System.identityHashCode(streamObserver))

    def completeWith(obs: Observable[T])(implicit sc: Scheduler): Unit =
      wrapObservable(obs, streamObserver)(identity)

    def failWith(error: Throwable): Unit = {
      log.error(s"[${streamObserver.id}] gRPC call completed with error", error)
      streamObserver.onError(GRPCErrors.toStatusException(error))
    }

    def interceptErrors(f: => Unit): Unit =
      try f
      catch { case NonFatal(e) => streamObserver.failWith(e) }
  }

  implicit class EitherVEExt[T](val e: Either[ValidationError, T]) extends AnyVal {
    def explicitGetErr(): T = e.fold(e => throw GRPCErrors.toStatusException(e), identity)
    def toFuture: Future[T] = Future.fromTry(e.left.map(err => GRPCErrors.toStatusException(err)).toTry)
  }

  implicit class OptionErrExt[T](val e: Option[T]) extends AnyVal {
    def explicitGetErr(err: ApiError): T = e.getOrElse(throw GRPCErrors.toStatusException(err))
  }

  implicit class AddressOrAliasExt(val a: AddressOrAlias) extends AnyVal {
    def resolved(blockchain: Blockchain): Option[Address] = blockchain.resolveAlias(a).toOption
  }

  implicit class FutureExt[T](val f: Future[T]) extends AnyVal {
    def wrapErrors(implicit ec: ExecutionContext): Future[T] = f.recoverWith {
      case err => Future.failed(GRPCErrors.toStatusException(err))
    }
  }

  private[this] def wrapObservable[A, B](source: Observable[A], dest: StreamObserver[B])(f: A => B)(implicit s: Scheduler): Unit = dest match {
    case cso: CallStreamObserver[B] @unchecked =>
      sealed trait QueueV {}
      case class Element(e: B)       extends QueueV
      case class Fail(ex: Throwable) extends QueueV
      case object Complete           extends QueueV

      val queue = AsyncQueue.bounded[QueueV](32)

      cso.setOnReadyHandler(() => drainQueue())

      def drainQueue(): Unit = {
        @tailrec
        def pushNext(): Unit =
          if (cso.isReady) queue.tryPoll() match {
            case None => // queue is empty
            case Some(Element(elem)) =>
              cso.onNext(elem)
              pushNext()

            case Some(Complete) =>
              cso.onCompleted()

            case Some(Fail(ex)) =>
              cso.failWith(ex)
          }

        cso.synchronized(pushNext())
      }

      val cancelable = source.subscribe(
        (elem: A) =>
          queue
            .offer(Element(f(elem)))
            .flatMap { _ =>
              drainQueue()
              Ack.Continue
            },
        err =>
          queue
            .offer(Fail(err))
            .map(_ => drainQueue()),
        () =>
          queue
            .offer(Complete)
            .map(_ => drainQueue())
      )

      cso match {
        case scso: ServerCallStreamObserver[_] =>
          scso.setOnCancelHandler(cancelable.cancel _)

        case _ =>
      }

    case _ =>
      source.subscribe(
        { (elem: A) =>
          dest.onNext(f(elem))
          Ack.Continue
        },
        dest.failWith,
        () => dest.onCompleted()
      )
  }
}
