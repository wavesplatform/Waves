package com.wavesplatform.api

import com.wavesplatform.account.{Address, AddressOrAlias}
import com.wavesplatform.api.http.ApiError
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.block.{PBBlock, PBBlocks}
import com.wavesplatform.protobuf.transaction.{PBSignedTransaction, PBTransactions, VanillaTransaction}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.{block => vb}
import io.grpc.stub.{ServerCallStreamObserver, StreamObserver}
import monix.execution.atomic.AtomicAny
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observable

import scala.concurrent.{ExecutionContext, Future, Promise}
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
    def id: String =
      Integer.toHexString(System.identityHashCode(streamObserver))

    def completeWith(obs: Observable[T])(implicit sc: Scheduler): Unit =
      wrapObservable(obs, streamObserver)

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

  private[this] def wrapObservable[A](source: Observable[A], dest: StreamObserver[A])(implicit s: Scheduler): Unit = dest match {
    case cso: ServerCallStreamObserver[A] @unchecked =>
      val nextItem = AtomicAny(Option.empty[(Promise[Ack], A)])

      def sendNextItem(): Unit =
        for ((p, elem) <- nextItem.getAndSet(None)) try {
          cso.onNext(elem)
          p.trySuccess(Ack.Continue)
        } catch {
          case NonFatal(t) =>
            cso.onError(t)
            p.tryFailure(t)
        }

      cso.setOnReadyHandler(() => sendNextItem())

      val cancelable = source.subscribe(
        (elem: A) =>
          if (cso.isCancelled) {
            Ack.Stop
          } else {
            val p = Promise[Ack]()
            if (nextItem.compareAndSet(None, Some(p -> elem))) {
              if (cso.isReady)
                sendNextItem()

              p.future
            } else Future.failed(new IllegalStateException(s"An element ${nextItem()} is pending"))
          },
        err => cso.onError(err), { () =>
          log.debug("Source observer completed")
          cso.onCompleted()
        }
      )
      cso.setOnCancelHandler { () =>
        log.warn("Stream cancelled")
        cancelable.cancel()
      }

    case _ =>
      log.warn(s"Unsupported StreamObserver type: $dest")
      source.subscribe(
        { (elem: A) =>
          dest.onNext(elem)
          Ack.Continue
        },
        dest.failWith,
        () => dest.onCompleted()
      )
  }
}
