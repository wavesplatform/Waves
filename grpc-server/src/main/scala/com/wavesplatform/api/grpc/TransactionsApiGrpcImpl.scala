package com.wavesplatform.api.grpc

import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.protobuf.transaction._
import com.wavesplatform.transaction.Authorized
import io.grpc.stub.StreamObserver
import io.grpc.{Status, StatusRuntimeException}
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.Future

class TransactionsApiGrpcImpl(commonApi: CommonTransactionsApi)(implicit sc: Scheduler) extends TransactionsApiGrpc.TransactionsApi {

  override def getTransactions(request: TransactionsRequest, responseObserver: StreamObserver[TransactionResponse]): Unit =
    responseObserver.interceptErrors {
      val transactionIds = request.transactionIds.map(_.toByteStr)
      val stream = request.recipient match {
        case Some(subject) =>
          commonApi.transactionsByAddress(
            subject.toAddressOrAlias,
            Option(request.sender).collect { case s if !s.isEmpty => s.toAddress },
            Set.empty,
            None
          )
        case None =>
          if (request.sender.isEmpty) {
            Observable.fromIterable(transactionIds.flatMap(commonApi.transactionById)).map {
              case (h, e, s) => (h, e.fold(identity, _._1), s)
            }
          } else {
            val senderAddress = request.sender.toAddress
            commonApi.transactionsByAddress(
              senderAddress,
              Some(senderAddress),
              Set.empty,
              None
            )
          }
      }

      val transactionIdSet = transactionIds.toSet

      responseObserver.completeWith(
        stream
          .filter { case (_, t, _) => transactionIdSet.isEmpty || transactionIdSet(t.id()) }
          .map {
            case (h, tx, false) => TransactionResponse(tx.id().toPBByteString, h, Some(tx.toPB), ApplicationStatus.SCRIPT_EXECUTION_FAILED)
            case (h, tx, _)     => TransactionResponse(tx.id().toPBByteString, h, Some(tx.toPB), ApplicationStatus.SUCCEEDED)
          }
      )
    }

  override def getUnconfirmed(request: TransactionsRequest, responseObserver: StreamObserver[TransactionResponse]): Unit =
    responseObserver.interceptErrors {
      val unconfirmedTransactions = if (!request.sender.isEmpty) {
        val senderAddress = request.sender.toAddress
        commonApi.unconfirmedTransactions.collect {
          case a: Authorized if a.sender.toAddress == senderAddress => a
        }
      } else {
        request.transactionIds.flatMap(id => commonApi.unconfirmedTransactionById(id.toByteStr))
      }

      responseObserver.completeWith(
        Observable.fromIterable(unconfirmedTransactions.map(t => TransactionResponse(t.id().toPBByteString, transaction = Some(t.toPB))))
      )
    }

  override def getStateChanges(request: TransactionsRequest, responseObserver: StreamObserver[InvokeScriptResultResponse]): Unit =
    responseObserver.interceptErrors {
      import com.wavesplatform.state.{InvokeScriptResult => VISR}

      val result = Observable(request.transactionIds: _*)
        .flatMap(txId => Observable.fromIterable(commonApi.transactionById(txId.toByteStr)))
        .collect {
          case (_, Right((tx, Some(isr))), _) =>
            InvokeScriptResultResponse.of(Some(PBTransactions.protobuf(tx)), Some(VISR.toPB(isr)))
        }

      responseObserver.completeWith(result)
    }

  override def getStatuses(request: TransactionsByIdRequest, responseObserver: StreamObserver[TransactionStatus]): Unit =
    responseObserver.interceptErrors {
      val result = Observable(request.transactionIds: _*).map { txId =>
        commonApi
          .unconfirmedTransactionById(txId)
          .map(_ => TransactionStatus(txId, TransactionStatus.Status.UNCONFIRMED))
          .orElse {
            commonApi.transactionById(txId).map {
              case (h, _, false) => TransactionStatus(txId, TransactionStatus.Status.CONFIRMED, h, ApplicationStatus.SCRIPT_EXECUTION_FAILED)
              case (h, _, _)     => TransactionStatus(txId, TransactionStatus.Status.CONFIRMED, h, ApplicationStatus.SUCCEEDED)
            }
          }
          .getOrElse(TransactionStatus(txId, TransactionStatus.Status.NOT_EXISTS))
      }
      responseObserver.completeWith(result)
    }

  override def sign(request: SignRequest): Future[PBSignedTransaction] = Future {
    throw new StatusRuntimeException(Status.UNIMPLEMENTED)
  }

  override def broadcast(tx: PBSignedTransaction): Future[PBSignedTransaction] =
    (for {
      maybeTx <- Future(tx.toVanilla)
      vtx     <- maybeTx.toFuture
      result  <- commonApi.broadcastTransaction(vtx)
      _       <- result.resultE.toFuture
    } yield tx).wrapErrors
}
