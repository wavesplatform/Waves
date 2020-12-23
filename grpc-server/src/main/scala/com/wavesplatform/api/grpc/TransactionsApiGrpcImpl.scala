package com.wavesplatform.api.grpc

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.common.CommonTransactionsApi.TransactionMeta
import com.wavesplatform.protobuf._
import com.wavesplatform.protobuf.transaction._
import com.wavesplatform.protobuf.utils.PBImplicitConversions.PBRecipientImplicitConversionOps
import com.wavesplatform.state.{InvokeScriptResult => VISR}
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
      val stream: Observable[TransactionMeta] = request.recipient match {
        // By recipient
        case Some(subject) =>
          val recipientAddrOrAlias = subject
            .toAddressOrAlias(AddressScheme.current.chainId)
            .fold(e => throw new IllegalArgumentException(e.toString), identity)

          val maybeSender = Option(request.sender)
            .collect { case s if !s.isEmpty => s.toAddress }

          commonApi.transactionsByAddress(
            recipientAddrOrAlias,
            maybeSender,
            Set.empty,
            None
          )

        // By sender
        case None if !request.sender.isEmpty =>
          val senderAddress = request.sender.toAddress
          commonApi.transactionsByAddress(
            senderAddress,
            Some(senderAddress),
            Set.empty,
            None
          )

        // By ids
        case None =>
          Observable.fromIterable(transactionIds.flatMap(commonApi.transactionById))
      }

      val transactionIdSet = transactionIds.toSet
      responseObserver.completeWith(
        stream
          .filter { case TransactionMeta(_, tx, _) => transactionIdSet.isEmpty || transactionIdSet(tx.id()) }
          .map(TransactionsApiGrpcImpl.toTransactionResponse)
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
        Observable.fromIterable(unconfirmedTransactions.map(t => TransactionResponse(t.id().toByteString, transaction = Some(t.toPB))))
      )
    }

  override def getStateChanges(request: TransactionsRequest, responseObserver: StreamObserver[InvokeScriptResultResponse]): Unit =
    responseObserver.interceptErrors {
      val result = Observable(request.transactionIds: _*)
        .flatMap(txId => Observable.fromIterable(commonApi.transactionById(txId.toByteStr)))
        .collect {
          case CommonTransactionsApi.TransactionMeta.Invoke(_, transaction, _, invokeScriptResult) =>
            InvokeScriptResultResponse.of(Some(PBTransactions.protobuf(transaction)), invokeScriptResult.map(VISR.toPB))
        }

      responseObserver.completeWith(result)
    }

  override def getStatuses(request: TransactionsByIdRequest, responseObserver: StreamObserver[TransactionStatus]): Unit =
    responseObserver.interceptErrors {
      val result = Observable(request.transactionIds: _*).map { txId =>
        commonApi
          .unconfirmedTransactionById(txId.toByteStr)
          .map(_ => TransactionStatus(txId, TransactionStatus.Status.UNCONFIRMED))
          .orElse {
            commonApi.transactionById(txId.toByteStr).map { m =>
              val status = if (m.succeeded) ApplicationStatus.SUCCEEDED else ApplicationStatus.SCRIPT_EXECUTION_FAILED
              TransactionStatus(txId, TransactionStatus.Status.CONFIRMED, m.height, status)
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

private object TransactionsApiGrpcImpl {
  def toTransactionResponse(meta: TransactionMeta): TransactionResponse = {
    val TransactionMeta(height, tx, succeeded) = meta
    val transactionId                          = tx.id().toByteString
    val status                                 = if (succeeded) ApplicationStatus.SUCCEEDED else ApplicationStatus.SCRIPT_EXECUTION_FAILED
    val invokeScriptResult = meta match {
      case TransactionMeta.Invoke(_, _, _, r) => r.map(VISR.toPB)
      case _                                  => None
    }

    TransactionResponse(transactionId, height, Some(tx.toPB), status, invokeScriptResult)
  }
}
