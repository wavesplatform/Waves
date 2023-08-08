package com.wavesplatform.api.grpc

import scala.concurrent.Future
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.api.common.{CommonTransactionsApi, TransactionMeta}
import com.wavesplatform.api.grpc.TransactionsApiGrpcImpl.applicationStatusFromTxStatus
import com.wavesplatform.protobuf.*
import com.wavesplatform.protobuf.transaction.*
import com.wavesplatform.protobuf.utils.PBImplicitConversions.PBRecipientImplicitConversionOps
import com.wavesplatform.state.{Blockchain, TxMeta, InvokeScriptResult as VISR}
import com.wavesplatform.transaction.{Authorized, EthereumTransaction}
import com.wavesplatform.transaction.TxValidationError.GenericError
import io.grpc.{Status, StatusRuntimeException}
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import monix.reactive.Observable

class TransactionsApiGrpcImpl(blockchain: Blockchain, commonApi: CommonTransactionsApi)(implicit sc: Scheduler)
    extends TransactionsApiGrpc.TransactionsApi {

  override def getTransactions(request: TransactionsRequest, responseObserver: StreamObserver[TransactionResponse]): Unit =
    responseObserver.interceptErrors {
      val transactionIds = request.transactionIds.map(_.toByteStr)
      val stream: Observable[TransactionMeta] = request.recipient match {
        // By recipient
        case Some(subject) =>
          val recipientAddrOrAlias = subject
            .toAddressOrAlias(AddressScheme.current.chainId)
            .flatMap(blockchain.resolveAlias(_))
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
          .collect {
            case m if transactionIdSet.isEmpty || transactionIdSet(m.transaction.id()) =>
              TransactionsApiGrpcImpl.toTransactionResponse(m)
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
        Observable.fromIterable(unconfirmedTransactions.map(t => TransactionResponse(t.id().toByteString, transaction = Some(t.toPB))))
      )
    }

  override def getStateChanges(request: TransactionsRequest, responseObserver: StreamObserver[InvokeScriptResultResponse]): Unit =
    responseObserver.interceptErrors {
      val result = Observable(request.transactionIds*)
        .flatMap(txId => Observable.fromIterable(commonApi.transactionById(txId.toByteStr)))
        .collect { case TransactionMeta.Invoke(_, transaction, _, _, invokeScriptResult) =>
          InvokeScriptResultResponse.of(
            Some(PBTransactions.protobuf(transaction)),
            invokeScriptResult.map(VISR.toPB(_, addressForTransfer = true))
          )
        }

      responseObserver.completeWith(result)
    }

  override def getStatuses(request: TransactionsByIdRequest, responseObserver: StreamObserver[TransactionStatus]): Unit =
    responseObserver.interceptErrors {
      val result = Observable(request.transactionIds*).map { txId =>
        commonApi
          .unconfirmedTransactionById(txId.toByteStr)
          .map(_ => TransactionStatus(txId, TransactionStatus.Status.UNCONFIRMED))
          .orElse {
            commonApi.transactionById(txId.toByteStr).map { m =>
              val status = applicationStatusFromTxStatus(m.status)

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
      vtxEither <- Future(tx.toVanilla) // Intercept runtime errors
      vtx       <- vtxEither.toFuture
      _      <- Either.cond(!vtx.isInstanceOf[EthereumTransaction], (), GenericError("ETH transactions should not be broadcasted over gRPC")).toFuture
      result <- commonApi.broadcastTransaction(vtx)
      _      <- result.resultE.toFuture // Check for success
    } yield tx).wrapErrors
}

private object TransactionsApiGrpcImpl {
  def toTransactionResponse(meta: TransactionMeta): TransactionResponse = {
    val transactionId = meta.transaction.id().toByteString
    val status        = applicationStatusFromTxStatus(meta.status)
    val invokeScriptResult = meta match {
      case TransactionMeta.Invoke(_, _, _, _, r) => r.map(VISR.toPB(_, addressForTransfer = true))
      case _                                     => None
    }

    TransactionResponse(transactionId, meta.height, Some(meta.transaction.toPB), status, invokeScriptResult)
  }

  def applicationStatusFromTxStatus(status: TxMeta.Status): ApplicationStatus.Recognized =
    status match {
      case TxMeta.Status.Succeeded => ApplicationStatus.SUCCEEDED
      case TxMeta.Status.Failed    => ApplicationStatus.SCRIPT_EXECUTION_FAILED
      case TxMeta.Status.Elided    => ApplicationStatus.ELIDED
    }
}
