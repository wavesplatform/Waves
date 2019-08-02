package com.wavesplatform.api.grpc
import com.wavesplatform.account.PublicKey
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.protobuf.transaction.{InvokeScriptResult, PBSignedTransaction, PBTransaction, VanillaTransaction}
import com.wavesplatform.state.{Blockchain, TransactionId}
import com.wavesplatform.transaction.AuthorizedTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.Future
import scala.util.Try

class TransactionsApiGrpcImpl(wallet: Wallet,
                              blockchain: Blockchain,
                              utx: UtxPool,
                              utxPoolSynchronizer: UtxPoolSynchronizer,
                              forceBroadcast: Boolean)(implicit sc: Scheduler)
    extends TransactionsApiGrpc.TransactionsApi {

  private[this] val commonApi = new CommonTransactionsApi(blockchain, utx, wallet, utxPoolSynchronizer)

  override def getTransactions(request: TransactionsRequest, responseObserver: StreamObserver[TransactionResponse]): Unit = {
    val stream = commonApi
      .transactionsByAddress(request.sender.toAddress)
      .map {
        case (height, transaction) if transactionFilter(request, transaction) => TransactionResponse(transaction.id(), height, Some(transaction.toPB))
      }

    responseObserver.completeWith(stream)
  }

  override def getUnconfirmed(request: TransactionsRequest, responseObserver: StreamObserver[TransactionResponse]): Unit = {
    val stream = Observable(commonApi.unconfirmedTransactions(): _*)
      .filter(transactionFilter(request, _))
      .map(tx => TransactionResponse(tx.id(), transaction = Some(tx.toPB)))

    responseObserver.completeWith(stream)
  }

  override def getStateChanges(request: TransactionsRequest, responseObserver: StreamObserver[InvokeScriptResult]): Unit = {
    import com.wavesplatform.state.{InvokeScriptResult => VISR}

    val result = Observable(request.transactionIds: _*)
      .flatMap(txId => Observable.fromIterable(blockchain.invokeScriptResult(TransactionId(txId.toByteStr)).toOption))
      .map(VISR.toPB)

    responseObserver.completeWith(result)
  }

  override def getStatuses(request: TransactionsByIdRequest, responseObserver: StreamObserver[TransactionStatus]): Unit = {
    val result = Observable(request.transactionIds: _*).map { txId =>
      blockchain.transactionHeight(txId) match {
        case Some(height) => TransactionStatus(txId, TransactionStatus.Status.CONFIRMED, height)

        case None =>
          utx.transactionById(txId) match {
            case Some(_) => TransactionStatus(txId, TransactionStatus.Status.UNCONFIRMED)
            case None    => TransactionStatus(txId, TransactionStatus.Status.NOT_EXISTS)
          }
      }
    }
    responseObserver.completeWith(result)
  }

  override def sign(request: SignRequest): Future[PBSignedTransaction] = Future {
    def signTransactionWith(tx: PBTransaction, wallet: Wallet, signerAddress: String): Either[ValidationError, PBSignedTransaction] =
      for {
        sender <- wallet.findPrivateKey(tx.sender.toString)
        signer <- if (tx.sender.toString == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
        tx     <- Try(tx.signed(signer.privateKey)).toEither.left.map(GenericError(_))
      } yield tx

    val signerAddress: PublicKey = if (request.signerPublicKey.isEmpty) request.getTransaction.sender else request.signerPublicKey.toPublicKey
    signTransactionWith(request.getTransaction, wallet, signerAddress.toString).explicitGetErr()
  }

  override def broadcast(tx: PBSignedTransaction): Future[PBSignedTransaction] = {
    commonApi
      .broadcastTransaction(tx.toVanilla, forceBroadcast)
      .map(_.resultE.map(_ => tx).explicitGetErr())
  }

  private[this] def transactionFilter(request: TransactionsRequest, tx: VanillaTransaction): Boolean = {
    val senderMatches = request.sender.isEmpty || (tx match {
      case a: AuthorizedTransaction => request.sender.isEmpty || a.sender.toAddress == request.sender.toAddress
      case _                        => false
    })

    val recipientMatches = tx match {
      case tt: TransferTransaction => request.recipient.isEmpty || tt.recipient == request.getRecipient.toAddressOrAlias
      case _                       => request.recipient.isEmpty
    }

    val transactionIdMatches = request.transactionIds.isEmpty || request.transactionIds.contains(tx.id().toPBByteString)
    senderMatches && recipientMatches && transactionIdMatches
  }
}
