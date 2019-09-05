package com.wavesplatform.api.grpc
import com.google.protobuf.ByteString
import com.wavesplatform.account.PublicKey
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction._
import com.wavesplatform.state.{Blockchain, TransactionId}
import com.wavesplatform.transaction.AuthorizedTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.Future
import scala.util.Try

class TransactionsApiGrpcImpl(
    wallet: Wallet,
    blockchain: Blockchain,
    utx: UtxPool,
    publishTransaction: VanillaTransaction => TracedResult[ValidationError, Boolean]
)(
    implicit sc: Scheduler
) extends TransactionsApiGrpc.TransactionsApi {

  private[this] val commonApi = new CommonTransactionsApi(blockchain, utx, wallet, publishTransaction)

  override def getTransactions(request: TransactionsRequest, responseObserver: StreamObserver[TransactionResponse]): Unit = {
    val transactions =
      if (!request.sender.isEmpty) commonApi.transactionsByAddress(request.sender.toAddress)
      else if (request.recipient.isDefined) commonApi.transactionsByAddress(request.getRecipient.toAddress)
      else Observable.empty

    val filter = transactionFilter(request.sender, request.recipient, request.transactionIds.toSet, _)
    val stream = transactions.collect {
      case (height, transaction) if filter(transaction) =>
        TransactionResponse(transaction.id(), height, Some(transaction.toPB))
    }

    responseObserver.completeWith(stream)
  }

  override def getUnconfirmed(request: TransactionsRequest, responseObserver: StreamObserver[TransactionResponse]): Unit = {
    val stream = {
      val txIds = request.transactionIds.toSet

      Observable(commonApi.unconfirmedTransactions(): _*)
        .filter(transactionFilter(request.sender, request.recipient, txIds, _))
        .map(tx => TransactionResponse(tx.id(), transaction = Some(tx.toPB)))
    }

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
        sender <- wallet.findPrivateKey(tx.sender.stringRepr)
        signer <- if (tx.sender.stringRepr == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
        tx     <- Try(tx.signed(signer.privateKey)).toEither.left.map(GenericError(_))
      } yield tx

    val signerAddress: PublicKey = if (request.signerPublicKey.isEmpty) request.getTransaction.sender else request.signerPublicKey.toPublicKey
    signTransactionWith(request.getTransaction, wallet, signerAddress.stringRepr).explicitGetErr()
  }

  override def broadcast(tx: PBSignedTransaction): Future[PBSignedTransaction] = Future {
    commonApi
      .broadcastTransaction(tx.toVanilla)
      .resultE
      .map(_ => tx)
      .explicitGetErr()
  }

  private[this] def transactionFilter(
      sender: ByteString,
      recipient: Option[Recipient],
      transactionIds: Set[ByteString],
      tx: VanillaTransaction
  ): Boolean = {
    val senderMatches = sender.isEmpty || (tx match {
      case a: AuthorizedTransaction => sender.isEmpty || a.sender.toAddress == sender.toAddress
      case _                        => false
    })

    lazy val recipientAddr = recipient.get.toAddressOrAlias
    val recipientMatches = recipient.isEmpty || (tx match {
      case tt: TransferTransaction      => tt.recipient == recipientAddr
      case lt: LeaseTransaction         => lt.recipient == recipientAddr
      case mtt: MassTransferTransaction => mtt.transfers.map(_.address).contains(recipientAddr)
      case tx: AuthorizedTransaction    => recipientAddr == tx.sender.toAddress
      case _                            => false
    })

    val transactionIdMatches = transactionIds.isEmpty || transactionIds.contains(tx.id().toPBByteString)
    senderMatches && recipientMatches && transactionIdMatches
  }
}
