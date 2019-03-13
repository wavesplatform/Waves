package com.wavesplatform.api.grpc
import com.google.protobuf.empty.Empty
import com.wavesplatform.account.{Address, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.transaction.{PBSignedTransaction, PBTransaction}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.CommonValidation
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import io.grpc.stub.StreamObserver
import io.netty.channel.group.ChannelGroup
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable

import scala.concurrent.Future
import scala.util.Try

class TransactionsApiGrpcImpl(functionalitySettings: FunctionalitySettings,
                              wallet: Wallet,
                              blockchain: Blockchain,
                              utx: UtxPool,
                              allChannels: ChannelGroup)
    extends TransactionsApiGrpc.TransactionsApi {

  private[this] val TransactionsBatchLimit = 100

  override def getTransactionsByAddress(request: TransactionsByAddressRequest, responseObserver: StreamObserver[PBSignedTransaction]): Unit = {
    def getTransactionsFromId(address: Address, fromId: Option[ByteStr]): Observable[PBSignedTransaction] = {
      def getResponse(address: Address, limit: Int, fromId: Option[ByteStr]): Either[String, Seq[PBSignedTransaction]] = {
        blockchain
          .addressTransactions(address, Set.empty, limit, fromId)
          .map(_.map(_._2.toPB))
      }

      val observableTask = Task(getResponse(address, TransactionsBatchLimit, fromId)) map {
        case Right(transactions) =>
          if (transactions.isEmpty) Observable.empty
          else Observable(transactions: _*) ++ Observable.defer(getTransactionsFromId(address, Some(transactions.last.toVanilla.id())))
        case Left(err) => Observable.raiseError(new IllegalArgumentException(err))
      }

      Observable.fromTask(observableTask).flatten
    }

    val stream = getTransactionsFromId(request.getAddress.toAddress, Option(request.fromId.toByteStr).filterNot(_.isEmpty))
    responseObserver.completeWith(stream)
  }

  override def getTransactionById(request: TransactionByIdRequest): Future[PBSignedTransaction] = {
    blockchain
      .transactionInfo(request.transactionId)
      .map(_._2.toPB)
      .toFuture
  }

  override def getUnconfirmedTransactions(request: Empty, responseObserver: StreamObserver[PBSignedTransaction]): Unit = {
    val stream = Observable(utx.all: _*).map(_.toPB)
    responseObserver.completeWith(stream)
  }

  override def getUnconfirmedTransactionById(request: TransactionByIdRequest): Future[PBSignedTransaction] = {
    utx
      .transactionById(request.transactionId)
      .map(_.toPB)
      .toFuture
  }

  override def calculateFee(request: PBTransaction): Future[CalculateFeeResponse] = {
    CommonValidation
      .getMinFee(blockchain, functionalitySettings, blockchain.height, request.toVanilla)
      .map { case (assetId, assetAmount, _) => CalculateFeeResponse(assetId.protoId, assetAmount) }
      .toFuture
  }

  override def signTransaction(request: TransactionSignRequest): Future[PBSignedTransaction] = {
    def signTransactionWith(tx: PBTransaction, wallet: Wallet, signerAddress: String): Either[ValidationError, PBSignedTransaction] =
      for {
        sender <- wallet.findPrivateKey(tx.sender.toString)
        signer <- if (tx.sender.toString == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
        tx     <- Try(tx.signed(signer.privateKey)).toEither.left.map(GenericError(_))
      } yield tx

    val signerAddress: PublicKeyAccount = if (request.signer.isEmpty) request.getTransaction.sender else request.signer.toPublicKeyAccount
    signTransactionWith(request.getTransaction, wallet, signerAddress.toString).toFuture
  }

  override def broadcastTransaction(tx1: PBSignedTransaction): Future[PBSignedTransaction] = {
    import com.wavesplatform.network._

    val tx = tx1.toVanilla
    val result = for {
      r <- utx.putIfNew(tx)
      _ = if (r._1) allChannels.broadcastTx(tx, None) else ()
    } yield tx1

    result.toFuture
  }
}
