package com.wavesplatform.api.grpc
import com.google.protobuf.empty.Empty
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.settings.{FunctionalitySettings, RestAPISettings}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.CommonValidation
import com.wavesplatform.transaction.TransactionFactory
import com.wavesplatform.transaction.protobuf.PBTransaction._
import com.wavesplatform.transaction.protobuf.Transaction
import com.wavesplatform.utils.Time
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import io.grpc.stub.StreamObserver
import io.netty.channel.group.ChannelGroup
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable

import scala.concurrent.Future

class TransactionsApiGrpcImpl(settings: RestAPISettings,
                              functionalitySettings: FunctionalitySettings,
                              wallet: Wallet,
                              blockchain: Blockchain,
                              utx: UtxPool,
                              allChannels: ChannelGroup,
                              time: Time)
    extends TransactionsApiGrpc.TransactionsApi {

  private[this] val TransactionsBatchLimit = 100

  override def transactionsByAddress(request: TransactionsByAddressRequest, responseObserver: StreamObserver[Transaction]): Unit = {
    def getTransactionsFromId(address: Address, fromId: Option[ByteStr]): Observable[Transaction] = {
      def getResponse(address: Address, limit: Int, fromId: Option[ByteStr]): Either[String, Seq[Transaction]] = {
        blockchain
          .addressTransactions(address, Set.empty, limit, fromId)
          .map(_.map(_._2.toPB))
      }

      val observableTask = Task(getResponse(address, TransactionsBatchLimit, fromId)) map {
        case Right(transactions) =>
          if (transactions.isEmpty) Observable.empty
          else Observable(transactions: _*) ++ Observable.defer(getTransactionsFromId(address, Some(transactions.last.id())))
        case Left(err) => Observable.raiseError(new IllegalArgumentException(err))
      }

      Observable.fromTask(observableTask).flatten
    }

    val stream = getTransactionsFromId(request.address.toAddress, Option(request.fromId).filterNot(_.isEmpty))
    responseObserver.completeWith(stream)
  }

  override def transactionById(request: TransactionByIdRequest): Future[Transaction] = {
    blockchain
      .transactionInfo(request.transactionId)
      .map(_._2.toPB)
      .toFuture
  }

  override def unconfirmedTransactions(request: Empty, responseObserver: StreamObserver[Transaction]): Unit = {
    val stream = Observable(utx.all: _*).map(_.toPB)
    responseObserver.completeWith(stream)
  }

  override def unconfirmedTransactionById(request: TransactionByIdRequest): Future[Transaction] = {
    utx
      .transactionById(request.transactionId)
      .map(_.toPB)
      .toFuture
  }

  override def calculateFee(request: Transaction): Future[CalculateFeeResponse] = {
    CommonValidation
      .getMinFee(blockchain, functionalitySettings, blockchain.height, request)
      .map { case (assetId, assetAmount, _) => CalculateFeeResponse(assetId, assetAmount) }
      .toFuture
  }

  override def signTransaction(request: TransactionSignRequest): Future[Transaction] = {
    val signerAddress = if (request.signer.isEmpty) request.transaction.sender.toString else request.signer
    TransactionFactory.protobuf(request.transaction, wallet, signerAddress).toFuture
  }

  override def broadcastTransaction(tx: Transaction): Future[Transaction] = {
    import com.wavesplatform.network._

    val result = for {
      (added, _) <- utx.putIfNew(tx)
      _ = if (added) allChannels.broadcastTx(tx, None)
    } yield tx

    result.toFuture
  }
}
