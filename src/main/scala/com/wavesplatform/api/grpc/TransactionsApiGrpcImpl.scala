package com.wavesplatform.api.grpc
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.settings.{FunctionalitySettings, RestAPISettings}
import com.wavesplatform.state.Blockchain
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

  private[this] val TransactionsBatchLimit = 1000

  override def transactionsByAddress(request: TransactionsByAddressRequest, responseObserver: StreamObserver[Transaction]): Unit = {
    def getTransactionsFromId(address: Address, fromId: Option[ByteStr]): Observable[Transaction] = {
      def getResponse(address: Address, limit: Int, fromId: Option[ByteStr]): Either[String, Seq[Transaction]] = {
        blockchain
          .addressTransactions(address, Set.empty, limit, fromId)
          .map(_.map(_._2.toPB))
      }

      val observableTask = Task(getResponse(address, TransactionsBatchLimit, fromId)) map {
        case Right(transactions) => if(transactions.isEmpty) Observable.empty else Observable(transactions: _*) ++ Observable.defer(getTransactionsFromId(address, Some(transactions.last.id())))
        case Left(err)           => Observable.raiseError(new Exception(err))
      }

      Observable.fromTask(observableTask).flatten
    }

    val stream = getTransactionsFromId(request.address.toAddress, Option(request.fromId).filterNot(_.isEmpty))
    val limitedStream = if (request.limit > 0) stream.take(request.limit) else stream
    responseObserver.completeWith(limitedStream)
  }

  override def transactionById(request: TransactionByIdRequest): Future[Transaction] = {
    Future(blockchain.transactionInfo(request.transactionId)).flatMap {
      case Some((_, transaction)) => Future.successful(transaction.toPB)
      case None                   => Future.failed(new NoSuchElementException("No such transaction"))
    }
  }

  override def unconfirmedTransactions(request: LimitedRequest, responseObserver: StreamObserver[Transaction]): Unit = {
    responseObserver.completeWith(Observable(utx.all.take(request.limit): _*).map(_.toPB))
  }
}
