package com.wavesplatform.api.grpc
import com.google.protobuf.empty.Empty
import com.wavesplatform.account.PublicKey
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.http.TransactionNotExists
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.{PBSignedTransaction, PBTransaction, VanillaTransaction}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.Future
import scala.util.Try

class TransactionsApiGrpcImpl(functionalitySettings: FunctionalitySettings,
                              wallet: Wallet,
                              blockchain: Blockchain,
                              utx: UtxPool,
                              broadcast: VanillaTransaction => Unit)(implicit sc: Scheduler)
    extends TransactionsApiGrpc.TransactionsApi {

  private[this] val commonApi = new CommonTransactionsApi(blockchain, utx, wallet, broadcast)

  override def getTransactions(request: TransactionsRequest, responseObserver: StreamObserver[TransactionWithHeight]): Unit = {
    val stream = commonApi
      .transactionsByAddress(request.getRecipient.toAddress, Option(request.fromId.toByteStr).filterNot(_.isEmpty))
      .map { case (height, transaction) => TransactionWithHeight(Some(transaction.toPB), height) }

    responseObserver.completeWith(stream)
  }

  override def getTransaction(request: TransactionRequest): Future[TransactionWithHeight] = {
    commonApi
      .transactionById(request.transactionId)
      .map { case (height, transaction) => TransactionWithHeight(Some(transaction.toPB), height) }
      .toFuture(TransactionNotExists)
  }

  override def getUnconfirmedTransactions(request: Empty, responseObserver: StreamObserver[PBSignedTransaction]): Unit = {
    val stream = Observable(commonApi.unconfirmedTransactions().map(_.toPB): _*)
    responseObserver.completeWith(stream)
  }

  override def getUnconfirmedTransaction(request: TransactionRequest): Future[PBSignedTransaction] = {
    commonApi
      .unconfirmedTransactionById(request.transactionId)
      .map(_.toPB)
      .toFuture(TransactionNotExists)
  }

  override def calculateFee(request: PBTransaction): Future[CalculateFeeResponse] = {
    commonApi.calculateFee(request.toVanilla).map { case (assetId, assetAmount, _) => CalculateFeeResponse(assetId.protoId, assetAmount) }.toFuture
  }

  override def signTransaction(request: SignRequest): Future[PBSignedTransaction] = {
    def signTransactionWith(tx: PBTransaction, wallet: Wallet, signerAddress: String): Either[ValidationError, PBSignedTransaction] =
      for {
        sender <- wallet.findPrivateKey(tx.sender.toString)
        signer <- if (tx.sender.toString == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
        tx     <- Try(tx.signed(signer.privateKey)).toEither.left.map(GenericError(_))
      } yield tx

    val signerAddress: PublicKey = if (request.signerPublicKey.isEmpty) request.getTransaction.sender else request.signerPublicKey.toPublicKeyAccount
    signTransactionWith(request.getTransaction, wallet, signerAddress.toString).toFuture
  }

  override def broadcastTransaction(tx: PBSignedTransaction): Future[PBSignedTransaction] = {
    commonApi
      .broadcastTransaction(tx.toVanilla)
      .map(_.toPB)
      .resultE
      .toFuture
  }
}
