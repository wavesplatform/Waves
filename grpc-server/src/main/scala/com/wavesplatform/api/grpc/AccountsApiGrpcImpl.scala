package com.wavesplatform.api.grpc
import com.wavesplatform.api.common.CommonAccountApi
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.transaction.{Amount, AssetAmount, DataTransactionData, PBTransactions}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.ValidationError.GenericError
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.Future

class AccountsApiGrpcImpl(blockchain: Blockchain, functionalitySettings: FunctionalitySettings)(implicit sc: Scheduler)
    extends AccountsApiGrpc.AccountsApi {
  private[this] val commonApi = new CommonAccountApi(blockchain, functionalitySettings)

  override def getBalance(request: BalanceRequest): Future[BalanceResponse] = Future {
    BalanceResponse(commonApi.balance(request.address.toAddress, request.confirmations))
  }

  override def getEffectiveBalance(request: BalanceRequest): Future[BalanceResponse] = Future {
    BalanceResponse(commonApi.effectiveBalance(request.address.toAddress, request.confirmations))
  }

  override def getBalanceDetails(request: BalanceRequest): Future[BalanceDetailsResponse] = Future {
    val details = commonApi.balanceDetails(request.address.toAddress)
    BalanceDetailsResponse(details.regular, details.generating, details.available, details.effective)
  }

  override def getAssetBalance(request: AssetBalanceRequest): Future[BalanceResponse] = Future {
    BalanceResponse(commonApi.assetBalance(request.address.toAddress, IssuedAsset(request.assetId.toByteStr)))
  }

  override def getPortfolio(request: AccountRequest, responseObserver: StreamObserver[Amount]): Unit = {
    val result = Observable
      .defer(Observable.fromIterable(commonApi.portfolio(request.address.toAddress)))
      .map {
        case (IssuedAsset(assetId), balance) =>
          Amount().withAssetAmount(AssetAmount(assetId, balance))

        case (Waves, balance) =>
          Amount().withWavesAmount(balance)
      }

    responseObserver.completeWith(result)
  }

  override def getScript(request: AccountRequest): Future[AccountScriptResponse] = Future {
    val desc = commonApi.script(request.address.toAddress)
    AccountScriptResponse(desc.script.getOrElse(ByteStr.empty).toPBByteString, desc.scriptText.getOrElse(""), desc.complexity, desc.extraFee)
  }

  override def getActiveLeases(request: AccountRequest, responseObserver: StreamObserver[TransactionWithHeight]): Unit = {
    val transactions = Observable.defer(commonApi.activeLeases(request.address.toAddress) match {
      case Right(txs)  => Observable.fromIterable(txs)
      case Left(error) => Observable.raiseError(new IllegalArgumentException(error))
    })

    val result = transactions.map { case (height, transaction) => TransactionWithHeight(Some(transaction.toPB), height) }
    responseObserver.completeWith(result)
  }

  override def getData(request: DataRequest): Future[DataTransactionData.DataEntry] = Future {
    val entry = commonApi
      .data(request.address.toAddress, request.key)
      .getOrElse(throw GenericError("Data key not found"))

    PBTransactions.toPBDataEntry(entry)
  }

  override def getDataStream(request: DataRequest, responseObserver: StreamObserver[DataTransactionData.DataEntry]): Unit = {
    val stream = commonApi
      .dataStream(request.address.toAddress, key => request.key.isEmpty || key.matches(request.key))
      .map(PBTransactions.toPBDataEntry)

    responseObserver.completeWith(stream)
  }
}
