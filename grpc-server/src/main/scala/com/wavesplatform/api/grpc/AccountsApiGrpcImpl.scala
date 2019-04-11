package com.wavesplatform.api.grpc
import com.wavesplatform.api.common.CommonAccountApi
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.transaction.{Amount, AssetAmount}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
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
      case Right(transactions) => Observable.fromIterable(transactions)
      case Left(error)  => Observable.raiseError(new IllegalArgumentException(error))
    })

    val result = transactions.map { case (height, transaction) => TransactionWithHeight(Some(transaction.toPB), height) }
    responseObserver.completeWith(result)
  }
}
