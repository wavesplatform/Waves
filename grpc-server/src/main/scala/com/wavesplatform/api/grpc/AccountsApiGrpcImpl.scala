package com.wavesplatform.api.grpc
import com.wavesplatform.api.common.CommonAccountApi
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.transaction.{AssetAmount, PBTransactions}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.IssuedAsset
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.Future

class AccountsApiGrpcImpl(blockchain: Blockchain, functionalitySettings: FunctionalitySettings)(implicit sc: Scheduler)
    extends AccountsApiGrpc.AccountsApi {
  private[this] val commonApi = new CommonAccountApi(blockchain, functionalitySettings)

  override def getBalances(request: BalancesRequest, responseObserver: StreamObserver[BalanceResponse]): Unit = {
    val wavesOption = if (request.includeWaves) {
      val details = commonApi.balanceDetails(request.address.toAddress)
      Some(
        BalanceResponse.WavesBalances(details.regular, details.generating, details.available, details.effective, details.leaseIn, details.leaseOut))
    } else {
      None
    }

    val assetIdSet = request.assets.map(_.toByteStr).toSet
    val assets =
      if (assetIdSet.isEmpty)
        Observable.empty
      else
        Observable
          .defer(Observable.fromIterable(commonApi.portfolio(request.address.toAddress)))
          .collect {
            case (IssuedAsset(assetId), balance) if request.includeAllAssets || assetIdSet.contains(assetId) =>
              AssetAmount(assetId, balance)
          }

    val resultStream = Observable
      .fromIterable(wavesOption)
      .map(wb => BalanceResponse(request.address, BalanceResponse.Balance.Waves(wb)))
      .++(assets.map(am => BalanceResponse(request.address, BalanceResponse.Balance.Asset(am))))

    responseObserver.completeWith(resultStream)
  }

  override def getScript(request: AccountRequest): Future[ScriptData] = Future {
    val desc = commonApi.script(request.address.toAddress)
    ScriptData(desc.script.getOrElse(ByteStr.empty).toPBByteString, desc.scriptText.getOrElse(""), desc.complexity)
  }

  override def getActiveLeases(request: AccountRequest, responseObserver: StreamObserver[TransactionWithHeight]): Unit = {
    val transactions = Observable.defer(commonApi.activeLeases(request.address.toAddress) match {
      case Right(txs)  => Observable.fromIterable(txs)
      case Left(error) => Observable.raiseError(new IllegalArgumentException(error))
    })

    val result = transactions.map { case (height, transaction) => TransactionWithHeight(Some(transaction.toPB), height) }
    responseObserver.completeWith(result)
  }

  override def getDataEntries(request: DataRequest, responseObserver: StreamObserver[DataEntryResponse]): Unit = {
    val stream = commonApi
      .dataStream(request.address.toAddress, key => request.key.isEmpty || key.matches(request.key))
      .map(de => DataEntryResponse(request.address, Some(PBTransactions.toPBDataEntry(de))))

    responseObserver.completeWith(stream)
  }
}
