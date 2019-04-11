package com.wavesplatform.api.grpc
import com.wavesplatform.api.common.CommonAccountApi
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.transaction.{AssetAmount, DataTransactionData, PBTransactions}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.Future

class AccountsApiGrpcImpl(blockchain: Blockchain, functionalitySettings: FunctionalitySettings)(implicit sc: Scheduler)
    extends AccountsApiGrpc.AccountsApi {
  private[this] val commonApi = new CommonAccountApi(blockchain, functionalitySettings)

  override def getPortfolio(request: PortfolioRequest): Future[PortfolioResponse] = Future {
    val wavesOption = if (request.includeWaves) {
      val details = commonApi.balanceDetails(request.address.toAddress)
      Some(PortfolioResponse.WavesBalances(details.regular, details.generating, details.available, details.effective))
    } else {
      None
    }

    val assetIdSet = request.assets.map(_.toByteStr).toSet
    val assets =
      if (assetIdSet.isEmpty)
        Seq.empty
      else
        commonApi
          .portfolio(request.address.toAddress)
          .toSeq
          .collect {
            case (IssuedAsset(assetId), balance) if assetIdSet.contains(assetId) =>
              AssetAmount(assetId, balance)
          }

    PortfolioResponse(wavesOption, assets)
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
