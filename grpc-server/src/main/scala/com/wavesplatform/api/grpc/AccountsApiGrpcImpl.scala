package com.wavesplatform.api.grpc

import com.google.protobuf.wrappers.{BytesValue, StringValue}
import com.wavesplatform.account.Alias
import com.wavesplatform.api.common.CommonAccountsApi
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.state.AccountScriptInfo
import com.wavesplatform.transaction.Asset.IssuedAsset
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.Future

class AccountsApiGrpcImpl(commonApi: CommonAccountsApi)(implicit sc: Scheduler) extends AccountsApiGrpc.AccountsApi {

  override def getBalances(request: BalancesRequest, responseObserver: StreamObserver[BalanceResponse]): Unit = responseObserver.interceptErrors {
    val wavesOption = if (request.assets.exists(_.isEmpty)) {
      val details = commonApi.balanceDetails(request.address.toAddress)
      Some(
        BalanceResponse.WavesBalances(details.regular, details.generating, details.available, details.effective, details.leaseIn, details.leaseOut)
      )
    } else {
      None
    }

    val assetIdSet = request.assets.toSet
    val assets =
      if (assetIdSet.isEmpty)
        Observable.empty
      else
        commonApi
          .portfolio(request.address.toAddress)
          .collect {
            case (IssuedAsset(assetId), balance) if request.assets.isEmpty || assetIdSet.contains(assetId.toPBByteString) =>
              Amount(assetId, balance)
          }

    val resultStream = Observable
      .fromIterable(wavesOption)
      .map(wb => BalanceResponse().withWaves(wb))
      .++(assets.map(am => BalanceResponse().withAsset(am)))

    responseObserver.completeWith(resultStream)
  }

  override def getScript(request: AccountRequest): Future[ScriptData] = Future {
    commonApi.script(request.address.toAddress) match {
      case None => ScriptData()
      case Some(AccountScriptInfo(_, script, complexity, _)) =>
        ScriptData(script.bytes().toPBByteString, script.expr.toString, complexity)

    }
  }

  override def getActiveLeases(request: AccountRequest, responseObserver: StreamObserver[TransactionResponse]): Unit =
    responseObserver.interceptErrors {
      val transactions = commonApi.activeLeases(request.address.toAddress)
      val result       = transactions.map { case (height, transaction) => TransactionResponse(transaction.id(), height, Some(transaction.toPB)) }
      responseObserver.completeWith(result)
    }

  override def getDataEntries(request: DataRequest, responseObserver: StreamObserver[DataEntryResponse]): Unit = responseObserver.interceptErrors {


    val stream = if (request.key.nonEmpty)
      {
        println(s"\n\t${Thread.currentThread().getName} REQ: ${request.key}\n")
        val option = commonApi.data(request.address.toAddress, request.key)
        println(s"\n\t${Thread.currentThread().getName} RES: $option\n")
        Observable.fromIterable(option)
      }
    else {
      println("\n\tREQ: key is empty")
      commonApi.dataStream(request.address.toAddress, Option(request.key).filter(_.nonEmpty))
    }


    responseObserver.completeWith(stream.map(de => DataEntryResponse(request.address, Some(PBTransactions.toPBDataEntry(de)))))
  }

  override def resolveAlias(request: StringValue): Future[BytesValue] =
    Future {
      val result = for {
        alias   <- Alias.create(request.value)
        address <- commonApi.resolveAlias(alias)
      } yield BytesValue(address.bytes)

      result.explicitGetErr()
    }
}
