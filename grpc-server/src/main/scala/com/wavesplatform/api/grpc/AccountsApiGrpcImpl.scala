package com.wavesplatform.api.grpc

import com.google.protobuf.wrappers.{BytesValue, StringValue}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.common.CommonAccountsApi
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.state.AccountScriptInfo
import com.wavesplatform.transaction.Asset
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.Future

class AccountsApiGrpcImpl(commonApi: CommonAccountsApi)(implicit sc: Scheduler) extends AccountsApiGrpc.AccountsApi {

  private def loadWavesBalance(address: Address): BalanceResponse = {
    val details = commonApi.balanceDetails(address)
    BalanceResponse().withWaves(
      BalanceResponse.WavesBalances(
        details.regular,
        details.generating,
        details.available,
        details.effective,
        details.leaseIn,
        details.leaseOut
      )
    )
  }
  private def assetBalanceResponse(v: (Asset.IssuedAsset, Long)): BalanceResponse =
    BalanceResponse().withAsset(Amount(v._1.id.toPBByteString, v._2))

  override def getBalances(request: BalancesRequest, responseObserver: StreamObserver[BalanceResponse]): Unit = responseObserver.interceptErrors {
    val addressOption: Option[Address] = if (request.address.isEmpty) None else Some(request.address.toAddress)
    val assetIds: Seq[Asset]           = request.assets.map(id => if (id.isEmpty) Asset.Waves else Asset.IssuedAsset(ByteStr(id.toByteArray)))

    val responseStream = (addressOption, assetIds) match {
      case (Some(address), Seq()) =>
        Observable(loadWavesBalance(address)) ++ commonApi.portfolio(address).map(assetBalanceResponse)
      case (Some(address), nonEmptyList) =>
        Observable
          .fromIterable(nonEmptyList)
          .map {
            case Asset.Waves           => loadWavesBalance(address)
            case ia: Asset.IssuedAsset => assetBalanceResponse(ia -> commonApi.assetBalance(address, ia))
          }
      case (None, Seq(_)) => // todo: asset distribution
        Observable.empty
      case (None, _) => // multiple distributions are not supported
        Observable.empty
    }

    responseObserver.completeWith(responseStream)
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

    val stream = if (request.key.nonEmpty) {
      println(s"\n\t${Thread.currentThread().getName} REQ: ${request.key}\n")
      val option = commonApi.data(request.address.toAddress, request.key)
      println(s"\n\t${Thread.currentThread().getName} RES: $option\n")
      Observable.fromIterable(option)
    } else {
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
