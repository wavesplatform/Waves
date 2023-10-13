package com.wavesplatform.api.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.common.{CommonAccountsApi, LeaseInfo}
import com.wavesplatform.api.http.ApiError.CustomValidationError
import com.wavesplatform.protobuf.*
import com.wavesplatform.protobuf.transaction.{PBRecipients, PBTransactions}
import com.wavesplatform.protobuf.utils.PBImplicitConversions.fromAssetIdAndAmount
import com.wavesplatform.transaction.Asset
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.Future

class AccountsApiGrpcImpl(commonApi: CommonAccountsApi)(implicit sc: Scheduler) extends AccountsApiGrpc.AccountsApi {

  private def loadWavesBalance(address: Address): BalanceResponse = {
    commonApi
      .balanceDetails(address)
      .fold(
        e => throw GRPCErrors.toStatusException(CustomValidationError(e)),
        details =>
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
      )
  }

  private def assetBalanceResponse(v: (Asset.IssuedAsset, Long)): BalanceResponse =
    BalanceResponse().withAsset(fromAssetIdAndAmount(v))

  override def getBalances(request: BalancesRequest, responseObserver: StreamObserver[BalanceResponse]): Unit = responseObserver.interceptErrors {
    val addressOption: Option[Address] = if (request.address.isEmpty) None else Some(request.address.toAddress())
    val assetIds: Seq[Asset]           = request.assets.map(id => if (id.isEmpty) Asset.Waves else Asset.IssuedAsset(id.toByteStr))

    val responseStream = (addressOption, assetIds) match {
      case (Some(address), Seq()) =>
        // FIXME: Strict loading because of segfault in leveldb
        Observable(loadWavesBalance(address)) ++ Observable.fromIterator(
          commonApi.portfolio(address).map(assetBalanceResponse).toListL.map(_.iterator)
        )
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

  override def getScript(request: AccountRequest): Future[ScriptResponse] = Future {
    commonApi.script(request.address.toAddress()) match {
      case Some(desc) => ScriptResponse(PBTransactions.toPBScript(Some(desc.script)), desc.script.expr.toString, desc.verifierComplexity, desc.publicKey.toByteString)
      case None       => ScriptResponse()
    }
  }

  // TODO: Lease info route?
  override def getActiveLeases(request: AccountRequest, responseObserver: StreamObserver[LeaseResponse]): Unit =
    responseObserver.interceptErrors {
      val result =
        Observable.fromIterator(
          commonApi
            .activeLeases(request.address.toAddress())
            .map { case LeaseInfo(leaseId, originTransactionId, sender, recipient, amount, height, status, _, _) =>
              assert(status == LeaseInfo.Status.Active)
              LeaseResponse(
                leaseId.toByteString,
                originTransactionId.toByteString,
                ByteString.copyFrom(sender.bytes),
                Some(PBRecipients.create(recipient)),
                amount,
                height
              )
            }
            .toListL // FIXME: Strict loading because of segfault in leveldb
            .map(_.iterator)
        )

      responseObserver.completeWith(result)
    }

  override def getDataEntries(request: DataRequest, responseObserver: StreamObserver[DataEntryResponse]): Unit = responseObserver.interceptErrors {
    val stream = if (request.key.nonEmpty) {
      Observable.fromIterable(commonApi.data(request.address.toAddress(), request.key))
    } else {
      // FIXME: Strict loading because of segfault in leveldb
      Observable.fromIterator(commonApi.dataStream(request.address.toAddress(), Option(request.key).filter(_.nonEmpty)).toListL.map(_.iterator))
    }

    responseObserver.completeWith(stream.map(de => DataEntryResponse(request.address, Some(PBTransactions.toPBDataEntry(de)))))
  }

  override def resolveAlias(request: String): Future[ByteString] =
    Future {
      val result = for {
        alias   <- Alias.create(request)
        address <- commonApi.resolveAlias(alias)
      } yield ByteString.copyFrom(address.bytes)

      result.explicitGetErr()
    }
}
