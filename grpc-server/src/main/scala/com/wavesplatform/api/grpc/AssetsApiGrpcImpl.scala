package com.wavesplatform.api.grpc

import com.wavesplatform.account.Address
import com.wavesplatform.api.common.{CommonAccountsApi, CommonAssetsApi}
import com.wavesplatform.api.http.ApiError.TransactionDoesNotExist
import com.wavesplatform.protobuf.*
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.state.{AssetDescription, AssetScriptInfo}
import com.wavesplatform.transaction.Asset.IssuedAsset
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.Future

class AssetsApiGrpcImpl(assetsApi: CommonAssetsApi, accountsApi: CommonAccountsApi)(implicit sc: Scheduler) extends AssetsApiGrpc.AssetsApi {
  override def getInfo(request: AssetRequest): Future[AssetInfoResponse] = Future {
    val result =
      for (info <- assetsApi.fullInfo(IssuedAsset(request.assetId.toByteStr)))
        yield {
          val result = assetInfoResponse(info.description).withSponsorBalance(info.sponsorBalance.getOrElse(0))
          info.issueTransaction.map(_.toPB).fold(result)(result.withIssueTransaction)
        }
    result.explicitGetErr(TransactionDoesNotExist)
  }

  override def getNFTList(request: NFTRequest, responseObserver: StreamObserver[NFTResponse]): Unit = responseObserver.interceptErrors {
    val addressOption: Option[Address]    = if (request.address.isEmpty) None else Some(request.address.toAddress)
    val afterAssetId: Option[IssuedAsset] = if (request.afterAssetId.isEmpty) None else Some(IssuedAsset(request.afterAssetId.toByteStr))

    val responseStream = addressOption match {
      case Some(address) =>
        accountsApi
          .nftList(address, afterAssetId)
          .concatMapIterable(_.map { case (a, d) =>
            NFTResponse(a.id.toByteString, Some(assetInfoResponse(d)))
          })
          .takeLast(request.limit)
      case _ => Observable.empty
    }

    responseObserver.completeWith(responseStream)
  }

  private def assetInfoResponse(d: AssetDescription): AssetInfoResponse =
    AssetInfoResponse(
      d.issuer.toByteString,
      d.name.toStringUtf8,
      d.description.toStringUtf8,
      d.decimals,
      d.reissuable,
      d.totalVolume.longValue,
      d.script.map { case AssetScriptInfo(script, complexity) =>
        ScriptData(
          PBTransactions.toPBScript(Some(script)),
          script.expr.toString,
          complexity
        )
      },
      d.sponsorship,
      sequenceInBlock = d.sequenceInBlock,
      issueHeight = d.issueHeight
    )
}
