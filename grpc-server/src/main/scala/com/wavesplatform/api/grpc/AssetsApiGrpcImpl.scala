package com.wavesplatform.api.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.api.common.CommonAssetsApi
import com.wavesplatform.api.http.ApiError.TransactionDoesNotExist
import com.wavesplatform.transaction.Asset.IssuedAsset
import monix.execution.Scheduler

import scala.concurrent.Future

class AssetsApiGrpcImpl(assetsApi: CommonAssetsApi)(implicit sc: Scheduler) extends AssetsApiGrpc.AssetsApi {
  override def getInfo(request: AssetRequest): Future[AssetInfoResponse] = Future {
    val result =
      for (info <- assetsApi.fullInfo(IssuedAsset(request.assetId)))
        yield AssetInfoResponse(
          info.description.issuer,
          ByteString.copyFrom(info.description.name),
          ByteString.copyFrom(info.description.description),
          info.description.decimals,
          info.description.reissuable,
          info.description.totalVolume.longValue(),
          info.description.script.map {
            case (script, complexity) =>
              ScriptData(
                script.bytes().toPBByteString,
                script.expr.toString,
                complexity
              )
          },
          info.description.sponsorship,
          Some(info.issueTransaction.toPB),
          info.sponsorBalance.getOrElse(0)
        )
    result.explicitGetErr(TransactionDoesNotExist)
  }
}
