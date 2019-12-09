package com.wavesplatform.api.grpc

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
          info.description.name.fold(bs => new String(bs.arr), identity),
          info.description.description.fold(bs => new String(bs.arr), identity),
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
          info.issueTransaction.map(_.toPB),
          info.sponsorBalance.getOrElse(0)
        )
    result.explicitGetErr(TransactionDoesNotExist)
  }
}
