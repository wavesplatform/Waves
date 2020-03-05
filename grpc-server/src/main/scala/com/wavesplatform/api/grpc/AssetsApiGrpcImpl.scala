package com.wavesplatform.api.grpc

import com.wavesplatform.api.common.CommonAssetsApi
import com.wavesplatform.api.http.ApiError.TransactionDoesNotExist
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.EstimatorProvider._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.IssuedAsset
import monix.execution.Scheduler

import scala.concurrent.Future

class AssetsApiGrpcImpl(blockchain: Blockchain)(implicit sc: Scheduler) extends AssetsApiGrpc.AssetsApi {
  private[this] val commonApi = new CommonAssetsApi(blockchain)

  override def getInfo(request: AssetRequest): Future[AssetInfoResponse] = Future {
    val result =
      for (info <- commonApi.fullInfo(IssuedAsset(request.assetId)))
        yield AssetInfoResponse(
          info.description.issuer,
          info.description.name.toStringUtf8,
          info.description.description.toStringUtf8,
          info.description.decimals,
          info.description.reissuable,
          info.description.totalVolume.longValue(),
          info.description.script.map(
            script =>
              ScriptData(
                PBTransactions.toPBScript(script).bytes,
                script.expr.toString,
                Script.estimate(script, blockchain.estimator).explicitGet()
              )
          ),
          info.description.sponsorship,
          info.issueTransaction.map(_.toPB),
          info.sponsorBalance.getOrElse(0)
        )
    result.explicitGetErr(TransactionDoesNotExist)
  }
}
