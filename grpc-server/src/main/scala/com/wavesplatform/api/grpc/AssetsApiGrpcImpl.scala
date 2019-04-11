package com.wavesplatform.api.grpc
import com.google.protobuf.ByteString
import com.wavesplatform.api.common.CommonAssetsApi
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.ValidationError.GenericError
import monix.execution.Scheduler

import scala.concurrent.Future

class AssetsApiGrpcImpl(blockchain: Blockchain)(implicit sc: Scheduler) extends AssetsApiGrpc.AssetsApi {
  private[this] val commonApi = new CommonAssetsApi(blockchain)

  override def getFullInfo(request: AssetRequest): Future[AssetInfoResponse] = Future {
    val info = commonApi.fullInfo(IssuedAsset(request.assetId))
      .getOrElse(throw GenericError("Asset not found"))

    AssetInfoResponse(
      info.description.issuer,
      ByteString.copyFrom(info.description.name),
      ByteString.copyFrom(info.description.description),
      info.description.decimals,
      info.description.reissuable,
      info.description.totalVolume.longValue(),
      info.description.script.map(_.bytes()).getOrElse(ByteStr.empty).toPBByteString,
      info.description.sponsorship,
      Some(info.issueTransaction.toPB),
      info.sponsorBalance.getOrElse(0)
    )
  }

  override def getScript(request: AssetRequest): Future[AssetScriptResponse] = Future {
    val info = commonApi.description(IssuedAsset(request.assetId))
      .getOrElse(throw GenericError("Asset not found"))

    AssetScriptResponse(info.script.map(_.bytes()).getOrElse(ByteStr.empty).toPBByteString)
  }
}
