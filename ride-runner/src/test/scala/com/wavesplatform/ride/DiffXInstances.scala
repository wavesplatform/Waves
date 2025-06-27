package com.wavesplatform.ride

import com.softwaremill.diffx.Diff
import com.softwaremill.diffx.generic.auto.*
import com.wavesplatform.account.{Address, AddressOrAlias, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.script.Script
import com.wavesplatform.ride.runner.input.*
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.{Asset, TxNonNegativeAmount}
import play.api.libs.json.{JsObject, JsValue}

trait DiffXInstances {
  given Diff[ByteStr]                        = Diff[String].contramap(_.toString)
  given Diff[StringOrBytesAsByteArray]       = Diff[ByteStr].contramap(ByteStr(_))
  given Diff[Script]                         = Diff[ByteStr].contramap(_.bytes())
  given Diff[PublicKey]                      = Diff[ByteStr].contramap(x => x)
  given Diff[JsValue]                        = Diff.derived[JsValue]
  given Diff[JsObject]                       = Diff.derived[JsObject]
  given Diff[Address]                        = Diff[ByteStr].contramap(a => ByteStr(a.bytes))
  given Diff[TxNonNegativeAmount]            = Diff[Long].contramap(_.value)
  given Diff[Asset.IssuedAsset]              = Diff[String].contramap(AssetPair.assetIdStr)
  given Diff[Asset]                          = Diff[String].contramap(AssetPair.assetIdStr)
  given Diff[RideRunnerLeaseBalance]         = Diff.derived[RideRunnerLeaseBalance]
  given Diff[RideRunnerDataEntry]            = Diff.derived[RideRunnerDataEntry]
  given Diff[Alias]                          = Diff[String].contramap(_.toString)
  given Diff[AddressOrAlias]                 = Diff[String].contramap(_.toString)
  given Diff[RideRunnerScriptInfo]           = Diff.derived[RideRunnerScriptInfo]
  given Diff[RideRunnerAccount]              = Diff.derived[RideRunnerAccount]
  given Diff[RideRunnerAsset]                = Diff.derived[RideRunnerAsset]
  given Diff[RideRunnerBlock]                = Diff.derived[RideRunnerBlock]
  given Diff[RideRunnerTransaction]          = Diff.derived[RideRunnerTransaction]
  given Diff[RideRunnerBlockchainState]      = Diff.derived[RideRunnerBlockchainState]
  given Diff[RideRunnerPostProcessingMethod] = Diff.derived[RideRunnerPostProcessingMethod]
  given Diff[RideRunnerTest]                 = Diff.derived[RideRunnerTest]
  given Diff[RideRunnerInput]                = Diff.derived[RideRunnerInput]
}
