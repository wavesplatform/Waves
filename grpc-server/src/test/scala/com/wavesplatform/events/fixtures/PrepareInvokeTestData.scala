package com.wavesplatform.events.fixtures

object PrepareInvokeTestData {
  val invokeAssetScript: Option[String] = Option.apply(
    s"""
       |{-# STDLIB_VERSION 5 #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |@Callable(i)
       |func setData(assetId:ByteVector)={
       |let issueAsset = Issue("issuedAsset67", "asset ride script 796669",416168074,2,true,unit,8)
       |let issueAssetId = issueAsset.calculateAssetId()
       |  [
       |    issueAsset,
       |    Reissue(assetId,51389,true),
       |    Burn(assetId,51389)
       |  ]
       |}
       |""".stripMargin
  )
}
