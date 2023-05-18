package com.wavesplatform.events.fixtures

import java.util.concurrent.ThreadLocalRandom.current

object PrepareInvokeTestData {
  val scriptTransferUnitInt: Long       = current.nextLong(21001, 22001)
  val issueAssetAmount: Long            = current.nextInt(416168074, 918171615)
  val scriptTransferIssueAssetInt: Long = current.nextInt(20601, 21000)
  val payment: Long                     = current.nextLong(12300, 32487)
  val baz                               = "baz"
  val bar                               = "bar"
  val libVersion: Int                   = current.nextInt(5, 7)

  val dataMap: Map[String, Any] = Map(
    "burnInt"                     -> current.nextInt(1, 10000),
    "leaseInt"                    -> current.nextInt(10001, 20000),
    "scriptTransferAssetInt"      -> current.nextInt(20001, 20600),
    "scriptTransferIssueAssetInt" -> scriptTransferIssueAssetInt,
    "scriptTransferUnitInt"       -> scriptTransferUnitInt,
    "sponsorFeeAssetInt"          -> current.nextInt(23001, 24000),
    "sponsorFeeIssueAssetInt"     -> current.nextInt(25001, 26000),
    "intVal"                      -> current.nextInt(27001, 28000),
    "reissueInt"                  -> current.nextInt(100000, 200000),
    "issueAssetName"              -> "issuedAssetName",
    "issueAssetDescription"       -> "asset_ride_description",
    "issueAssetAmount"            -> issueAssetAmount,
    "issueAssetDecimals"          -> current.nextInt(0, 8),
    "issueAssetNonce"             -> current.nextInt(0, 8),
    "stringVal"                   -> "test_string",
    "booleanVal"                  -> "true"
  )

  val invokeAssetScript: String =
    s"""
       |{-# STDLIB_VERSION $libVersion #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |@Callable(i)
       |func setData(assetId:ByteVector, address:ByteVector)={
       |let issueAsset = Issue("${dataMap.apply("issueAssetName")}","${dataMap.apply("issueAssetDescription")}",${dataMap
      .apply("issueAssetAmount")},${dataMap.apply("issueAssetDecimals")},true,unit,${dataMap.apply("issueAssetNonce")})
       |let issueAssetId = issueAsset.calculateAssetId()
       |let lease =  Lease(Address(address), ${dataMap.apply("leaseInt")})
       |  [
       |    issueAsset,
       |    lease,
       |    LeaseCancel(lease.calculateLeaseId()),
       |    IntegerEntry("int", ${dataMap.apply("intVal")}),
       |    BinaryEntry("byte", assetId),
       |    BooleanEntry("bool", ${dataMap.apply("booleanVal")}),
       |    StringEntry("str", "${dataMap.apply("stringVal")}"),
       |    DeleteEntry("int"),
       |    Reissue(assetId, ${dataMap.apply("reissueInt")},true),
       |    Burn(assetId, ${dataMap.apply("burnInt")}),
       |    ScriptTransfer(Address(address), ${dataMap.apply("scriptTransferAssetInt")}, assetId),
       |    ScriptTransfer(Address(address), ${dataMap.apply("scriptTransferIssueAssetInt")}, issueAssetId),
       |    ScriptTransfer(Address(address), ${dataMap.apply("scriptTransferUnitInt")}, unit),
       |    SponsorFee(assetId, ${dataMap.apply("sponsorFeeAssetInt")}),
       |    SponsorFee(issueAssetId, ${dataMap.apply("sponsorFeeIssueAssetInt")})
       |  ]
       |}
       |""".stripMargin

  val mainDAppScript: String =
    s"""
       |{-# STDLIB_VERSION $libVersion #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |@Callable(i)
       |func setData(acc1:ByteVector, acc2:ByteVector, a:Int, key1:String, assetId:ByteVector)={
       |strict res = invoke(Address(acc2),"$bar",[a, assetId, acc1],[AttachedPayment(assetId,$payment)])
       |match res {
       |   case r : Int =>
       |(
       | [
       |   IntegerEntry(key1, r)
       | ]
       |)
       |	case _ => throw("Incorrect invoke result for res in dApp 1")
       | }
       |}
       |""".stripMargin

  def nestedDAppScript(firstRecipient: String): String =
    s"""
       |{-# STDLIB_VERSION $libVersion #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |@Callable(i)
       |func $bar(a: Int, assetId: ByteVector, acc1: ByteVector)={
       |strict res2 = invoke(Address(acc1),"$baz",[a],[])
       |match res2 {
       |case r: Int =>
       |(
       |  [
       |    ScriptTransfer($firstRecipient, ${dataMap.apply("scriptTransferAssetInt")}, assetId)
       |  ],
       | a * 2
       |)
       | case _ => throw("Incorrect invoke result for res2")
       | }
       |}
       |""".stripMargin

  def doubleNestedDAppScript(secondRecipient: String): String =
    s"""
       |{-# STDLIB_VERSION $libVersion #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |@Callable(i)
       |func $baz(a: Int) = {
       |(
       | [
       |   ScriptTransfer($secondRecipient, ${dataMap.apply("scriptTransferUnitInt")}, unit)
       | ],
       |a + 2
       |)
       |}
       |""".stripMargin

}
