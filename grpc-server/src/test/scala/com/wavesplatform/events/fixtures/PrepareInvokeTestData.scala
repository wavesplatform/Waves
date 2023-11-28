package com.wavesplatform.events.fixtures

object PrepareInvokeTestData {
  val scriptTransferIssueAssetNum: Long = 21000
  val scriptTransferUnitNum: Long       = 22000
  val scriptTransferAssetNum: Long      = 25000
  val paymentNum: Long                  = 30000
  val sponsorFeeAssetNum: Long          = 35000
  val sponsorFeeIssueAssetNum: Long     = 40000
  val reissueNum: Int                   = 50000
  val burnNum: Int                      = 100000
  val leaseNum: Long                    = 200000
  val foo                               = "foo"
  val baz                               = "baz"
  val bar                               = "bar"

  val issueData: Map[String, Any] = Map(
    "name"        -> "issuedAssetName",
    "description" -> "asset_ride_description",
    "amount"      -> 416168000,
    "decimals"    -> 8,
    "nonce"       -> 1
  )

  val dataMap: Map[String, Any] = Map(
    "intVal"     -> 25400,
    "stringVal"  -> "test_string",
    "booleanVal" -> "true"
  )

  def invokeAssetScript(libVersion: Int): String =
    s"""
       |{-# STDLIB_VERSION $libVersion #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |@Callable(i)
       |func setData(assetId:ByteVector, address:ByteVector)={
       |let issueAsset = Issue("${issueData.apply("name")}","${issueData.apply("description")}",${issueData.apply("amount")},
       |${issueData.apply("decimals")},true,unit,${issueData.apply("nonce")})
       |let issueAssetId = issueAsset.calculateAssetId()
       |let lease =  Lease(Address(address), $leaseNum)
       |  [
       |    issueAsset,
       |    lease,
       |    LeaseCancel(lease.calculateLeaseId()),
       |    IntegerEntry("int", ${dataMap.apply("intVal")}),
       |    BinaryEntry("byte", assetId),
       |    BooleanEntry("bool", ${dataMap.apply("booleanVal")}),
       |    StringEntry("str", "${dataMap.apply("stringVal")}"),
       |    DeleteEntry("int"),
       |    Reissue(assetId, $reissueNum,true),
       |    Burn(assetId, $burnNum),
       |    ScriptTransfer(Address(address), $scriptTransferAssetNum, assetId),
       |    ScriptTransfer(Address(address), $scriptTransferIssueAssetNum, issueAssetId),
       |    ScriptTransfer(Address(address), $scriptTransferUnitNum, unit),
       |    SponsorFee(assetId, $sponsorFeeAssetNum),
       |    SponsorFee(issueAssetId, $sponsorFeeIssueAssetNum)
       |  ]
       |}
       |""".stripMargin

  def mainDAppScript(libVersion: Int): String =
    s"""
       |{-# STDLIB_VERSION $libVersion #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |@Callable(i)
       |func foo(acc1:ByteVector, acc2:ByteVector, a:Int, key1:String, assetId:ByteVector)={
       |strict res = invoke(Address(acc1),"$bar",[a, assetId, acc2],[AttachedPayment(assetId,$paymentNum)])
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

  def nestedDAppScript(firstRecipient: String, libVersion: Int): String =
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
       |    ScriptTransfer($firstRecipient, $scriptTransferAssetNum, assetId)
       |  ],
       | a * 2
       |)
       | case _ => throw("Incorrect invoke result for res2")
       | }
       |}
       |""".stripMargin

  def doubleNestedDAppScript(secondRecipient: String, libVersion: Int): String =
    s"""
       |{-# STDLIB_VERSION $libVersion #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |@Callable(i)
       |func $baz(a: Int) = {
       |(
       | [
       |   ScriptTransfer($secondRecipient, $scriptTransferUnitNum, unit)
       | ],
       |a + 2
       |)
       |}
       |""".stripMargin
}
