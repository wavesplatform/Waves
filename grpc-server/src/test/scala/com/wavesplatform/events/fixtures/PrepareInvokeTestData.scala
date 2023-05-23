package com.wavesplatform.events.fixtures

import java.util.concurrent.ThreadLocalRandom.current

object PrepareInvokeTestData {
  val burnNum: Int                      = current.nextInt(1, 10000)
  val leaseNum: Long                     = current.nextLong(10001, 20000)
  val reissueNum: Int                   = current.nextInt(100000, 200000)
  val scriptTransferAssetNum: Long      = current.nextInt(20001, 20600)
  val scriptTransferUnitNum: Long       = current.nextInt(21001, 22001)
  val scriptTransferIssueAssetNum: Long = current.nextInt(20601, 21000)
  val sponsorFeeAssetNum: Long          = current.nextInt(23001, 24000)
  val sponsorFeeIssueAssetNum: Long     = current.nextInt(25001, 26000)
  val paymentNum: Long                  = current.nextLong(12300, 32487)
  val baz                               = "baz"
  val bar                               = "bar"
  val libVersion: Int                   = current.nextInt(5, 7)

  val issueData: Map[String, Any] = Map(
    "name"        -> "issuedAssetName",
    "description" -> "asset_ride_description",
    "amount"      -> current.nextLong(416168074, 918171615),
    "decimals"    -> current.nextLong(0, 8),
    "nonce"       -> current.nextLong(0, 8)
  )

  val dataMap: Map[String, Any] = Map(
    "intVal"                      -> current.nextInt(27001, 28000),
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

  val mainDAppScript: String =
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
       |    ScriptTransfer($firstRecipient, $scriptTransferUnitNum, assetId)
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
       |   ScriptTransfer($secondRecipient, $scriptTransferUnitNum, unit)
       | ],
       |a + 2
       |)
       |}
       |""".stripMargin

}
