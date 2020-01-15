package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.state.BinaryDataEntry
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure
import com.wavesplatform.common.utils._
import com.wavesplatform.it.NodeConfigs

import scala.concurrent.duration._

class EstimatorTestSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val estimator = ScriptEstimatorV2

  private val featureHeight = 8

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(_.raw(s"""
                              | waves.blockchain.custom.functionality {
                              |   estimator-pre-check-height =  $featureHeight
                              |   pre-activated-features = {14 = 0}
                              |}""".stripMargin))
      .buildNonConflicting()

  private val smartAcc  = pkByAddress(firstAddress)
  private val callerAcc = pkByAddress(secondAddress)

  private val accScript = ScriptCompiler
    .compile(
      """
        |{-# STDLIB_VERSION 3 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |
        |func validate(sigs: String, i: Int) = {
        |    let msg = this.getBinaryValue("v32767")
        |    let s = this.getBinaryValue("s")
        |    let key = this.getBinaryValue("key")
        |
        |    if rsaVerify(SHA512, msg, s, key) then
        |        sigs + "_" + toBase64String(s.take(2))
        |    else throw("sig verification failed")
        |}
        |
        |@Callable(inv)
        |func default() = {
        |    let i = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,
        |            26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,
        |            51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,
        |            76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,
        |            101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,
        |            126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,
        |            151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,
        |            176,177,178,179,180,181,182,183,184,185,186,187,188,189]
        |
        |    let sigs = FOLD<190>(i, "", validate)
        |
        |    WriteSet([DataEntry("result", sigs.size())])
        |}
                                                """.stripMargin,
      estimator
    )
    .explicitGet()
    ._1
    .bytes()
    .base64

  private val assetScript = ScriptCompiler
    .compile(
      """
        |{-# STDLIB_VERSION 3 #-}
        |{-# CONTENT_TYPE EXPRESSION #-}
        |{-# SCRIPT_TYPE ASSET #-}
        |
        |func validate(sigs: String, i: Int) = {
        |    let msg = this.issuer.getBinaryValue("v32767")
        |    let s = this.issuer.getBinaryValue("s")
        |    let key = this.issuer.getBinaryValue("key")
        |
        |    if rsaVerify(SHA512, msg, s, key) then
        |        sigs + "_" + toBase64String(s.take(2))
        |    else throw("sig verification failed")
        |}
        |
        |
        |    let i = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,
        |            26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46]
        |
        |    let sigs = FOLD<47>(i, "", validate)
        |
        |    sigs.size() > 0
        |
                                        """.stripMargin,
      estimator
    )
    .explicitGet()
    ._1
    .bytes()
    .base64

  var issuedAssetId = ""

  test("send waves to accounts") {

    Seq(smartAcc.stringRepr, callerAcc.stringRepr).foreach(
      r =>
        sender
          .transfer(
            sender.address,
            recipient = r,
            assetId = None,
            amount = 5.waves,
            fee = minFee,
            waitForTx = true
          )
    )

    val value = ByteStr(("4djidnqe9lK09le4FvgRD0BTok55nHP8MKFotfUeQOWEfBkp6MV4skCceWgDGBnc" * 512).dropRight(1).getBytes)
    val data = List(
      BinaryDataEntry(
        "v32767",
        value
      ),
      BinaryDataEntry(
        "key",
        Base64.decode(
          "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA9TuTafHJ0vfVBzFmF0jpUgcYt9Evuqkrk5BYZQyMPK5mB6YCEKDpK+C57oEhKf4sWNhxhBy5pwOQVCXQFO+IWLyTreVDKg091gAosZNFES5uC1hHmMKJUUHoXirEa1Zk6dgc9ErtCe3tJmA5FwVeGG1zg+4PUR3DTBIbPojVFu0GneMU9/p2Yp42PmOwuCzBHSWJxLGqL6wFvQKevT12vesNRSpF+d4Dl6IxDONojJDuPhPdHYIRNaQiSiEWtZ2pwg5WCUTwyl9ZpUM8Cx5e3pwbaqq3vufsbImAAR0QtTbgfi/YaEXj5lrd3Y1T4QWJczEeQHsdKXNkY0ZaErzRVwIDAQAB"
        )
      ),
      BinaryDataEntry(
        "s",
        Base64.decode(
          "E1CFy1u245yo1UNJXVXtW/4H8YSVt+98S7ackF4r1reeJHXL3z8V4rUyiq8B2qVg39GBfGiP82UJlmLbOP9BEztIPdMyLI+mQUBdEPQ35oziXxbJndqnqJozdoSaYUfq4UpEk334y7l6E4xOQmW7IsMhgG+T4+oK6AKXtNxLCB4Dpcaz7xM+kdsi/37usNvKcAfitx3bKdSL0ukFsaDLGuyingqIcofa7lj4s/xybWI1d4xcjhRUFbOzslnWeuoxePUZ/UJvg2wOhG/HtTVbfVc6z0nvS+yfju/AqAiPUdcsfu8dfu5xZsxf0HiXU6vweFWUHV5tY6SauZuopkcJEw=="
        )
      )
    )
    sender.putData(smartAcc.stringRepr, data, 0.3.waves, waitForTx = true)
  }

  test("can issue scripted asset and set script fro asset before precheck activation") {
    issuedAssetId = sender
      .issue(
        smartAcc.stringRepr,
        "Test",
        "Test asset",
        1000,
        0,
        script = Some(assetScript),
        fee = issueFee,
        waitForTx = true
      )
      .id

    sender
      .setAssetScript(
        issuedAssetId,
        smartAcc.stringRepr,
        issueFee,
        Some(assetScript),
        waitForTx = true
      )
      .id

  }

  test("can set contract and invoke script before precheck activation") {
    sender.setScript(smartAcc.stringRepr, Some(accScript), setScriptFee + smartFee, waitForTx = true).id

    sender
      .invokeScript(
        callerAcc.stringRepr,
        smartAcc.stringRepr,
        Some("default"),
        List.empty,
        Seq.empty,
        smartMinFee,
        None,
        waitForTx = true
      )
      ._1
      .id

    sender.setScript(smartAcc.stringRepr, Some(accScript), setScriptFee + smartFee, waitForTx = true).id
  }

  test(s"wait height from to $featureHeight for precheck activation") {
    sender.waitForHeight(featureHeight + 1, 5.minutes)
  }

  test("can't issue asset after estimator v1 precheck activation") {

    assertBadRequestAndMessage(
      sender.issue(
        smartAcc.stringRepr,
        "Test",
        "Test asset",
        1000,
        8,
        fee = issueFee+smartFee,
        script = Some(assetScript)
      ),
      "Script is too complex: 33280 > 4000"
    )
  }

  test("can't set script for issued asset after estimator v1 precheck activation") {
    assertBadRequestAndMessage(
      sender.setAssetScript(
        issuedAssetId,
        smartAcc.stringRepr,
        issueFee+smartFee,
        Some(assetScript)
      ),
      "Script is too complex: 33280 > 4000"
    )
  }

  test("can't set contract to account after estimator v1 precheck activation") {
    assertBadRequestAndMessage(sender.setScript(smartAcc.stringRepr, Some(accScript), setScriptFee + smartFee), "Contract function (default) is too complex")
  }

  test("can still invoke account and asset scripts after estimator v1 precheck activation") {
    sender
      .invokeScript(
        callerAcc.stringRepr,
        smartAcc.stringRepr,
        Some("default"),
        List.empty,
        Seq.empty,
        smartMinFee,
        None,
        waitForTx = true
      )
      ._1
      .id

    sender.transfer(smartAcc.stringRepr, callerAcc.stringRepr, 1, minFee + 2 * smartFee, Some(issuedAssetId), None, waitForTx = true)
  }

}
