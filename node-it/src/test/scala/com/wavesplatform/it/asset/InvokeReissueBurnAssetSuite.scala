package com.wavesplatform.it.asset

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.BaseSuite
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{Transaction}
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_BYTESTR, CONST_LONG}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class InvokeReissueBurnAssetSuite extends BaseSuite {

  def script(asset: Map[String, Any]): String = {
    val s  = if (asset("compiledScript").equals("")) "unit" else asset("compiledScript")
    val d  = asset("decimals")
    val ds = asset("description")
    val r  = asset("isReissuable")
    val n  = asset("name")
    val q  = asset("quantity")
    val ns = asset("nounce")

    s"""
       |{-# STDLIB_VERSION 4 #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |{-# CONTENT_TYPE DAPP #-}
       |
       |@Callable (i) func issueAsset() = [Issue(${if (s.equals(None)) "unit" else s}, $d, "$ds", $r, "$n", $q, $ns)]
       |
       |@Callable (i) func burnAsset(a: ByteVector, q: Int) = [Burn(a, q)]
       |
       |@Callable (i) func reissueAsset(a: ByteVector, r: Boolean, q: Int) = [Reissue(a, r, q)]
       |
       """.stripMargin
  }

  "Creation Issue/Reissue/Burn transactions in @Callable" - {

    "simple asset" - {

      val simpleAsset = Map(
        "type"           -> "Simple",
        "compiledScript" -> None,
        "name"           -> "SimpleAsset",
        "description"    -> "description",
        "quantity"       -> 100500,
        "isReissuable"   -> false,
        "decimals"       -> 8,
        "nounce"         -> 0
      )
      val simpleReissuableAsset = Map(
        "type"           -> "Reissuable",
        "compiledScript" -> None,
        "name"           -> "ReissuableAsset",
        "description"    -> "description",
        "quantity"       -> 100000000,
        "isReissuable"   -> true,
        "decimals"       -> 3,
        "nounce"         -> 0
      )
      val nftAsset = Map(
        "type"           -> "NFT",
        "compiledScript" -> None,
        "name"           -> "NFTAsset",
        "description"    -> "description",
        "quantity"       -> 1,
        "isReissuable"   -> false,
        "decimals"       -> 0,
        "nounce"         -> 0
      )

      val nftReissuableAsset = Map(
        "type"           -> "NFT Reissuable",
        "compiledScript" -> None,
        "name"           -> "NFTReissuableAsset",
        "description"    -> "description",
        "quantity"       -> 1,
        "isReissuable"   -> true,
        "decimals"       -> 0,
        "nounce"         -> 0
      )

      for (data <- Seq(simpleAsset, simpleReissuableAsset, nftAsset)) s"${data("type")} asset could be issued in callable" in {
        val acc = createDapp(script(data))
        val tx  = invokeScript(acc, "issueAsset")

        validateAssetIssued(acc, tx, data)
      }

      for (data <- Seq(simpleAsset, simpleReissuableAsset)) s"${data("type")} asset could be partially burned" in {
        val acc            = createDapp(script(data))
        val txIssue        = invokeScript(acc, "issueAsset")
        val assetId        = validateAssetIssued(acc, txIssue, data)
        val burnQuantity   = 1000
        val remainQuantity = data("quantity").asInstanceOf[Number].longValue - burnQuantity

        invokeScript(acc, "burnAsset", assetId = assetId, count = burnQuantity)

        sender.assetsDetails(assetId).quantity shouldBe remainQuantity
        sender.assertAssetBalance(acc, assetId, remainQuantity)
      }

      for (data <- Seq(simpleAsset, simpleReissuableAsset, nftAsset)) s"${data("type")} could be fully burned" in {
        val acc     = createDapp(script(data))
        val txIssue = invokeScript(acc, "issueAsset")
        val assetId = validateAssetIssued(acc, txIssue, data)

        invokeScript(acc, "burnAsset", assetId = assetId, count = data("quantity").asInstanceOf[Number].intValue)

        sender.assetsDetails(assetId).quantity shouldBe 0
        sender.assertAssetBalance(acc, assetId, 0)
      }

      for (data <- Seq(simpleReissuableAsset)) s"${data("type")} could be reissued" in {
        val acc               = createDapp(script(data))
        val txIssue           = invokeScript(acc, "issueAsset")
        val assetId           = validateAssetIssued(acc, txIssue, data)
        val initialQuantity   = data("quantity").asInstanceOf[Number].longValue
        val addedQuantity     = 100500
        val initialReissuable = data("isReissuable").asInstanceOf[Boolean].booleanValue

        invokeScript(acc, "reissueAsset", count = addedQuantity, isReissuable = !initialReissuable)

        sender.assetsDetails(assetId).reissuable shouldBe !initialReissuable
        sender.assetsDetails(assetId).quantity shouldBe initialQuantity + addedQuantity
        sender.assertAssetBalance(acc, assetId, initialQuantity + addedQuantity)
      }
    }
  }

  def createDapp(scriptParts: String*): String = {
    val script  = scriptParts.mkString(" ")
    val address = miner.createAddress()
    val compiledScript = ScriptCompiler
      .compile(
        script,
        ScriptEstimatorV2
      )
      .explicitGet()
      ._1

    miner.transfer(sender.address, address, 10.waves, minFee, waitForTx = true)

    nodes.waitForHeightAriseAndTxPresent(
      miner
        .signedBroadcast(
          SetScriptTransaction
            .selfSigned(1.toByte, KeyPair(Base58.decode(miner.seed(address))), Some(compiledScript), setScriptFee, System.currentTimeMillis())
            .explicitGet()
            .json
            .value
        )
        .id
    )

    address
  }

  def invokeScript(
      address: String,
      function: String,
      wait: Boolean = true,
      assetId: String = "",
      count: Int = 1,
      isReissuable: Boolean = true
  ): Transaction = {
    val args = function match {
      case "issueAsset"   => List.empty
      case "burnAsset"    => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet(), CONST_LONG(count))
      case "reissueAsset" => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet(), CONST_BOOLEAN(isReissuable), CONST_LONG(count))
    }

    val tx = miner
      .invokeScript(
        address,
        address,
        fee = smartMinFee + smartFee,
        waitForTx = wait,
        func = Some(function),
        args = args
      )

    if (wait) nodes.waitForHeightAriseAndTxPresent(tx._1.id)
    tx._1
  }

  def validateAssetIssued(account: String, tx: Transaction, data: Map[String, Any]): String = {
    val asset = sender.debugStateChanges(tx.id.toString).stateChanges.get.issues.head
    val assetInfo = sender.assetsDetails(asset.assetId)

    assetInfo.originTransactionId shouldBe tx.id
    assetInfo.issueTimestamp shouldBe tx.timestamp
    assetInfo.issuer shouldBe tx.sender.get
    assetInfo.name shouldBe data("name")
    assetInfo.description shouldBe data("description")
    assetInfo.reissuable shouldBe data("isReissuable")
    assetInfo.decimals shouldBe data("decimals")
    assetInfo.quantity shouldBe data("quantity")
    assetInfo.scriptDetails shouldBe data("compiledScript")

    sender.assertAssetBalance(account, asset.assetId, data("quantity").asInstanceOf[Number].longValue)

    asset.assetId
  }
}
