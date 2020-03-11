package com.wavesplatform.it.asset

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.BaseSuite
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.Transaction
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_BYTESTR, CONST_LONG}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class InvokeReissueBurnAssetSuite extends BaseSuite {
  val initialWavesBalance = 100.waves
  val setScriptPrice      = 0.01.waves

  def script(asset: Map[String, Any], function: String = ""): String = {
    val s           = if (asset("compiledScript").equals("")) "unit" else asset("compiledScript")
    val d           = asset("decimals")
    val ds          = asset("description")
    val r           = asset("isReissuable")
    val n           = asset("name")
    val q           = asset("quantity")
    val ns          = asset("nounce")
    val issueParams = s""""$n","$ds",$q, $d,$r,${if (s.equals(None)) "unit" else s}"""

    s"""
       |{-# STDLIB_VERSION 4 #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |{-# CONTENT_TYPE DAPP #-}
       |
       |@Callable (i)
       |func issue10Assets() = {
       |  [
       |    Issue($issueParams, 0),
       |    Issue($issueParams, 1),
       |    Issue($issueParams, 2),
       |    Issue($issueParams, 3),
       |    Issue($issueParams, 4),
       |    Issue($issueParams, 5),
       |    Issue($issueParams, 6),
       |    Issue($issueParams, 7),
       |    Issue($issueParams, 8),
       |    Issue($issueParams, 9)
       |  ]
       |}
       |
       |@Callable (i)
       |func issue11Assets() = {
       |  [
       |    Issue($issueParams, 1),
       |    Issue($issueParams, 2),
       |    Issue($issueParams, 3),
       |    Issue($issueParams, 4),
       |    Issue($issueParams, 5),
       |    Issue($issueParams, 6),
       |    Issue($issueParams, 7),
       |    Issue($issueParams, 8),
       |    Issue($issueParams, 9),
       |    Issue($issueParams, 10),
       |    Issue($issueParams, 11)
       |  ]
       |}
       |
       |@Callable (i) func issueAsset() = [Issue($issueParams, $ns)]
       |
       |@Callable (i) func burnAsset(a: ByteVector, q: Int) = [Burn(a, q)]
       |
       |@Callable (i) func reissueAsset(a: ByteVector, r: Boolean, q: Int) = [Reissue(a, r, q)]
       |
       |$function
       |
       """.stripMargin
  }

  val simpleNonreissuableAsset = Map(
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

  for (method <- Seq("@Callable", "Transaction")) s"Asset Issue/Reissue/Burn via $method" - {

    for (data <- Seq(simpleNonreissuableAsset, simpleReissuableAsset, nftAsset)) s"${data("type")} asset could be issued in callable" in {
      val acc = createDapp(script(data))

      val fee = invocationCost(if (isNft(data)) 0 else 1)
      val tx  = issue(acc, method, data, fee)

      validateIssuedAssets(acc, tx, data, method = method)
      sender.balanceDetails(acc).regular shouldBe (initialWavesBalance - setScriptPrice - fee)
    }

    for (data <- Seq(simpleNonreissuableAsset, simpleReissuableAsset)) s"${data("type")} asset could be partially burned" in {
      val acc            = createDapp(script(data))
      val fee            = invocationCost(if (isNft(data)) 0 else 1)
      val txIssue        = issue(acc, method, data, fee)
      val assetId        = validateIssuedAssets(acc, txIssue, data, method = method)
      val burnQuantity   = 1000
      val remainQuantity = data("quantity").asInstanceOf[Number].longValue - burnQuantity

      burn(acc, method, assetId, burnQuantity)

      sender.assetsDetails(assetId).quantity shouldBe remainQuantity
      sender.assertAssetBalance(acc, assetId, remainQuantity)
    }

    for (data <- Seq(simpleNonreissuableAsset, simpleReissuableAsset, nftAsset)) s"${data("type")} could be fully burned" in {
      val acc     = createDapp(script(data))
      val fee     = invocationCost(if (isNft(data)) 0 else 1)
      val txIssue = invokeScript(acc, "issueAsset", fee = fee)
      val assetId = validateIssuedAssets(acc, txIssue, data, method = method)

      burn(acc, method, assetId, data("quantity").asInstanceOf[Number].intValue)

      sender.assetsDetails(assetId).quantity shouldBe 0
      sender.assertAssetBalance(acc, assetId, 0)
    }

    "Reissuable asset could be reissued" in {
      val acc               = createDapp(script(simpleReissuableAsset))
      val txIssue           = invokeScript(acc, "issueAsset")
      val assetId           = validateIssuedAssets(acc, txIssue, simpleReissuableAsset, method = method)
      val initialQuantity   = simpleReissuableAsset("quantity").asInstanceOf[Number].longValue
      val addedQuantity     = 100500
      val initialReissuable = simpleReissuableAsset("isReissuable").asInstanceOf[Boolean].booleanValue

      reissue(acc, method, assetId, addedQuantity, !initialReissuable)

      sender.assetsDetails(assetId).reissuable shouldBe !initialReissuable
      sender.assetsDetails(assetId).quantity shouldBe initialQuantity + addedQuantity
      sender.assertAssetBalance(acc, assetId, initialQuantity + addedQuantity)
    }

    "Non-reissuable asset could not be reissued" in {
      val acc     = createDapp(script(simpleNonreissuableAsset))
      val txIssue = invokeScript(acc, "issueAsset")
      val assetId = validateIssuedAssets(acc, txIssue, simpleNonreissuableAsset, method = method)
      assertBadRequestAndMessage(
        reissue(acc, method, assetId, 100500, false),
        "State check failed. Reason: Asset is not reissuable"
      )
    }
  }

  "Restrictions in @Callable" - {

    "Issue 10 assets should not produce an error" in {
      val acc = createDapp(script(simpleNonreissuableAsset))
      val tx  = invokeScript(acc, "issue10Assets")

      for (nth <- 0 to 9) validateIssuedAssets(acc, tx, simpleNonreissuableAsset, nth, "@Callable")
    }

    "Issue more then 10 assets should produce an error" in {
      val acc = createDapp(script(simpleNonreissuableAsset))
      assertBadRequestAndMessage(
        invokeScript(acc, "issue11Assets"),
        "State check failed. Reason: Too many script actions: max: 10, actual: 11"
      )
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

    miner.transfer(sender.address, address, initialWavesBalance, minFee, waitForTx = true)

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
      isReissuable: Boolean = true,
      fee: Long = invokeFee
  ): Transaction = {
    val args = function match {
      case "issueAsset"    => List.empty
      case "issue10Assets" => List.empty
      case "issue11Assets" => List.empty
      case "burnAsset"     => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet(), CONST_LONG(count))
      case "reissueAsset"  => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet(), CONST_BOOLEAN(isReissuable), CONST_LONG(count))
    }

    val tx = miner
      .invokeScript(
        address,
        address,
        fee = fee,
        waitForTx = wait,
        func = Some(function),
        args = args
      )

    if (wait) nodes.waitForHeightAriseAndTxPresent(tx._1.id)
    tx._1
  }

  def validateIssuedAssets(account: String, tx: Transaction, data: Map[String, Any], nth: Int = -1, method: String): String = {
    val assetId = method match {
      case "@Callable" =>
        (if (nth == -1) sender.debugStateChanges(tx.id.toString).stateChanges.get.issues.head
         else sender.debugStateChanges(tx.id.toString).stateChanges.get.issues(nth)).assetId
      case _ => tx.id
    }

    val assetInfo = method match {
      case "@Callable" => sender.assetsDetails(assetId)
      case _           => sender.assetsDetails(tx.id)
    }

    assetInfo.originTransactionId shouldBe tx.id
    assetInfo.issueTimestamp shouldBe tx.timestamp
    assetInfo.issuer shouldBe tx.sender.get
    assetInfo.name shouldBe data("name")
    assetInfo.description shouldBe data("description")
    assetInfo.reissuable shouldBe data("isReissuable")
    assetInfo.decimals shouldBe data("decimals")
    assetInfo.quantity shouldBe data("quantity")
    assetInfo.scriptDetails shouldBe data("compiledScript")

    sender.assertAssetBalance(account, assetId, data("quantity").asInstanceOf[Number].longValue)

    assetId
  }

  def issue(account: String, method: String, data: Map[String, Any], fee: Long = invokeFee): Transaction = {
    method match {
      case "@Callable" => invokeScript(account, "issueAsset", fee = fee)
      case _ => {
        // sender.issue(account,  fee = fee)
        sender.issue(
          account,
          data("name").asInstanceOf[String],
          data("description").asInstanceOf[String],
          data("quantity").asInstanceOf[Number].longValue,
          data("decimals").asInstanceOf[Number].byteValue,
          reissuable = data("isReissuable").asInstanceOf[Boolean],
          fee = fee,
          waitForTx = true
        )
      }
    }
  }

  def reissue(account: String, method: String, assetId: String, quantity: Long, reissuable: Boolean, fee: Long = invokeFee): Unit = {
    method match {
      case "@Callable" => invokeScript(account, "reissueAsset", assetId = assetId, count = quantity.toInt, isReissuable = reissuable)
      case _           => sender.reissue(account, assetId, quantity, reissuable, waitForTx = true)
    }
  }

  def burn(account: String, method: String, assetId: String, quantity: Long, fee: Long = invokeFee): Unit = {
    method match {
      case "@Callable" => invokeScript(account, "burnAsset", assetId = assetId, count = quantity.toInt)
      case _           => sender.burn(account, assetId, quantity, waitForTx = true)
    }
  }

  def isNft(asset: Map[String, Any]): Boolean = {
    asset("quantity").asInstanceOf[Number].longValue == 1 && asset("decimals").asInstanceOf[Number].longValue == 0
  }

  def invocationCost(aCount: Int, isSmartAcc: Boolean = true, sPCount: Int = 0, sAinActions: Int = 0): Long = {
    0.005.waves + (if (isSmartAcc) 0.004.waves else 0L) + 0.004.waves * sPCount + 0.004.waves * sAinActions + 1.waves * aCount
  }
}
