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
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}

case class Asset(t: String, n: String, ds: String, q: Long, r: Boolean, d: Byte, nc: Long)

class IssueReissueBurnAssetSuite extends BaseSuite {
  val initialWavesBalance = 100.waves
  val setScriptPrice      = 0.01.waves

  def script(asset: Asset, function: String = ""): String = {
    val issueParams = s""""${asset.n}","${asset.ds}",${asset.q}, ${asset.d},${asset.r},unit"""

    s"""
       |{-# STDLIB_VERSION 4 #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |{-# CONTENT_TYPE DAPP #-}
       |
       |@Callable (i)
       |func issue2Assets() = {
       |  [
       |    Issue($issueParams, 0),
       |    Issue($issueParams, 0)
       |  ]
       |}
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
       |@Callable (i) func process11actions(a: ByteVector) = {
       |  [
       |    Issue($issueParams, 0),
       |    Reissue(a, true, 1000),
       |    Issue($issueParams, 2),
       |    Issue($issueParams, 3),
       |    Reissue(a, true, 2000),
       |    Reissue(a, true, 2000),
       |    Reissue(a, true, 3000),
       |    Burn(a, 6212),
       |    Reissue(a, true, 2000),
       |    Issue($issueParams, 1),
       |    Burn(a, 12311)
       |  ]
       |}
       |
       |@Callable (i) func issueAsset() = [Issue($issueParams, ${asset.nc})]
       |
       |@Callable (i) func burnAsset(a: ByteVector, q: Int) = [Burn(a, q)]
       |
       |@Callable (i) func reissueAsset(a: ByteVector, r: Boolean, q: Int) = [Reissue(a, r, q)]
       |
       |@Callable (i) func reissueAndReissue(a: ByteVector, rq: Int) = [Reissue(a, true, rq), Reissue(a, false, rq)]
       |
       |@Callable(i)
       |func transferAndBurn(a: ByteVector, q: Int) = {
       |  [
       |    ScriptTransfer(Address(fromBase58String("${miner.address}")), q, a),
       |    Burn(a, q)
       | ]
       |}
       |
       |$function
       |
       """.stripMargin
  }

  val simpleNonreissuableAsset = Asset("Simple", "SimpleAsset", "description", 100500, false, 8, 0)
  val simpleReissuableAsset    = Asset("Reissuable", "ReissuableAsset", "description", 100000000, true, 3, 0)
  val nftAsset                 = Asset("NFT", "NFTAsset", "description", 1, false, 0, 0)

  for (method <- Seq("@Callable", "Transaction")) s"Asset Issue/Reissue/Burn via $method" - {

    for (data <- Seq(simpleNonreissuableAsset, simpleReissuableAsset, nftAsset)) s"${data.t} asset could be issued in callable" in {
      val acc = createDapp(script(data))

      val fee = invocationCost(if (isNft(data)) 0 else 1)
      val tx  = issue(acc, method, data, fee)

      validateIssuedAssets(acc, tx, data, method = method)
      sender.balanceDetails(acc).regular shouldBe (initialWavesBalance - setScriptPrice - fee)
    }

    for (data <- Seq(simpleNonreissuableAsset, simpleReissuableAsset)) s"${data.t} asset could be partially burned" in {
      val acc            = createDapp(script(data))
      val fee            = invocationCost(if (isNft(data)) 0 else 1)
      val txIssue        = issue(acc, method, data, fee)
      val assetId        = validateIssuedAssets(acc, txIssue, data, method = method)
      val burnQuantity   = 1000
      val remainQuantity = data.q - burnQuantity

      burn(acc, method, assetId, burnQuantity)

      sender.assetsDetails(assetId).quantity shouldBe remainQuantity
      sender.assertAssetBalance(acc, assetId, remainQuantity)
    }

    for (data <- Seq(simpleNonreissuableAsset, simpleReissuableAsset, nftAsset)) s"${data.t} could be fully burned" in {
      val acc     = createDapp(script(data))
      val fee     = invocationCost(if (isNft(data)) 0 else 1)
      val txIssue = issue(acc, method, data, fee)
      val assetId = validateIssuedAssets(acc, txIssue, data, method = method)

      burn(acc, method, assetId, data.q)

      sender.assetsDetails(assetId).quantity shouldBe 0
      sender.assertAssetBalance(acc, assetId, 0)
    }

    "Reissuable asset could be reissued" in {
      val acc               = createDapp(script(simpleReissuableAsset))
      val txIssue           = issue(acc, method, simpleReissuableAsset, invocationCost(1))
      val assetId           = validateIssuedAssets(acc, txIssue, simpleReissuableAsset, method = method)
      val initialQuantity   = simpleReissuableAsset.q
      val addedQuantity     = 100500
      val initialReissuable = simpleReissuableAsset.r

      reissue(acc, method, assetId, addedQuantity, !initialReissuable)

      sender.assetsDetails(assetId).reissuable shouldBe !initialReissuable
      sender.assetsDetails(assetId).quantity shouldBe initialQuantity + addedQuantity
      sender.assertAssetBalance(acc, assetId, initialQuantity + addedQuantity)
    }

    "Non-reissuable asset could not be reissued" in {
      val acc     = createDapp(script(simpleNonreissuableAsset))
      val txIssue = issue(acc, method, simpleNonreissuableAsset, invocationCost(1))
      val assetId = validateIssuedAssets(acc, txIssue, simpleNonreissuableAsset, method = method)
      assertBadRequestAndMessage(
        reissue(acc, method, assetId, 100500, false),
        "Asset is not reissuable"
      )
    }
  }

  "Restrictions in @Callable" - {
    val method = "@Callable"

    "Issue two identical assets with the same nonce (one invocation) should produce an error" in {
      val acc = createDapp(script(simpleNonreissuableAsset))
      assertBadRequestAndMessage(
        invokeScript(acc, "issue2Assets"),
        " is already issued"
      )
    }

    "Issue two identical assets with the same nonce (different invocations) should not produce an error" in {
      val acc      = createDapp(script(simpleNonreissuableAsset))
      val txIssue1 = issue(acc, method, simpleNonreissuableAsset, invocationCost(1))
      val txIssue2 = issue(acc, method, simpleNonreissuableAsset, invocationCost(1))
      validateIssuedAssets(acc, txIssue1, simpleNonreissuableAsset, method = method)
      validateIssuedAssets(acc, txIssue2, simpleNonreissuableAsset, method = method)
    }

    "Reissuing more than Long.Max should produce an error" in {
      val acc             = createDapp(script(simpleReissuableAsset))
      val txIssue         = issue(acc, method, simpleReissuableAsset, invocationCost(1))
      val assetId         = validateIssuedAssets(acc, txIssue, simpleReissuableAsset, method = method)
      val initialQuantity = simpleReissuableAsset.q

      assertBadRequestAndMessage(
        reissue(acc, method, assetId, Long.MaxValue - initialQuantity + 1, true),
        "State check failed. Reason: Asset total value overflow"
      )
    }

    "Burning more than current asset count should produce an error" in {
      val acc     = createDapp(script(simpleReissuableAsset))
      val txIssue = issue(acc, method, simpleReissuableAsset, invocationCost(1))
      val assetId = validateIssuedAssets(acc, txIssue, simpleReissuableAsset, method = method)

      assertBadRequestAndMessage(
        invokeScript(acc,"transferAndBurn", assetId = assetId, count = (simpleReissuableAsset.q / 2 + 1).toInt),
        "State check failed. Reason: Accounts balance errors"
      )
    }

    "Reissuing NFT asset should produce an error" in {
      val acc     = createDapp(script(nftAsset))
      val txIssue = issue(acc, method, nftAsset, invocationCost(1))
      val assetId = validateIssuedAssets(acc, txIssue, nftAsset, method = method)

      assertBadRequestAndMessage(
        reissue(acc, method, assetId, 2, true),
        "State check failed. Reason: Asset is not reissuable"
      )
    }

    "Reissuing after setting isReissuiable to falser inside one invocation should produce an error" in {
      val acc     = createDapp(script(simpleReissuableAsset))
      val txIssue = issue(acc, method, simpleReissuableAsset, invocationCost(1))
      val assetId = validateIssuedAssets(acc, txIssue, simpleReissuableAsset, method = method)

      invokeScript(acc, "reissueAndReissue", assetId = assetId, count = 1000)

      sender.assetsDetails(assetId).quantity should be(simpleReissuableAsset.q + 2000)
    }

    "Issue 10 assets should not produce an error" in {
      val acc = createDapp(script(simpleNonreissuableAsset))
      val tx  = invokeScript(acc, "issue10Assets")

      for (nth <- 0 to 9) validateIssuedAssets(acc, tx, simpleNonreissuableAsset, nth, "@Callable")
    }

    "Issue more than 10 assets should produce an error" in {
      val acc = createDapp(script(simpleNonreissuableAsset))
      assertBadRequestAndMessage(
        invokeScript(acc, "issue11Assets"),
        "State check failed. Reason: Too many script actions: max: 10, actual: 11"
      )
    }

    "More than 10 actions Issue/Reissue/Burn should produce an error" in {
      val acc     = createDapp(script(simpleReissuableAsset))
      val txIssue = issue(acc, method, simpleReissuableAsset, invocationCost(1))
      val assetId = validateIssuedAssets(acc, txIssue, simpleReissuableAsset, method = method)

      assertBadRequestAndMessage(
        invokeScript(acc, "process11actions", assetId = assetId),
        "State check failed. Reason: Too many script actions: max: 10, actual: 11"
      )
    }

    "More than 10 issue action in one invocation should produce an error" in {
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

    nodes.waitForTransaction(
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
      fee: Long = invokeFee,
      payments: Seq[InvokeScriptTransaction.Payment] = Seq.empty
  ): Transaction = {
    val args = function match {
      case "issueAsset"        => List.empty
      case "issue2Assets"      => List.empty
      case "issue10Assets"     => List.empty
      case "issue11Assets"     => List.empty
      case "transferAndBurn"     => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet(), CONST_LONG(count))
      case "process11actions"  => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet())
      case "burnAsset"         => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet(), CONST_LONG(count))
      case "reissueAsset"      => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet(), CONST_BOOLEAN(isReissuable), CONST_LONG(count))
      case "reissueAndReissue" => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet(), CONST_LONG(count))
    }

    val tx = miner
      .invokeScript(
        address,
        address,
        fee = fee,
        waitForTx = wait,
        func = Some(function),
        args = args,
        payment = payments
      )

    if (wait) nodes.waitForTransaction(tx._1.id)
    tx._1
  }

  def validateIssuedAssets(account: String, tx: Transaction, data: Asset, nth: Int = -1, method: String): String = {
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
    assetInfo.name shouldBe data.n
    assetInfo.description shouldBe data.ds
    assetInfo.reissuable shouldBe data.r
    assetInfo.decimals shouldBe data.d
    assetInfo.quantity shouldBe data.q
    assetInfo.scriptDetails shouldBe None

    sender.assertAssetBalance(account, assetId, data.q)

    assetId
  }

  def issue(account: String, method: String, data: Asset, fee: Long = invokeFee): Transaction = {
    method match {
      case "@Callable" => invokeScript(account, "issueAsset", fee = fee)
      case _ => {
        sender.issue(
          account,
          data.n,
          data.ds,
          data.q,
          data.d,
          reissuable = data.r,
          fee = fee,
          waitForTx = true
        )
      }
    }
  }

  def reissue(account: String, method: String, assetId: String, quantity: Long, reissuable: Boolean, fee: Long = invokeFee): Unit = {
    method match {
      case "@Callable" => invokeScript(account, "reissueAsset", assetId = assetId, count = quantity.toInt, isReissuable = reissuable)
      case _           => sender.reissue(account, assetId, quantity, reissuable, version = TxVersion.V2, waitForTx = true)
    }
  }

  def burn(account: String, method: String, assetId: String, quantity: Long, fee: Long = invokeFee): Unit = {
    method match {
      case "@Callable" => invokeScript(account, "burnAsset", assetId = assetId, count = quantity.toInt)
      case _           => sender.burn(account, assetId, quantity, fee = invokeFee, version = TxVersion.V2, waitForTx = true)
    }
  }

  def isNft(asset: Asset): Boolean = {
    asset.q == 1 && asset.d == 0
  }

  def invocationCost(aCount: Int, isSmartAcc: Boolean = true, sPCount: Int = 0, sAinActions: Int = 0): Long = {
    0.005.waves + (if (isSmartAcc) 0.004.waves else 0L) + 0.004.waves * sPCount + 0.004.waves * sAinActions + 1.waves * aCount
  }
}
