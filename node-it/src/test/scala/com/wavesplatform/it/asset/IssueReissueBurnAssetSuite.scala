package com.wavesplatform.it.asset

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.BaseSuite
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{AssetInfo, BurnInfoResponse, IssueInfoResponse, ReissueInfoResponse, StateChangesDetails, Transaction}
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_BYTESTR, CONST_LONG}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}

import scala.concurrent.duration._

class IssueReissueBurnAssetSuite extends BaseSuite {
  private val initialWavesBalance = 100.waves
  private val setScriptPrice      = 0.01.waves

  private val CallableMethod    = "@Callable"
  private val TransactionMethod = "Transaction"

  private val simpleNonreissuableAsset = Asset("Simple", "SimpleAsset", "description", 100500, reissuable = false, 8, 0)
  private val simpleReissuableAsset    = Asset("Reissuable", "ReissuableAsset", "description", 100000000, reissuable = true, 3, 0)
  private val longMaxAsset             = Asset("Long max", "name", "", Long.MaxValue, reissuable = true, 8, Long.MaxValue)
  private val longMinAsset             = Asset("Long min", "name" * 4, "A" * 1000, Long.MaxValue, reissuable = true, 0, Long.MinValue)
  private val nftAsset                 = Asset("NFT", "NFTAsset", "description", 1, reissuable = false, 0, 0)

  for (method <- Seq(CallableMethod, TransactionMethod)) s"Asset Issue/Reissue/Burn via $method" - {
    val isCallable = method == CallableMethod

    for (data <- Seq(simpleNonreissuableAsset, simpleReissuableAsset, nftAsset, longMaxAsset, longMinAsset))
      s"${data.assetType} asset could be issued in callable" in {
        val acc = createDapp(script(data))

        val fee = invocationCost(if (isNft(data)) 0 else 1)
        val tx  = issue(acc, method, data, fee)

        validateIssuedAssets(acc, tx, data, method = method)
        sender.balanceDetails(acc).regular shouldBe (initialWavesBalance - setScriptPrice - fee)
      }

    for (data <- Seq(simpleNonreissuableAsset, simpleReissuableAsset)) s"${data.assetType} asset could be partially burned" in {
      val acc            = createDapp(script(data))
      val fee            = invocationCost(if (isNft(data)) 0 else 1)
      val txIssue        = issue(acc, method, data, fee)
      val assetId        = validateIssuedAssets(acc, txIssue, data, method = method)
      val burnQuantity   = 1000
      val remainQuantity = data.quantity - burnQuantity

      burn(acc, TransactionMethod, assetId, burnQuantity / 2)
      burn(acc, CallableMethod, assetId, burnQuantity / 2)
      sender.assetsDetails(assetId).quantity shouldBe remainQuantity
      sender.assertAssetBalance(acc, assetId, remainQuantity)
    }

    for (data <- Seq(simpleNonreissuableAsset, simpleReissuableAsset, nftAsset, longMaxAsset)) s"${data.assetType} could be fully burned" in {
      val acc     = createDapp(script(data))
      val fee     = invocationCost(if (isNft(data)) 0 else 1)
      val txIssue = issue(acc, method, data, fee)
      val assetId = validateIssuedAssets(acc, txIssue, data, method = method)

      sender.assertAssetBalance(acc, assetId, data.quantity)
      val tx = burn(acc, method, assetId, data.quantity)

      sender.assetsDetails(assetId).quantity shouldBe 0
      sender.assertAssetBalance(acc, assetId, 0)

      if (isCallable) assertStateChanges(tx) { sd =>
        sd.burns should matchPattern {
          case Seq(BurnInfoResponse(`assetId`, data.quantity)) =>
        }
      }

      if (data.reissuable) {
        reissue(acc, method, assetId, data.quantity, reissuable = true)
        sender.assetsDetails(assetId).quantity shouldBe data.quantity
        sender.assertAssetBalance(acc, assetId, data.quantity)
      }
    }

    "Reissuable asset could be reissued" in {
      val acc               = createDapp(script(simpleReissuableAsset))
      val txIssue           = issue(acc, method, simpleReissuableAsset, invocationCost(1))
      val assetId           = validateIssuedAssets(acc, txIssue, simpleReissuableAsset, method = method)
      val initialQuantity   = simpleReissuableAsset.quantity
      val addedQuantity     = 100500
      val initialReissuable = simpleReissuableAsset.reissuable

      reissue(acc, TransactionMethod, assetId, addedQuantity / 2, reissuable = true)
      reissue(acc, CallableMethod, assetId, addedQuantity / 2, reissuable = false)

      sender.assetsDetails(assetId).reissuable shouldBe !initialReissuable
      sender.assetsDetails(assetId).quantity shouldBe initialQuantity + addedQuantity
      sender.assertAssetBalance(acc, assetId, initialQuantity + addedQuantity)
    }

    "Non-reissuable asset could not be reissued" in {
      val acc     = createDapp(script(simpleNonreissuableAsset))
      val txIssue = issue(acc, method, simpleNonreissuableAsset, invocationCost(1))
      val assetId = validateIssuedAssets(acc, txIssue, simpleNonreissuableAsset, method = method)
      assertError(reissue(acc, method, assetId, 100500, reissuable = false, checkStateChanges = false).id, method, "Asset is not reissuable")
    }
  }

  "Restrictions in " + CallableMethod - {
    val method = CallableMethod

    "Issue two identical assets with the same nonce (one invocation) should produce an error" in {
      val acc = createDapp(script(simpleNonreissuableAsset))
      assertError(invokeScript(acc, "issue2Assets", fee = invocationCost(2)).id, CallableMethod, " is already issued")
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
      val initialQuantity = simpleReissuableAsset.quantity

      assertError(
        reissue(acc, method, assetId, Long.MaxValue - initialQuantity + 1, reissuable = true, checkStateChanges = false).id,
        method,
        "Asset total value overflow"
      )
    }

    "Burning more than current asset count should produce an error" in {
      val acc     = createDapp(script(simpleReissuableAsset))
      val txIssue = issue(acc, method, simpleReissuableAsset, invocationCost(1))
      val assetId = validateIssuedAssets(acc, txIssue, simpleReissuableAsset, method = method)

      val id = invokeScript(acc, "transferAndBurn", assetId = assetId, count = (simpleReissuableAsset.quantity / 2 + 1).toInt, wait = false).id
      sender.waitFor("empty utx")(n => n.utxSize, (n: Int) => n == 0, 100.millis)
      sender.transactionStatus(Seq(id)).head.status shouldBe "not_found"
    }

    "Reissuing NFT asset should produce an error" in {
      val acc     = createDapp(script(nftAsset))
      val txIssue = issue(acc, method, nftAsset, invocationCost(1))
      val assetId = validateIssuedAssets(acc, txIssue, nftAsset, method = method)

      assertError(
        reissue(acc, method, assetId, 2, reissuable = true, checkStateChanges = false).id,
        method,
        "Asset is not reissuable"
      )
    }

    "Reissuing after setting isReissuiable to falser inside one invocation should produce an error" in {
      val acc     = createDapp(script(simpleReissuableAsset))
      val txIssue = issue(acc, method, simpleReissuableAsset, invocationCost(1))
      val assetId = validateIssuedAssets(acc, txIssue, simpleReissuableAsset, method = method)

      assertError(
        invokeScript(acc, "reissueAndReissue", assetId = assetId, count = 1000, fee = invocationCost(1)).id,
        CallableMethod,
        "Asset is not reissuable"
      )

      sender.assetsDetails(assetId).quantity should be(simpleReissuableAsset.quantity)
    }

    "Issue 10 assets should not produce an error" in {
      val acc = createDapp(script(simpleNonreissuableAsset))
      val tx  = invokeScript(acc, "issue10Assets", fee = invocationCost(10))
      for (nth <- 0 to 9) {
        val assetId = validateIssuedAssets(acc, tx, simpleNonreissuableAsset, nth, CallableMethod)
        assertQuantity(assetId)(simpleNonreissuableAsset.quantity, reissuable = false)
        sender.assertAssetBalance(acc, assetId, simpleNonreissuableAsset.quantity)
      }
    }

    "Issue more than 10 assets should produce an error" in {
      val acc = createDapp(script(simpleNonreissuableAsset))
      assertError(
        invokeScript(acc, "issue11Assets").id,
        CallableMethod,
        "Too many script actions: max: 10, actual: 11"
      )
    }

    "More than 10 actions Issue/Reissue/Burn should produce an error" in {
      val acc     = createDapp(script(simpleReissuableAsset))
      val txIssue = issue(acc, method, simpleReissuableAsset, invocationCost(1))
      val assetId = validateIssuedAssets(acc, txIssue, simpleReissuableAsset, method = method)

      assertError(
        invokeScript(acc, "process11actions", assetId = assetId).id,
        CallableMethod,
        "Too many script actions: max: 10, actual: 11"
      )
    }

    "More than 10 issue action in one invocation should produce an error" in {
      val acc = createDapp(script(simpleNonreissuableAsset))
      assertError(
        invokeScript(acc, "issue11Assets").id,
        CallableMethod,
        "Too many script actions: max: 10, actual: 11"
      )
    }
  }

  "State changes" - {
    "No issue" in {
      val script =
        """
          |{-# STDLIB_VERSION 4 #-}
          |{-# SCRIPT_TYPE ACCOUNT #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |@Callable(i)
          |func nooperation() = {
          |  [
          |  ]
          |}""".stripMargin

      val acc = createDapp(script)
      assertStateChanges(invokeScript(acc, "nooperation")) { sc =>
        sc.issues shouldBe empty
        sc.burns shouldBe empty
        sc.reissues shouldBe empty
      }
    }

    "Two assets" in {
      val asset = Asset("test", "name", "description", 10, reissuable = true, 8, 1)
      val acc   = createDapp(script(asset))

      val issue1   = issue(acc, CallableMethod, asset)
      val asset1Id = validateIssuedAssets(acc, issue1, asset)

      reissue(acc, CallableMethod, asset1Id, 10, reissuable = true)
      burn(acc, CallableMethod, asset1Id, 5)
      assertQuantity(asset1Id)(15)
      sender.assertAssetBalance(acc, asset1Id, 15)

      val issue2   = issue(acc, CallableMethod, asset)
      val asset2Id = validateIssuedAssets(acc, issue2, asset)
      burn(acc, CallableMethod, asset2Id, 5)
      reissue(acc, CallableMethod, asset2Id, 10, reissuable = false)
      assertQuantity(asset2Id)(15, reissuable = false)
      sender.assertAssetBalance(acc, asset2Id, 15)
    }

    "NFT burning removes it from list" in {
      val acc     = createDapp(script(nftAsset))
      val txIssue = issue(acc, CallableMethod, nftAsset, invocationCost(1))
      val assetId = validateIssuedAssets(acc, txIssue, nftAsset, method = CallableMethod)
      sender.nftList(acc, 2).map(_.assetId) shouldBe Seq(assetId)
      burn(acc, CallableMethod, assetId, 1)
      sender.nftList(acc, 1) shouldBe empty
    }

    "distribution works" in {
      val acc   = createDapp(script(simpleReissuableAsset))
      val asset = issueValidated(acc, simpleReissuableAsset)
      invokeScript(acc, "transferAndBurn", assetId = asset, count = 100)
      nodes.waitForHeightArise()
      sender.assetDistribution(asset).map { case (a, v) => a.stringRepr -> v } shouldBe Map(
        miner.address -> 100L,
        acc           -> (simpleReissuableAsset.quantity - 200)
      )
      reissue(acc, CallableMethod, asset, 400, reissuable = false)
      invokeScript(acc, "transferAndBurn", assetId = asset, count = 100)
      nodes.waitForHeightArise()
      sender.assetDistribution(asset).map { case (a, v) => a.stringRepr -> v } shouldBe Map(
        miner.address -> 200L,
        acc           -> simpleReissuableAsset.quantity
      )
    }

    "rollback works" in {
      val acc         = createDapp(script(simpleReissuableAsset))
      val asset       = issueValidated(acc, simpleReissuableAsset)
      val simpleAsset = issue(acc, TransactionMethod, simpleReissuableAsset, 1.1.waves).id

      sender.debugStateChangesByAddress(acc, 100).flatMap(_.stateChanges) should matchPattern {
        case Seq(StateChangesDetails(Nil, Nil, Seq(issue), Nil, Nil, Nil, None)) if issue.name == simpleReissuableAsset.name =>
      }

      val height = nodes.waitForHeightArise()
      nodes.waitForHeightArise()
      invokeScript(acc, "reissueIssueAndNft", assetId = asset, fee = invocationCost(1))
      burn(acc, CallableMethod, simpleAsset, 5000)
      burn(acc, TransactionMethod, asset, 10000)
      nodes.waitForHeightArise()

      nodes.rollback(height, returnToUTX = false)
      assertQuantity(asset)(simpleReissuableAsset.quantity)
      sender.assertAssetBalance(acc, asset, simpleReissuableAsset.quantity)
      sender.assertAssetBalance(acc, simpleAsset, simpleReissuableAsset.quantity)
      sender.assetsBalance(acc).balances.map(_.assetId).toSet shouldBe Set(asset, simpleAsset)
      sender.nftList(acc, 10) shouldBe empty
      sender.debugStateChangesByAddress(acc, 100).flatMap(_.stateChanges) should matchPattern {
        case Seq(StateChangesDetails(Nil, Nil, Seq(issue), Nil, Nil, Nil, None)) if issue.name == simpleReissuableAsset.name =>
      }
    }

    "liquid block works" in {
      val acc   = createDapp(script(simpleReissuableAsset))
      val asset = issueValidated(acc, simpleReissuableAsset)
      val tx    = invokeScript(acc, "reissueIssueAndNft", assetId = asset, fee = invocationCost(1))
      def checks(): Unit = {
        assertStateChanges(tx) { sd =>
          sd.issues should have size 2
          sd.burns should have size 1
          sd.reissues should have size 1
        }
        assertQuantity(asset)(simpleReissuableAsset.quantity)
        sender.assertAssetBalance(acc, asset, simpleReissuableAsset.quantity)
        sender.assetsBalance(acc).balances should have size 2
        sender.nftList(acc, 10) should have size 1
      }
      checks()
      nodes.waitForHeightArise()
      checks()
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
      count: Long = 1,
      isReissuable: Boolean = true,
      fee: Long = invokeFee,
      payments: Seq[InvokeScriptTransaction.Payment] = Seq.empty
  ): Transaction = {
    val args = function match {
      case "transferAndBurn"    => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet(), CONST_LONG(count))
      case "reissueIssueAndNft" => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet())
      case "process11actions"   => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet())
      case "burnAsset"          => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet(), CONST_LONG(count))
      case "reissueAsset"       => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet(), CONST_BOOLEAN(isReissuable), CONST_LONG(count))
      case "reissueAndReissue"  => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet(), CONST_LONG(count))
      case _                    => Nil
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

  def assertStateChanges(tx: Transaction)(f: StateChangesDetails => Unit): Unit = {
    f(stateChanges(tx))
    f(stateChangesStrings(tx))

    val result      = sender.debugStateChangesByAddress(tx.sender.get, 100)
    val stateChange = result.find(_.id == tx.id)
    stateChange should not be empty
    f(stateChange.get.stateChanges.get)
  }

  def stateChanges(tx: Transaction): StateChangesDetails =
    sender.debugStateChanges(tx.id).stateChanges.get

  def stateChangesStrings(tx: Transaction): StateChangesDetails =
    sender.debugStateChanges(tx.id, amountsAsStrings = true).stateChanges.get

  def validateIssue(issue: IssueInfoResponse, data: Asset): Unit = {
    issue.name shouldBe data.name
    issue.description shouldBe data.description
    issue.decimals shouldBe data.decimals
    issue.isReissuable shouldBe data.reissuable
    issue.quantity shouldBe data.quantity
  }

  def assertQuantity(assetId: String)(quantity: Long, reissuable: Boolean = true): Unit = {
    assertAssetDetails(assetId) { ai =>
      ai.quantity shouldBe quantity
      ai.reissuable shouldBe reissuable
    }
  }

  def assertAssetDetails(assetId: String)(f: AssetInfo => Unit): Unit = {
    val assetInfo = sender.assetsDetails(assetId)
    f(assetInfo)
  }

  def validateIssuedAssets(account: String, tx: Transaction, data: Asset, nth: Int = -1, method: String = CallableMethod): String = {
    val assetId = method match {
      case CallableMethod => invokeAssetId(tx, nth)
      case _              => tx.id
    }

    val assetInfo = sender.assetsDetails(assetId)

    assetInfo.originTransactionId shouldBe tx.id
    assetInfo.issueTimestamp shouldBe tx.timestamp
    assetInfo.issuer shouldBe tx.sender.get
    assetInfo.name shouldBe data.name
    assetInfo.description shouldBe data.description
    assetInfo.reissuable shouldBe data.reissuable
    assetInfo.decimals shouldBe data.decimals
    assetInfo.quantity shouldBe data.quantity
    assetInfo.scriptDetails shouldBe None

    sender.assertAssetBalance(account, assetId, data.quantity)

    if (method == CallableMethod) assertStateChanges(tx) { sd =>
      val issue = if (nth == -1) sd.issues.head else sd.issues(nth)
      validateIssue(issue, data)
    }

    assetId
  }

  def invokeAssetId(tx: Transaction, nth: Int = -1): String = {
    (if (nth == -1) stateChanges(tx).issues.head
     else stateChanges(tx).issues(nth)).assetId
  }

  def issueValidated(account: String, data: Asset): String = {
    val tx = issue(account, CallableMethod, data)
    validateIssuedAssets(account, tx, data)
  }

  def issue(account: String, method: String, data: Asset, fee: Long = invocationCost(1)): Transaction = {
    method match {
      case CallableMethod =>
        val tx = invokeScript(account, "issueAsset", fee = fee)
        assertStateChanges(tx) { sd =>
          sd.issues match {
            case Seq(issue) => validateIssue(issue, data)
          }
        }
        tx

      case _ =>
        sender.issue(
          account,
          data.name,
          data.description,
          data.quantity,
          data.decimals,
          reissuable = data.reissuable,
          fee = fee,
          waitForTx = true
        )
    }
  }

  def reissue(
      account: String,
      method: String,
      assetId: String,
      quantity: Long,
      reissuable: Boolean,
      fee: Long = invokeFee,
      checkStateChanges: Boolean = true
  ): Transaction = {
    method match {
      case CallableMethod =>
        val tx = invokeScript(account, "reissueAsset", assetId = assetId, count = quantity, isReissuable = reissuable, fee = fee)
        if (checkStateChanges)
          assertStateChanges(tx) { sd =>
            sd.reissues should matchPattern {
              case Seq(ReissueInfoResponse(`assetId`, `reissuable`, `quantity`)) =>
            }
          }
        tx

      case _ => sender.reissue(account, assetId, quantity, reissuable, version = TxVersion.V2, waitForTx = true)
    }
  }

  def burn(account: String, method: String, assetId: String, quantity: Long, fee: Long = invokeFee): Transaction = {
    method match {
      case CallableMethod =>
        val tx = invokeScript(account, "burnAsset", assetId = assetId, count = quantity, fee = fee)
        assertStateChanges(tx) { sd =>
          sd.burns should matchPattern {
            case Seq(BurnInfoResponse(`assetId`, `quantity`)) =>
          }
        }
        tx
      case _ => sender.burn(account, assetId, quantity, fee = invokeFee, version = TxVersion.V2, waitForTx = true)
    }
  }

  def isNft(asset: Asset): Boolean = {
    asset.quantity == 1 && asset.decimals == 0
  }

  def invocationCost(aCount: Int, isSmartAcc: Boolean = true, sPCount: Int = 0, sAinActions: Int = 0): Long = {
    0.005.waves + (if (isSmartAcc) 0.004.waves else 0L) + 0.004.waves * sPCount + 0.004.waves * sAinActions + 1.waves * aCount
  }

  def script(asset: Asset, function: String = ""): String = {
    def createIssueParams(asset: Asset) = s""""${asset.name}","${asset.description}",${asset.quantity}, ${asset.decimals},${asset.reissuable},unit"""
    val issueParams                     = createIssueParams(asset)

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
       |@Callable (i) func issueAsset() = [Issue($issueParams, ${asset.nonce})]
       |
       |@Callable (i) func burnAsset(a: ByteVector, q: Int) = [Burn(a, q)]
       |
       |@Callable (i) func reissueAsset(a: ByteVector, r: Boolean, q: Int) = [Reissue(a, r, q)]
       |
       |@Callable (i) func reissueAndReissue(a: ByteVector, rq: Int) = [Reissue(a, false, rq), Reissue(a, false, rq)]
       |
       |@Callable(i)
       |func transferAndBurn(a: ByteVector, q: Int) = {
       |  [
       |    ScriptTransfer(Address(fromBase58String("${miner.address}")), q, a),
       |    Burn(a, q)
       | ]
       |}
       |
       |@Callable(i)
       |func reissueIssueAndNft(a: ByteVector) = {
       |  [
       |    Issue($issueParams, ${asset.nonce + 1}),
       |    Reissue(a, true, 100),
       |    Burn(a, 100),
       |    Issue(${createIssueParams(nftAsset)}, 1)
       |  ]
       |}
       |
       |$function
       |
       """.stripMargin
  }

  def assertError(f: => String, method: String, msg: String): Unit =
    method match {
      case CallableMethod =>
        val id = f
        sender.debugStateChanges(id).stateChanges.get.errorMessage.get.text should include(msg)
      case TransactionMethod =>
        assertApiError(f) { e =>
          e.message should include(msg)
        }
    }
}
