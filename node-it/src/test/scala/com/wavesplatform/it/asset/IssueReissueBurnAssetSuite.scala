package com.wavesplatform.it.asset

import com.google.protobuf.ByteString
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.grpc.AssetInfoResponse
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.BaseSuite
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{
  AssetInfo,
  BurnInfoResponse,
  DebugStateChanges,
  IssueInfoResponse,
  ReissueInfoResponse,
  StateChangesDetails,
  Transaction
}
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_BYTESTR, CONST_LONG}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}

case class Asset(assetType: String, name: String, description: String, quantity: Long, reissuable: Boolean, decimals: Byte, nonce: Long)

class IssueReissueBurnAssetSuite extends BaseSuite {
  val initialWavesBalance = 100.waves
  val setScriptPrice      = 0.01.waves

  val CallableMethod = "@Callable"

  val simpleNonreissuableAsset = Asset("Simple", "SimpleAsset", "description", 100500, false, 8, 0)
  val simpleReissuableAsset    = Asset("Reissuable", "ReissuableAsset", "description", 100000000, true, 3, 0)
  val nftAsset                 = Asset("NFT", "NFTAsset", "description", 1, false, 0, 0)

  for (method <- Seq(CallableMethod, "Transaction")) s"Asset Issue/Reissue/Burn via $method" - {
    val isCallable = method == CallableMethod

    for (data <- Seq(simpleNonreissuableAsset, simpleReissuableAsset, nftAsset)) s"${data.assetType} asset could be issued in callable" in {
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

      burn(acc, method, assetId, burnQuantity)
      sender.assetsDetails(assetId).quantity shouldBe remainQuantity
      sender.assertAssetBalance(acc, assetId, remainQuantity)
    }

    for (data <- Seq(simpleNonreissuableAsset, simpleReissuableAsset, nftAsset)) s"${data.assetType} could be fully burned" in {
      val acc     = createDapp(script(data))
      val fee     = invocationCost(if (isNft(data)) 0 else 1)
      val txIssue = issue(acc, method, data, fee)
      val assetId = validateIssuedAssets(acc, txIssue, data, method = method)

      val tx = burn(acc, method, assetId, data.quantity)

      sender.assetsDetails(assetId).quantity shouldBe 0
      sender.assertAssetBalance(acc, assetId, 0)

      if (isCallable) assertStateChanges(tx) { sd =>
        sd.burns should matchPattern {
          case Seq(BurnInfoResponse(`assetId`, data.quantity)) =>
        }
      }
    }

    "Reissuable asset could be reissued" in {
      val acc               = createDapp(script(simpleReissuableAsset))
      val txIssue           = issue(acc, method, simpleReissuableAsset, invocationCost(1))
      val assetId           = validateIssuedAssets(acc, txIssue, simpleReissuableAsset, method = method)
      val initialQuantity   = simpleReissuableAsset.quantity
      val addedQuantity     = 100500
      val initialReissuable = simpleReissuableAsset.reissuable

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
        reissue(acc, method, assetId, 100500, reissuable = false),
        "Asset is not reissuable"
      )
    }
  }

  "Restrictions in " + CallableMethod - {
    val method = CallableMethod

    "Issue two identical assets with the same nonce (one invocation) should produce an error" ignore {
      /* SC-575  */
      val acc = createDapp(script(simpleNonreissuableAsset))
      assertBadRequestAndMessage(
        invokeScript(acc, "issue2Assets"),
        "State check failed. Reason: Reason should be here"
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
      val initialQuantity = simpleReissuableAsset.quantity

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
        invokeScript(acc, "transferAndBurn", assetId = assetId, count = (simpleReissuableAsset.quantity / 2 + 1).toInt),
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

    "Reissuing after setting isReissuiable to falser inside one invocation should produce an error" ignore /* SC-580 */ {
      val acc     = createDapp(script(simpleReissuableAsset))
      val txIssue = issue(acc, method, simpleReissuableAsset, invocationCost(1))
      val assetId = validateIssuedAssets(acc, txIssue, simpleReissuableAsset, method = method)

      invokeScript(acc, "reissueAndReissue", assetId = assetId, count = 1000)

      sender.assetsDetails(assetId).quantity should be(simpleReissuableAsset.quantity + 1000)
    }

    "Issue 10 assets should not produce an error" in {
      val acc = createDapp(script(simpleNonreissuableAsset))
      val tx  = invokeScript(acc, "issue10Assets")
      for (nth <- 0 to 9) {
        val assetId = validateIssuedAssets(acc, tx, simpleNonreissuableAsset, nth, CallableMethod)
        assertQuantity(assetId)(simpleReissuableAsset.quantity)
        sender.assertAssetBalance(acc, assetId, simpleReissuableAsset.quantity)
      }
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

  "State changes" - {
    "No issue" in {
      val script =
        """
          |{-# STDLIB_VERSION 4 #-}
          |{-# SCRIPT_TYPE ACCOUNT #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |""" + CallableMethod + """ (i)
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
      val asset = Asset("test", "name", "description", 100000000, true, 8, 1)
      val acc   = createDapp(script(asset))

      val issue1   = issue(acc, CallableMethod, asset)
      val asset1Id = validateIssuedAssets(acc, issue1, asset)

      reissue(acc, CallableMethod, asset1Id, 100000000, true)
      burn(acc, CallableMethod, asset1Id, 500000000)
      assertQuantity(asset1Id)(150000000)
      sender.assertAssetBalance(acc, asset1Id, 150000000)

      val issue2   = issue(acc, CallableMethod, asset)
      val asset2Id = validateIssuedAssets(acc, issue2, asset)
      burn(acc, CallableMethod, asset2Id, 500000000)
      reissue(acc, CallableMethod, asset2Id, 100000000, false)
      assertQuantity(asset2Id)(150000000, false)
      sender.assertAssetBalance(acc, asset2Id, 150000000)
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
        acc           -> (simpleReissuableAsset.quantity)
      )
    }

    "rollback works" in {
      val acc    = createDapp(script(simpleReissuableAsset))
      val asset  = issueValidated(acc, simpleReissuableAsset)
      val height = nodes.waitForHeightArise()
      invokeScript(acc, "reissueIssueAndNft", assetId = asset)
      nodes.waitForHeightArise()
      nodes.rollback(height, returnToUTX = false)
      sender.debugStateChangesByAddress(acc, 100) should matchPattern {
        case Seq(sc: DebugStateChanges) if sc.stateChanges.forall(sd => sd.issues.nonEmpty && sd.reissues.isEmpty && sd.burns.isEmpty) =>
      }
      assertQuantity(asset)(simpleReissuableAsset.quantity)
      sender.assertAssetBalance(acc, asset, simpleReissuableAsset.quantity)
      sender.assetsBalance(acc).balances.map(_.assetId) shouldBe Seq(asset)
      sender.nftList(acc, 10) shouldBe empty
    }

    "liquid block works" in {
      val acc   = createDapp(script(simpleReissuableAsset))
      val asset = issueValidated(acc, simpleReissuableAsset)
      val tx    = invokeScript(acc, "reissueIssueAndNft", assetId = asset)
      assertStateChanges(tx) { sd =>
        sd.issues should have size 1
        sd.burns should have size 1
        sd.reissues should have size 1
      }
      assertQuantity(asset)(simpleReissuableAsset.quantity)
      sender.assertAssetBalance(acc, asset, simpleReissuableAsset.quantity)
      sender.assetsBalance(acc).balances.map(_.assetId) shouldBe Seq(asset)
      sender.nftList(acc, 10) should have size 1
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
      case "issueAsset"         => List.empty
      case "issue2Assets"       => List.empty
      case "issue10Assets"      => List.empty
      case "issue11Assets"      => List.empty
      case "transferAndBurn"    => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet(), CONST_LONG(count))
      case "reissueIssueAndNft" => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet())
      case "process11actions"   => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet())
      case "burnAsset"          => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet(), CONST_LONG(count))
      case "reissueAsset"       => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet(), CONST_BOOLEAN(isReissuable), CONST_LONG(count))
      case "reissueAndReissue"  => List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet(), CONST_LONG(count))
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
    f(grpcStateChanges(tx))
  }

  def stateChanges(tx: Transaction): StateChangesDetails =
    sender.debugStateChanges(tx.id).stateChanges.get

  def stateChangesStrings(tx: Transaction): StateChangesDetails =
    sender.debugStateChanges(tx.id, amountsAsStrings = true).stateChanges.get

  def grpcStateChanges(tx: Transaction): StateChangesDetails = {
    import com.wavesplatform.it.api.SyncGrpcApi._
    sender.stateChanges(tx.id)
  }

  def grpcBalance(address: String, assetId: String): Long = {
    import com.wavesplatform.it.api.SyncGrpcApi._
    sender.grpc.assetsBalance(ByteString.copyFrom(Base58.decode(address)), Seq(assetId)).headOption.fold(0L)(_._2)
  }

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

    assertGrpcAssetDetails(assetId) { ai =>
      ai.totalVolume shouldBe quantity
      ai.reissuable shouldBe reissuable
    }
  }

  def assertAssetDetails(assetId: String)(f: AssetInfo => Unit): Unit = {
    val assetInfo = sender.assetsDetails(assetId)
    f(assetInfo)
  }

  def assertGrpcAssetDetails(assetId: String)(f: AssetInfoResponse => Unit): Unit = {
    import com.wavesplatform.it.api.SyncGrpcApi._
    val assetInfo = sender.grpc.assetInfo(assetId)
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
    grpcBalance(account, assetId) shouldBe data.quantity

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

      case _ => {
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
  }

  def reissue(account: String, method: String, assetId: String, quantity: Long, reissuable: Boolean, fee: Long = invokeFee): Transaction = {
    method match {
      case CallableMethod =>
        val tx = invokeScript(account, "reissueAsset", assetId = assetId, count = quantity.toInt, isReissuable = reissuable)
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
        val tx = invokeScript(account, "burnAsset", assetId = assetId, count = quantity.toInt)
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
       |    Issue($issueParams, ${asset.nonce + 1})
       |    Reissue(a, 100),
       |    Burn(a, 100),
       |    Issue(${createIssueParams(nftAsset)}, 1)
       |  ]
       |}
       |
       |$function
       |
       """.stripMargin
  }

}
