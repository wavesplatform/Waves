package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.Transaction
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BYTESTR
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.{CancelAfterFailure, Matchers, OptionValues}

class InvokeScriptAssetIssueSuite extends BaseTransactionSuite with Matchers with CancelAfterFailure with OptionValues {
  import InvokeScriptAssetIssueSuite._

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.MultiPaymentInvokeScript.id, 0), (BlockchainFeatures.BlockV5.id, 0)))
      .buildNonConflicting()

  private val smartAcc  = pkByAddress(firstAddress)
  private val callerAcc = pkByAddress(secondAddress)

  private var issueTx: Transaction        = _
  private var invokeScriptTx: Transaction = _

  private var issueTxAssetId: String      = _
  private var invokeScriptAssetId: String = _

  test("Correct data for assets issued by transaction") {
    issueTx = sender.issue(callerAcc.stringRepr, "TxAsset", "TxDesc", 100, 0, fee = issueFee + smartFee, version = TxVersion.V3, waitForTx = true)
    issueTxAssetId = issueTx.id

    val assetInfo = sender.assetsDetails(issueTx.id)
    assetInfo.assetId shouldBe issueTx.id
    assetInfo.originTransactionId shouldBe issueTx.id
    assetInfo.issueTimestamp shouldBe issueTx.timestamp
    assetInfo.issuer shouldBe issueTx.sender.get
    assetInfo.name shouldBe "TxAsset"
    assetInfo.description shouldBe "TxDesc"
    assetInfo.reissuable shouldBe true
    assetInfo.decimals shouldBe 0
    assetInfo.quantity shouldBe 100

    val balance = sender.assetsBalance(secondAddress).balances.find(_.assetId == issueTxAssetId)
    balance shouldBe 'defined
    balance.value.issueTransaction.value.id shouldBe issueTx.id
  }

  test("Correct data for assets issued by script") {

    sender.setScript(
      smartAcc.stringRepr,
      Some(ScriptCompiler.compile(dAppV4, ScriptEstimatorV3).explicitGet()._1.bytes().base64),
      fee = setScriptFee + smartFee,
      waitForTx = true
    )
    invokeScriptTx = sender
      .invokeScript(
        callerAcc.stringRepr,
        smartAcc.stringRepr,
        Some("i"),
        args = List.empty,
        waitForTx = true
      )
      ._1

    val stateChanges = sender.debugStateChanges(invokeScriptTx.id).stateChanges
    stateChanges shouldBe 'defined
    stateChanges.value.issues.size shouldBe 1

    invokeScriptAssetId = stateChanges.value.issues.head.assetId

    val assetInfo = sender.assetsDetails(invokeScriptAssetId)
    assetInfo.assetId shouldBe invokeScriptAssetId
    assetInfo.originTransactionId shouldBe invokeScriptTx.id
    assetInfo.issueTimestamp shouldBe invokeScriptTx.timestamp
    assetInfo.issuer shouldBe smartAcc.stringRepr
    assetInfo.name shouldBe "InvokeAsset"
    assetInfo.description shouldBe "InvokeDesc"
    assetInfo.reissuable shouldBe true
    assetInfo.decimals shouldBe 0
    assetInfo.quantity shouldBe 100L

    stateChanges.value.issues.head.name shouldBe "InvokeAsset"
    stateChanges.value.issues.head.description shouldBe "InvokeDesc"
    stateChanges.value.issues.head.quantity shouldBe 100L
    stateChanges.value.issues.head.decimals shouldBe 0
    stateChanges.value.issues.head.isReissuable shouldBe true
    stateChanges.value.issues.head.compiledScript shouldBe None
    stateChanges.value.issues.head.nonce shouldBe 0

    val balance = sender.assetsBalance(firstAddress).balances.find(_.assetId == invokeScriptAssetId)
    balance shouldBe 'defined
    balance.value.issueTransaction shouldBe None
  }

  test("Correct data for assets reissued by transaction") {
    sender.reissue(callerAcc.stringRepr, issueTxAssetId, 100L, reissuable = true, waitForTx = true)

    val assetInfo = sender.assetsDetails(issueTxAssetId)
    assetInfo.assetId shouldBe issueTxAssetId
    assetInfo.originTransactionId shouldBe issueTx.id
    assetInfo.issueTimestamp shouldBe issueTx.timestamp
    assetInfo.issuer shouldBe issueTx.sender.get
    assetInfo.name shouldBe "TxAsset"
    assetInfo.description shouldBe "TxDesc"
    assetInfo.reissuable shouldBe true
    assetInfo.decimals shouldBe 0
    assetInfo.quantity shouldBe 200L
  }

  test("Correct data for assets reissued by script") {
    val txId = sender
      .invokeScript(
        callerAcc.stringRepr,
        smartAcc.stringRepr,
        Some("r"),
        args = List(CONST_BYTESTR(ByteStr.decodeBase58(invokeScriptAssetId).get).explicitGet()),
        waitForTx = true
      )
      ._1
      .id

    val assetInfo = sender.assetsDetails(invokeScriptAssetId)
    assetInfo.assetId shouldBe invokeScriptAssetId
    assetInfo.originTransactionId shouldBe invokeScriptTx.id
    assetInfo.issueTimestamp shouldBe invokeScriptTx.timestamp
    assetInfo.issuer shouldBe smartAcc.stringRepr
    assetInfo.name shouldBe "InvokeAsset"
    assetInfo.description shouldBe "InvokeDesc"
    assetInfo.reissuable shouldBe true
    assetInfo.decimals shouldBe 0
    assetInfo.quantity shouldBe 200L

    val stateChanges = sender.debugStateChanges(txId).stateChanges
    stateChanges.value.reissues.size shouldBe 1
    stateChanges.value.reissues.head.assetId shouldBe invokeScriptAssetId
    stateChanges.value.reissues.head.quantity shouldBe 100L
  }

  test("Correct data for assets burnt by transaction") {
    sender.burn(callerAcc.stringRepr, issueTxAssetId, 100L, waitForTx = true)

    val assetInfo = sender.assetsDetails(issueTxAssetId)
    assetInfo.assetId shouldBe issueTx.id
    assetInfo.originTransactionId shouldBe issueTx.id
    assetInfo.issueTimestamp shouldBe issueTx.timestamp
    assetInfo.issuer shouldBe issueTx.sender.get
    assetInfo.name shouldBe "TxAsset"
    assetInfo.description shouldBe "TxDesc"
    assetInfo.reissuable shouldBe true
    assetInfo.decimals shouldBe 0
    assetInfo.quantity shouldBe 100L
  }

  test("Correct data for assets burnt by script") {
    val txId = sender
      .invokeScript(
        callerAcc.stringRepr,
        smartAcc.stringRepr,
        Some("b"),
        args = List(CONST_BYTESTR(ByteStr.decodeBase58(invokeScriptAssetId).get).explicitGet()),
        waitForTx = true
      )
      ._1
      .id

    val assetInfo = sender.assetsDetails(invokeScriptAssetId)
    assetInfo.assetId shouldBe invokeScriptAssetId
    assetInfo.originTransactionId shouldBe invokeScriptTx.id
    assetInfo.issueTimestamp shouldBe invokeScriptTx.timestamp
    assetInfo.issuer shouldBe smartAcc.stringRepr
    assetInfo.name shouldBe "InvokeAsset"
    assetInfo.description shouldBe "InvokeDesc"
    assetInfo.reissuable shouldBe true
    assetInfo.decimals shouldBe 0
    assetInfo.quantity shouldBe 100L

    val stateChanges = sender.debugStateChanges(txId).stateChanges
    stateChanges.value.burns.size shouldBe 1
    stateChanges.value.burns.head.assetId shouldBe invokeScriptAssetId
    stateChanges.value.burns.head.quantity shouldBe 100L
  }

  test("correct data for NFT issued by transaction") {
    val nftIssueTx        = sender.issue(callerAcc.stringRepr, "TxNft", "TxNftDesc", 1, 0, fee = issueFee + smartFee, reissuable = false, waitForTx = true)
    val nftIssueTxAssetId = nftIssueTx.id

    val info = sender.nftAssetsBalance(secondAddress, 100)
    info.size shouldBe 1
    info.head.assetId shouldBe nftIssueTxAssetId

    val afterInfo = sender.nftAssetsBalance(secondAddress, 100, Some(nftIssueTxAssetId))
    afterInfo.size shouldBe 0
  }

  test("correct data for NFT issued by script") {
    val nftInvokeScriptTx = sender
      .invokeScript(
        callerAcc.stringRepr,
        smartAcc.stringRepr,
        Some("n"),
        args = List.empty,
        waitForTx = true
      )
      ._1

    val stateChanges = sender.debugStateChanges(nftInvokeScriptTx.id).stateChanges
    stateChanges shouldBe 'defined
    stateChanges.value.issues.size shouldBe 1

    val nftInvokeScriptAssetId = stateChanges.value.issues.head.assetId

    val info = sender.nftAssetsBalance(firstAddress, 100)
    info.size shouldBe 1
    info.head.assetId shouldBe nftInvokeScriptAssetId

    val afterInfo = sender.nftAssetsBalance(firstAddress, 100, Some(nftInvokeScriptAssetId))
    afterInfo.size shouldBe 0
  }
}

object InvokeScriptAssetIssueSuite {
  private val dAppV4: String =
    """{-# STDLIB_VERSION 4 #-}
      |{-# CONTENT_TYPE DAPP #-}
      |
      |@Callable(i)
      |func i() = [Issue(unit, 0, "InvokeDesc", true, "InvokeAsset", 100, 0)]
      |
      |@Callable(i)
      |func r(id: ByteVector) = [Reissue(id, true, 100)]
      |
      |@Callable(i)
      |func b(id: ByteVector) = [Burn(id, 100)]
      |
      |@Callable(i)
      |func n() = [Issue(unit, 0, "NFT", false, "_NFT", 1, 0)]
      |
      |""".stripMargin
}
