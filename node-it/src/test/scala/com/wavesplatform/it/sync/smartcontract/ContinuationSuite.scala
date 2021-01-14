package com.wavesplatform.it.sync.smartcontract

import cats.implicits._
import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_BYTESTR}
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.ContinuationTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{CreateAliasTransaction, DataTransaction, TxVersion, smart}
import monix.eval.Coeval
import org.scalatest.OptionValues

class ContinuationSuite extends BaseTransactionSuite with OptionValues {
  private val activationHeight = 8

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 2, Seq.empty)
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.ContinuationTransaction.id, activationHeight)))
      .buildNonConflicting()

  private lazy val dApp: KeyPair         = firstKeyPair
  private lazy val dAppAddress: String   = firstAddress
  private lazy val caller: KeyPair       = secondKeyPair
  private lazy val callerAddress: String = secondAddress
  private lazy val dApp2: KeyPair        = thirdKeyPair
  private lazy val dAppAddress2: String  = thirdAddress

  private val dummyEstimator = new ScriptEstimator {
    override val version: Int = 0
    override def apply(
        declaredVals: Set[String],
        functionCosts: Map[FunctionHeader, Coeval[Long]],
        expr: Terms.EXPR
    ): Either[String, Long] = Right(1)
  }

  private def compile(scriptText: String): String =
    ScriptCompiler.compile(scriptText, dummyEstimator).explicitGet()._1.bytes().base64

  private lazy val dAppScript1 =
    compile(
      s"""
         |{-# STDLIB_VERSION 5 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |
         | func g() =
         |   groth16Verify(
         |     base64'lp7+dPDIOfm77haSFnvr33VwYH/KbIalfOJPRvBLzqlHD8BxunNebMr6Gr6S+u+nh7yLzdqr7HHQNOpZI8mdj/7lR0IBqB9zvRfyTr+guUG22kZo4y2KINDp272xGglKEeTglTxyDUriZJNF/+T6F8w70MR/rV+flvuo6EJ0+HA+A2ZnBbTjOIl9wjisBV+0iISo2JdNY1vPXlpwhlL2fVpW/WlREkF0bKlBadDIbNJBgM4niJGuEZDru3wqrGueETKHPv7hQ8em+p6vQolp7c0iknjXrGnvlpf4QtUtpg3z/D+snWjRPbVqRgKXWtihuIvPFaM6dt7HZEbkeMnXWwSINeYC/j3lqYnce8Jq+XkuF42stVNiooI+TuXECnFdFi9Ib25b9wtyz3H/oKg48He1ftntj5uIRCOBvzkFHGUF6Ty214v3JYvXJjdS4uS2jekplZYoV0aXEnYEOIvfF7d4xay3qkx2NspooM4HeZpiHknIWkUVhGVJBzBDLjLB',
         |     base64'jiGBK+TGHfH8Oadexhdet7ExyIWibSmamWQvffZkyl3WnMoVbTQ3lOks4Mca3sU5qgcaLyQQ1FjFW4g6vtoMapZ43hTGKaWO7bQHsOCvdwHCdwJDulVH16cMTyS9F0BfBJxa88F+JKZc4qMTJjQhspmq755SrKhN9Jf+7uPUhgB4hJTSrmlOkTatgW+/HAf5kZKhv2oRK5p5kS4sU48oqlG1azhMtcHEXDQdcwf9ANel4Z9cb+MQyp2RzI/3hlIx',
         |     base64''
         |   )
         |
         | # complexity = 5.4kk, expecting 1351 steps in at least 6 blocks
         | @Callable(i)
         | func foo(fail: Boolean) = {
         |  let a =
         |    getInteger(Address(base58''), "key") == unit &&
         |    !(${List.fill(2000)("g()").mkString("||")})
         |  if (fail && a)
         |    then
         |      throw("fail")
         |    else
         |      [
         |        BooleanEntry("entry1", a),
         |        Issue("Asset", "Description", 2, 5, true, unit, 0),
         |        Reissue(base58'$scriptedAssetId', 100, true),
         |        Burn(base58'$scriptedAssetId', 100),
         |        ScriptTransfer(i.caller, 10, base58'$scriptedAssetId')
         |      ]
         | }
         |
         | @Callable(i)
         | func performActionWithAsset(assetId: ByteVector) = {
         |    let a = !(${List.fill(380)("g()").mkString("||")})
         |    if (a)
         |      then
         |        [Burn(assetId, 1)]
         |      else
         |        throw("unexpected")
         | }
       """.stripMargin
    )

  private lazy val dAppScript2 =
    compile(
      s"""
         |{-# STDLIB_VERSION 5 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |
         | func g() =
         |   groth16Verify(
         |     base64'lp7+dPDIOfm77haSFnvr33VwYH/KbIalfOJPRvBLzqlHD8BxunNebMr6Gr6S+u+nh7yLzdqr7HHQNOpZI8mdj/7lR0IBqB9zvRfyTr+guUG22kZo4y2KINDp272xGglKEeTglTxyDUriZJNF/+T6F8w70MR/rV+flvuo6EJ0+HA+A2ZnBbTjOIl9wjisBV+0iISo2JdNY1vPXlpwhlL2fVpW/WlREkF0bKlBadDIbNJBgM4niJGuEZDru3wqrGueETKHPv7hQ8em+p6vQolp7c0iknjXrGnvlpf4QtUtpg3z/D+snWjRPbVqRgKXWtihuIvPFaM6dt7HZEbkeMnXWwSINeYC/j3lqYnce8Jq+XkuF42stVNiooI+TuXECnFdFi9Ib25b9wtyz3H/oKg48He1ftntj5uIRCOBvzkFHGUF6Ty214v3JYvXJjdS4uS2jekplZYoV0aXEnYEOIvfF7d4xay3qkx2NspooM4HeZpiHknIWkUVhGVJBzBDLjLB',
         |     base64'jiGBK+TGHfH8Oadexhdet7ExyIWibSmamWQvffZkyl3WnMoVbTQ3lOks4Mca3sU5qgcaLyQQ1FjFW4g6vtoMapZ43hTGKaWO7bQHsOCvdwHCdwJDulVH16cMTyS9F0BfBJxa88F+JKZc4qMTJjQhspmq755SrKhN9Jf+7uPUhgB4hJTSrmlOkTatgW+/HAf5kZKhv2oRK5p5kS4sU48oqlG1azhMtcHEXDQdcwf9ANel4Z9cb+MQyp2RzI/3hlIx',
         |     base64''
         |   )
         |
         | @Callable(i)
         | func setIsAllowedTrue() = {
         |    let a = !(${List.fill(380)("g()").mkString("||")})
         |    if (a)
         |      then
         |        [BooleanEntry("isAllowed", true)]
         |      else
         |        throw("unexpected")
         | }
         |
       """.stripMargin
    )

  private val scriptWithTooManyStateCalls =
    compile(
      s"""
         |{-# STDLIB_VERSION 5 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |
         | @Callable(i)
         | func foo() = {
         |  let a =
         |    ${List.fill(70)("transferTransactionById(base58'') == unit").mkString(" && ")} &&
         |    !(${List.fill(150)("sigVerify(base64'',base64'',base64'')").mkString("||")})
         |  if (a) then throw("fail") else []
         | }
         |
       """.stripMargin
    )

  private val pureInvokeFee = invokeFee - smartFee
  private val actionsFee    = smartFee * 3 + issueFee
  private val enoughFee     = pureInvokeFee * 1351 + actionsFee
  private val redundantFee  = pureInvokeFee * 10
  private val paymentAmount = 123456L

  private val minSponsoredAssetFee      = 10000L
  private val sponsoredAssetAmount      = 10.waves
  private lazy val sponsoredAssetIssuer = thirdKeyPair
  private lazy val sponsoredAssetId = {
    val id = sender.issue(sponsoredAssetIssuer, quantity = sponsoredAssetAmount, fee = issueFee + smartFee, waitForTx = true).id
    sender.sponsorAsset(sponsoredAssetIssuer, id, baseFee = minSponsoredAssetFee, fee = issueFee + smartFee, waitForTx = true)
    sender.transfer(sponsoredAssetIssuer, caller.toAddress.stringRepr, sponsoredAssetAmount, assetId = Some(id), fee = smartMinFee, waitForTx = true)
    id
  }
  private lazy val sponsorFee = Some((sponsoredAssetId, minSponsoredAssetFee))

  private lazy val scriptedAssetId = {
    val id = sender.issue(dApp, script = Some(compile("true")), waitForTx = true).id
    sender.transfer(dApp, dAppAddress2, 100, assetId = Some(id), fee = smartMinFee, waitForTx = true)
    id
  }

  test("can't set continuation before activation") {
    assertBadRequestAndMessage(
      sender.setScript(dApp, Some(dAppScript1), setScriptFee),
      "State check failed. Reason: ActivationError(Continuation Transaction feature has not been activated yet)"
    )
  }

  test("can set continuation after activation") {
    nodes.waitForHeight(activationHeight)

    sender.setScript(dApp, Some(dAppScript1), setScriptFee, waitForTx = true).id
    val scriptInfo = sender.addressScriptInfo(dAppAddress)
    scriptInfo.script.isEmpty shouldBe false
    scriptInfo.scriptText.isEmpty shouldBe false
    scriptInfo.script.get.startsWith("base64:") shouldBe true

    sender.setScript(dApp2, Some(dAppScript2), setScriptFee, waitForTx = true).id
    val scriptInfo2 = sender.addressScriptInfo(dAppAddress2)
    scriptInfo2.script.isEmpty shouldBe false
    scriptInfo2.scriptText.isEmpty shouldBe false
    scriptInfo2.script.get.startsWith("base64:") shouldBe true
  }

  test("can't set continuation with state calls complexity exceeding limit") {
    def setScript = sender.setScript(dApp, Some(scriptWithTooManyStateCalls), setScriptFee + smartFee, waitForTx = true)
    assertBadRequestAndMessage(setScript, "Complexity of state calls exceeding limit = 4000 for function(s): foo = 4200")
  }

  test("can't invoke continuation if tx version below V3") {
    def invoke =
      sender.invokeScript(
        caller,
        dAppAddress,
        func = Some("foo"),
        args = List(CONST_BOOLEAN(false)),
        fee = pureInvokeFee * 8,
        version = TxVersion.V2,
        waitForTx = true
      )
    assertBadRequestAndMessage(invoke, "Continuation is not allowed for Invoke Script Transaction with version below V3")
  }

  test("continuation with rollback") {
    val startHeight = sender.height + 2
    sender.waitForHeight(startHeight)

    val invoke = sender
      .invokeScript(
        caller,
        dAppAddress,
        func = Some("foo"),
        args = List(CONST_BOOLEAN(false)),
        fee = enoughFee + redundantFee,
        version = TxVersion.V3,
        payment = Seq(Payment(paymentAmount, Waves)),
        waitForTx = true
      )
      ._1

    sender.transactionStatus(invoke.id).applicationStatus.value shouldBe "script_execution_in_progress"

    waitForContinuation(invoke.id, shouldBeFailed = false)
    val endHeight = sender.height

    assertContinuationChain(invoke.id, endHeight, actionsFee = actionsFee)
    assertStateChanges(invoke)
    assertBalances(startHeight, endHeight, enoughFee, actionsFee, expectedPayment = Some(paymentAmount))

    testPartialRollback(startHeight, invoke, actionsFee)
    testFullRollback(startHeight, invoke)
  }

  test("continuation with sponsored asset") {
    sponsoredAssetId // run txs

    val startHeight = sender.height + 2
    sender.waitForHeight(startHeight)

    val invoke = sender
      .invokeScript(
        caller,
        dAppAddress,
        func = Some("foo"),
        args = List(CONST_BOOLEAN(false)),
        fee = Sponsorship.fromWaves(enoughFee + redundantFee, minSponsoredAssetFee),
        feeAssetId = Some(sponsoredAssetId),
        version = TxVersion.V3,
        waitForTx = true
      )
      ._1

    waitForContinuation(invoke.id, shouldBeFailed = false)
    val endHeight = sender.height

    assertContinuationChain(invoke.id, endHeight, feeAssetInfo = sponsorFee, actionsFee = actionsFee)
    assertStateChanges(invoke)
    assertBalances(startHeight, endHeight, enoughFee, actionsFee, Some((sponsoredAssetId, minSponsoredAssetFee, sponsoredAssetIssuer)))
  }

  test("failed continuation") {
    val entry = List(EmptyDataEntry("isAllowed"))
    sender.putData(dApp, entry, calcDataFee(entry, TxVersion.V1) + smartFee, waitForTx = true)

    val startHeight = sender.height + 2
    sender.waitForHeight(startHeight)

    val invoke = sender
      .invokeScript(
        caller,
        dAppAddress,
        func = Some("foo"),
        args = List(CONST_BOOLEAN(true)),
        fee = enoughFee + redundantFee,
        version = TxVersion.V3,
        payment = Seq(Payment(paymentAmount, Waves)),
        waitForTx = true
      )
      ._1
    waitForContinuation(invoke.id, shouldBeFailed = true)
    val endHeight = sender.height

    assertContinuationChain(invoke.id, sender.height, shouldBeFailed = true)
    assertFailedStateChanges(invoke)
    assertBalances(startHeight, endHeight, enoughFee - actionsFee, actionsFee = 0)
  }

  test("failed continuation with sponsored asset") {
    sponsoredAssetId // run txs

    val entry = List(EmptyDataEntry("isAllowed"))
    sender.putData(dApp, entry, calcDataFee(entry, TxVersion.V1) + smartFee, waitForTx = true)

    val startHeight = sender.height + 2
    sender.waitForHeight(startHeight)

    val invoke = sender
      .invokeScript(
        caller,
        dAppAddress,
        func = Some("foo"),
        args = List(CONST_BOOLEAN(true)),
        fee = Sponsorship.fromWaves(enoughFee + redundantFee, minSponsoredAssetFee),
        feeAssetId = Some(sponsoredAssetId),
        version = TxVersion.V3,
        payment = Seq(Payment(paymentAmount, IssuedAsset(ByteStr.decodeBase58(sponsoredAssetId).get))),
        waitForTx = true
      )
      ._1
    waitForContinuation(invoke.id, shouldBeFailed = true)
    val endHeight = sender.height

    assertContinuationChain(invoke.id, sender.height, shouldBeFailed = true, feeAssetInfo = sponsorFee)
    assertFailedStateChanges(invoke)
    assertBalances(
      startHeight,
      endHeight,
      enoughFee - actionsFee,
      actionsFee = 0,
      Some((sponsoredAssetId, minSponsoredAssetFee, sponsoredAssetIssuer))
    )
  }

  test("continuation prioritization") {
    val extraFee    = invokeFee / 1000
    val assetScript = compile(s"""getBooleanValue(Address(base58'$dAppAddress2'), "isAllowed")""")
    val assetId     = sender.issue(dApp, quantity = 100, script = Some(assetScript), fee = issueFee + smartFee, waitForTx = true).id

    val entry = List(BooleanDataEntry("isAllowed", value = false))
    sender.putData(dApp2, entry, calcDataFee(entry, TxVersion.V1) + smartFee, waitForTx = true)

    val invoke1 = sender
      .invokeScript(
        caller,
        dAppAddress,
        Some("performActionWithAsset"),
        List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet()),
        fee = enoughFee,
        version = TxVersion.V3
      )
      ._1

    val invoke2 = sender
      .invokeScript(
        caller,
        dAppAddress2,
        Some("setIsAllowedTrue"),
        fee = enoughFee,
        extraFeePerStep = extraFee,
        version = TxVersion.V3
      )
      ._1

    waitForContinuation(invoke2.id, shouldBeFailed = false)
    waitForContinuation(invoke1.id, shouldBeFailed = false)

    assertContinuationChain(invoke1.id, sender.height, actionsFee = smartFee)
    assertContinuationChain(invoke2.id, sender.height, extraFeePerStep = extraFee)
  }

  test("hold transactions from DApp address until continuation is completed") {
    val startHeight = sender.height
    val invoke = sender.invokeScript(
      caller,
      dAppAddress,
      func = Some("foo"),
      args = List(CONST_BOOLEAN(false)),
      fee = enoughFee,
      version = TxVersion.V3,
      waitForTx = true
    )
    sendTransactions(dApp)
    waitForContinuation(invoke._1.id, shouldBeFailed = false)
    assertAbsenceOfTransactions(startHeight, sender.height)

    nodes.waitForHeight(sender.height + 2)
    assertExistenceOfTransactions(dApp, startHeight, sender.height)
  }

  test("don't forbid transactions from other addresses while continuation is not completed") {
    val startHeight = sender.height
    sender.invokeScript(
      caller,
      dAppAddress,
      func = Some("foo"),
      args = List(CONST_BOOLEAN(false)),
      fee = enoughFee,
      version = TxVersion.V3,
      waitForTx = true
    )
    sendTransactions(caller)
    nodes.waitForHeight(sender.height + 2)
    assertExistenceOfTransactions(caller, startHeight, sender.height)
  }

  test("insufficient fee") {
    lazy val invokeScriptTx = sender.invokeScript(
      caller,
      dAppAddress,
      func = Some("foo"),
      args = List(CONST_BOOLEAN(false)),
      fee = invokeFee,
      version = TxVersion.V3
    )

    assertBadRequestAndMessage(
      invokeScriptTx,
      "Fee in WAVES for InvokeScriptTransaction (900000 in WAVES) " +
        "with 1353 invocation steps " +
        "does not exceed minimal value of 676500000 WAVES."
    )
  }

  private def waitForContinuation(invokeId: String, shouldBeFailed: Boolean): Boolean =
    nodes.waitFor(
      s"chain of continuation for InvokeScript Transaction with id = $invokeId is completed"
    )(
      _.transactionStatus(invokeId).applicationStatus.value
    )(
      _.forall(_ == (if (shouldBeFailed) "script_execution_failed" else "succeeded"))
    )

  private def assertContinuationChain(
      invokeId: String,
      completionHeight: Int,
      shouldBeFailed: Boolean = false,
      feeAssetInfo: Option[(String, Long)] = None,
      extraFeePerStep: Long = 0,
      actionsFee: Long = 0
  ): Unit = {
    nodes.foreach { node =>
      val endStatus = if (shouldBeFailed) "script_execution_failed" else "succeeded"

      val invoke = node.transactionInfo[DebugStateChanges](invokeId)
      val continuations =
        node
          .blockSeq(invoke.height, completionHeight)
          .flatMap(_.transactions)
          .filter(tx => tx._type == ContinuationTransaction.typeId && tx.invokeScriptTransactionId.contains(invokeId))

      continuations.zipWithIndex
        .foreach {
          case (c, i) =>
            c.version.value shouldBe TxVersion.V1
            c.nonce.value should be >= 0

            val expectedFeeInWaves = if (i == continuations.size - 1) pureInvokeFee + actionsFee else pureInvokeFee
            val expectedFeeInAttachedAsset =
              feeAssetInfo.fold(expectedFeeInWaves) { case (_, sponsorship) => Sponsorship.fromWaves(expectedFeeInWaves, sponsorship) }
            val totalExpectedFee = expectedFeeInAttachedAsset + extraFeePerStep
            c.fee shouldBe totalExpectedFee

            val expectedStatus = if (i == continuations.size - 1) endStatus else "succeeded"
            c.applicationStatus.value shouldBe expectedStatus

            val txInfo = node.transactionInfo[DebugStateChanges](c.id)
            txInfo.height should (be >= invoke.height and be <= completionHeight)
        }

      invoke.extraFeePerStep.value shouldBe extraFeePerStep
      invoke.applicationStatus.value shouldBe endStatus
      invoke.continuationTransactionIds.value shouldBe continuations.map(_.id)
    }
  }

  private def assertBalances(
      startHeight: Int,
      endHeight: Int,
      totalFee: Long,
      actionsFee: Long,
      sponsorship: Option[(String, Long, KeyPair)] = None,
      expectedPayment: Option[Long] = None
  ): Unit = {
    def balanceDiff(address: String, assetId: Option[String] = None) =
      balanceDiffByHeight(startHeight, endHeight)(address, assetId)

    val minersBalanceIncrease =
      nodes
        .map { node =>
          node.address -> balanceDiff(node.address)
        }
        .filterNot(_._2 == 0)
        .toMap

    val blockReward    = sender.lastBlock().reward.value
    val blocks         = sender.blockSeq(startHeight, endHeight)
    val lastStepHeight = blocks.findLast(_.transactions.nonEmpty).get.height

    val (blockRewardDistribution, _) =
      blocks
        .foldLeft((Map[String, Long](), 0L)) {
          case ((balances, previousBlockReward), block) =>
            val actionsReward      = if (block.height == lastStepHeight) actionsFee else 0
            val transactionsReward = (block.transactions.size * pureInvokeFee + actionsReward) * 2 / 5
            val totalReward        = previousBlockReward + transactionsReward + blockReward
            val result             = balances |+| Map(block.generator -> totalReward)
            (result, transactionsReward * 3 / 2)
        }

    minersBalanceIncrease.values.sum shouldBe totalFee + (endHeight - startHeight + 1) * blockReward
    minersBalanceIncrease should contain theSameElementsAs blockRewardDistribution

    sponsorship.fold {
      balanceDiff(callerAddress) shouldBe -totalFee - expectedPayment.getOrElse(0L)
      balanceDiff(dAppAddress) shouldBe expectedPayment.getOrElse(0L)
    } {
      case (assetId, minSponsoredFee, assetIssuer) =>
        val feeInAsset = Sponsorship.fromWaves(totalFee, minSponsoredFee)
        balanceDiff(assetIssuer.toAddress.toString) shouldBe -totalFee
        balanceDiff(assetIssuer.toAddress.toString, Some(assetId)) shouldBe feeInAsset
        balanceDiff(callerAddress, Some(assetId)) shouldBe -feeInAsset - expectedPayment.getOrElse(0L)
        balanceDiff(dAppAddress) shouldBe expectedPayment.getOrElse(0L)
    }
  }

  private def assertUnchangedBalances(startHeight: Int, endHeight: Int): Unit = {
    def balanceDiff(address: String) = balanceDiffByHeight(startHeight, endHeight)(address)
    val blockReward = sender.lastBlock().reward.value

    nodes.foreach(node => balanceDiff(node.address) % blockReward shouldBe 0)
    balanceDiff(callerAddress) shouldBe 0
    balanceDiff(dAppAddress) shouldBe 0
  }

  private def balanceDiffByHeight(startHeight: Int, endHeight: Int)(address: String, assetId: Option[String] = None): Long = {
    val startBalance  = sender.accountsBalances(Some(startHeight - 1), Seq(address), assetId).head._2
    val resultBalance = sender.accountsBalances(Some(endHeight), Seq(address), assetId).head._2
    resultBalance - startBalance
  }

  private def assertStateChanges(invoke: Transaction): Unit = {
    val matchExpectingChanges = matchPattern {
      case Seq(
          StateChangesDetails(
            Seq(PutDataResponse("boolean", true, "entry1")),
            Seq(TransfersInfoResponse(`callerAddress`, transferAssetId, 10)),
            Seq(IssueInfoResponse(_, "Asset", "Description", 2, 5, true, None, 0)),
            Seq(ReissueInfoResponse(reissueAssetId, true, 100)),
            Seq(BurnInfoResponse(burnAssetId, 100)),
            Nil,
            None
          )
          ) if Set(reissueAssetId, burnAssetId, transferAssetId.value).head == scriptedAssetId =>
    }
    sender.debugStateChanges(invoke.id).stateChanges.toSeq should matchExpectingChanges
    sender.debugStateChangesByAddress(dAppAddress, 1).flatMap(_.stateChanges) should matchExpectingChanges

    sender.getDataByKey(dAppAddress, "entry1") shouldBe BooleanDataEntry("entry1", true)

    sender.transactionsByAddress(dAppAddress, limit = 10).find(_.id == invoke.id) shouldBe defined
    sender.transactionsByAddress(callerAddress, limit = 10).find(_.id == invoke.id) shouldBe defined
  }

  private def assertFailedStateChanges(invoke: Transaction): Unit = {
    sender.debugStateChanges(invoke.id).stateChanges shouldBe None

    sender.transactionsByAddress(dAppAddress, limit = 10).find(_.id == invoke.id) shouldBe None
    sender.transactionsByAddress(callerAddress, limit = 10).find(_.id == invoke.id) shouldBe defined

    sender.getData(dAppAddress, "entry1") shouldBe Seq()
  }

  private def assertNoStateChanges(invoke: Transaction): Unit = {
    sender.debugStateChangesByAddress(dAppAddress, 10).flatMap(_.stateChanges) shouldBe Seq()

    sender.transactionsByAddress(dAppAddress, limit = 10).find(_.id == invoke.id) shouldBe None
    sender.transactionsByAddress(callerAddress, limit = 10).find(_.id == invoke.id) shouldBe None

    sender.getData(dAppAddress, "entry1") shouldBe Seq()
  }

  private def testPartialRollback(startHeight: Int, invoke: Transaction, actionsFee: Long): Unit = {
    nodes.rollback(startHeight + 1, returnToUTX = false)

    sender.transactionStatus(invoke.id).applicationStatus.value shouldBe "script_execution_in_progress"
    sender.getData(dAppAddress) shouldBe Nil

    waitForContinuation(invoke.id, shouldBeFailed = false)
    val endHeight = sender.height

    assertContinuationChain(invoke.id, endHeight, actionsFee = actionsFee)
    assertStateChanges(invoke)
    assertBalances(startHeight, endHeight, enoughFee, actionsFee, expectedPayment = Some(paymentAmount))
  }

  private def testFullRollback(startHeight: Int, invoke: Transaction): Unit = {
    nodes.rollback(startHeight - 1, returnToUTX = false)
    val endHeight = sender.height + 1
    nodes.waitForHeight(endHeight)
    
    sender.blockSeq(startHeight, endHeight).flatMap(_.transactions) shouldBe Nil
    assertNoStateChanges(invoke)
    assertUnchangedBalances(startHeight, endHeight)
  }

  private def assertAbsenceOfTransactions(fromHeight: Int, toHeight: Int): Unit =
    nodes.foreach { node =>
      val transactions =
        node
          .blockSeq(fromHeight, toHeight)
          .flatMap(_.transactions)

      val invokeIndex           = transactions.indexWhere(_._type == smart.InvokeScriptTransaction.typeId)
      val lastContinuationIndex = transactions.lastIndexWhere(_._type == ContinuationTransaction.typeId)

      val otherTransactionsExist =
        transactions
          .slice(invokeIndex, lastContinuationIndex)
          .exists(tx => tx._type != smart.InvokeScriptTransaction.typeId && tx._type != ContinuationTransaction.typeId)

      otherTransactionsExist shouldBe false
    }

  private def assertExistenceOfTransactions(txSender: KeyPair, fromHeight: Int, toHeight: Int): Unit =
    nodes.foreach { node =>
      val transactions = node.blockSeq(fromHeight, toHeight).flatMap(_.transactions)
      val publicKey    = txSender.toAddress.toString
      transactions.exists(tx => tx._type == DataTransaction.typeId && tx.sender.get == publicKey) shouldBe true
      transactions.exists(tx => tx._type == TransferTransaction.typeId && tx.sender.get == publicKey) shouldBe true
      transactions.exists(tx => tx._type == CreateAliasTransaction.typeId && tx.sender.get == publicKey) shouldBe true
    }

  private def sendTransactions(txSender: KeyPair) = {
    sender.transfer(txSender, thirdAddress, amount = 1, smartMinFee)
    sender.createAlias(txSender, s"alias${System.currentTimeMillis()}", smartMinFee)
    val data = List(StringDataEntry("key", "value"))
    sender.putData(txSender, data, calcDataFee(data, TxVersion.V1) + smartFee)
  }
}
