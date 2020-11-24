package com.wavesplatform.it.sync.smartcontract

import cats.implicits._
import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{DebugStateChanges, Transaction}
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_BYTESTR}
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.state._
import com.wavesplatform.transaction.smart.ContinuationTransaction
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
      .overrideBase(_.raw("waves.blockchain.use-evaluator-v2 = true"))
      .buildNonConflicting()

  private lazy val dApp: KeyPair   = firstKeyPair
  private lazy val caller: KeyPair = secondKeyPair

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

  private val script =
    compile(
      s"""
         |{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |
         | @Callable(i)
         | func foo(fail: Boolean) = {
         |  let a =
         |    getInteger(Address(base58''), "key") == unit &&
         |    !(${List.fill(150)("sigVerify(base64'',base64'',base64'')").mkString("||")})
         |  if (fail && a)
         |    then
         |      throw("fail")
         |    else
         |      [BooleanEntry("a", a), BinaryEntry("sender", i.caller.bytes)]
         | }
         |
         | @Callable(i)
         | func setIsAllowedTrue() = {
         |    let a = !(${List.fill(150)("sigVerify(base64'',base64'',base64'')").mkString("||")})
         |    if (a)
         |      then
         |        [BooleanEntry("isAllowed", true)]
         |      else
         |        throw("unexpected")
         | }
         |
         | @Callable(i)
         | func performActionWithAsset(assetId: ByteVector) = {
         |    let a = !(${List.fill(150)("sigVerify(base64'',base64'',base64'')").mkString("||")})
         |    if (a)
         |      then
         |        [Burn(assetId, 1)]
         |      else
         |        throw("unexpected")
         | }
         |
       """.stripMargin
    )

  private val scriptWithTooManyStateCalls =
    compile(
      s"""
         |{-# STDLIB_VERSION 4 #-}
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

  private val minSponsoredAssetFee = 10000
  private val sponsoredAssetAmount = 10.waves
  private lazy val sponsoredAssetId = {
    val id = sender.issue(thirdKeyPair, quantity = sponsoredAssetAmount, waitForTx = true).id
    sender.sponsorAsset(thirdKeyPair, id, baseFee = minSponsoredAssetFee, waitForTx = true)
    sender.transfer(thirdKeyPair, caller.toAddress.stringRepr, sponsoredAssetAmount, assetId = Some(id), waitForTx = true)
    id
  }

  private val pureInvokeFee = invokeFee - smartFee

  test("can't set continuation before activation") {
    assertBadRequestAndMessage(
      sender.setScript(dApp, Some(script), setScriptFee),
      "State check failed. Reason: Contract function (foo) is too complex: 30635 > 4000"
    )
  }

  test("can set continuation after activation") {
    nodes.waitForHeight(activationHeight)
    sender.setScript(dApp, Some(script), setScriptFee, waitForTx = true).id

    val scriptInfo = sender.addressScriptInfo(dApp.toAddress.toString)
    scriptInfo.script.isEmpty shouldBe false
    scriptInfo.scriptText.isEmpty shouldBe false
    scriptInfo.script.get.startsWith("base64:") shouldBe true
  }

  test("can't set continuation with state calls complexity exceeding limit") {
    def setScript = sender.setScript(dApp, Some(scriptWithTooManyStateCalls), setScriptFee, waitForTx = true)
    assertBadRequestAndMessage(setScript, "Complexity of state calls exceeding limit = 4000 for function(s): foo = 4200")
  }

  test("can't invoke continuation if tx version below V3") {
    def invoke =
      sender.invokeScript(
        caller,
        dApp.toAddress.toString,
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

    val enoughFee    = pureInvokeFee * 8
    val redundantFee = 123456789

    val invoke = sender
      .invokeScript(
        caller,
        dApp.toAddress.toString,
        func = Some("foo"),
        args = List(CONST_BOOLEAN(false)),
        fee = enoughFee + redundantFee,
        version = TxVersion.V3,
        waitForTx = true
      )
      ._1

    sender.transactionStatus(invoke.id).applicationStatus.value shouldBe "script_execution_in_progress"

    waitForContinuation(invoke.id, shouldBeFailed = false)
    val endHeight = sender.height

    assertBalances(startHeight, endHeight, enoughFee)
    assertContinuationChain(invoke.id, sender.height)
    nodes.foreach { node =>
      node.getDataByKey(dApp.toAddress.toString, "entry1") shouldBe BooleanDataEntry("entry1", true)
      node.getDataByKey(dApp.toAddress.toString, "entry2") shouldBe BinaryDataEntry("entry2", ByteStr(Base58.decode(caller.toAddress.toString)))
    }
    sender.transactionsByAddress(dApp.toAddress.toString, limit = 10).find(_.id == invoke.id) shouldBe defined
    sender.transactionsByAddress(caller.toAddress.toString, limit = 10).find(_.id == invoke.id) shouldBe defined

    testPartialRollback(startHeight, invoke)
    testFullRollback(startHeight, invoke)
  }

  test("continuation with sponsored asset") {
    val invoke = sender
      .invokeScript(
        caller,
        dApp.toAddress.toString,
        func = Some("foo"),
        args = List(CONST_BOOLEAN(false)),
        fee = 1.waves,
        feeAssetId = Some(sponsoredAssetId),
        version = TxVersion.V3,
        waitForTx = true
      )
      ._1
    waitForContinuation(invoke.id, shouldBeFailed = false)
    assertContinuationChain(invoke.id, sender.height, feeAssetInfo = Some((sponsoredAssetId, minSponsoredAssetFee)))
    nodes.foreach { node =>
      node.getDataByKey(dApp.toAddress.toString, "entry1") shouldBe BooleanDataEntry("entry1", true)
      node.getDataByKey(dApp.toAddress.toString, "entry2") shouldBe BinaryDataEntry("entry2", ByteStr(Base58.decode(caller.toAddress.toString)))
    }
    sender.transactionsByAddress(dApp.toAddress.toString, limit = 10).find(_.id == invoke.id) shouldBe defined
    sender.transactionsByAddress(caller.toAddress.toString, limit = 10).find(_.id == invoke.id) shouldBe defined
  }

  test("failed continuation") {
    val invoke = sender
      .invokeScript(
        caller,
        dApp.toAddress.toString,
        func = Some("foo"),
        args = List(CONST_BOOLEAN(true)),
        fee = 1.waves,
        version = TxVersion.V3,
        waitForTx = true
      )
      ._1
    waitForContinuation(invoke.id, shouldBeFailed = true)
    assertContinuationChain(invoke.id, sender.height, shouldBeFailed = true)
    sender.transactionsByAddress(dApp.toAddress.toString, limit = 10).find(_.id == invoke.id) shouldBe None
    sender.transactionsByAddress(caller.toAddress.toString, limit = 10).find(_.id == invoke.id) shouldBe defined
  }

  test("failed continuation with sponsored asset") {
    val invoke = sender
      .invokeScript(
        caller,
        dApp.toAddress.toString,
        func = Some("foo"),
        args = List(CONST_BOOLEAN(true)),
        fee = 1.waves,
        feeAssetId = Some(sponsoredAssetId),
        version = TxVersion.V3,
        waitForTx = true
      )
      ._1
    waitForContinuation(invoke.id, shouldBeFailed = true)
    assertContinuationChain(invoke.id, sender.height, shouldBeFailed = true, feeAssetInfo = Some((sponsoredAssetId, minSponsoredAssetFee)))
    sender.transactionsByAddress(dApp.toAddress.toString, limit = 10).find(_.id == invoke.id) shouldBe None
    sender.transactionsByAddress(caller.toAddress.toString, limit = 10).find(_.id == invoke.id) shouldBe defined
  }

  test("continuation prioritization") {
    val assetScript = compile(s"""getBooleanValue(Address(base58'${dApp.toAddress.toString}'), "isAllowed")""")
    val assetId     = sender.issue(dApp, quantity = 100, script = Some(assetScript), fee = issueFee + smartFee, waitForTx = true).id

    val entry = List(BooleanDataEntry("isAllowed", value = false))
    sender.putData(dApp, entry, calcDataFee(entry, TxVersion.V1) + smartFee, waitForTx = true)

    val invoke1 = sender
      .invokeScript(
        caller,
        dApp.toAddress.toString,
        Some("performActionWithAsset"),
        List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet()),
        fee = invokeFee * 10,
        version = TxVersion.V3
      )
      ._1

    val invoke2 = sender
      .invokeScript(
        caller,
        dApp.toAddress.toString,
        Some("setIsAllowedTrue"),
        fee = invokeFee * 10,
        extraFeePerStep = invokeFee / 10,
        version = TxVersion.V3
      )
      ._1

    waitForContinuation(invoke2.id, shouldBeFailed = false)
    waitForContinuation(invoke1.id, shouldBeFailed = false)

    assertContinuationChain(invoke1.id, sender.height, actions = 1)
    assertContinuationChain(invoke2.id, sender.height, extraFeePerStep = invokeFee / 10)
  }

  test("hold transactions from DApp address until continuation is completed") {
    val startHeight = sender.height
    val invoke = sender.invokeScript(
      caller,
      dApp.toAddress.toString,
      func = Some("foo"),
      args = List(CONST_BOOLEAN(false)),
      fee = 1.waves,
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
      dApp.toAddress.toString,
      func = Some("foo"),
      args = List(CONST_BOOLEAN(false)),
      fee = 1.waves,
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
      dApp.toAddress.toString,
      func = Some("foo"),
      args = List(CONST_BOOLEAN(false)),
      fee = invokeFee,
      version = TxVersion.V3
    )

    assertBadRequestAndMessage(
      invokeScriptTx,
      "Fee in WAVES for InvokeScriptTransaction (900000 in WAVES) " +
        "with 8 invocation steps " +
        "does not exceed minimal value of 4000000 WAVES."
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
      actions: Int = 0
  ): Unit = {
    nodes.foreach { node =>
      val endStatus = if (shouldBeFailed) "script_execution_failed" else "succeeded"

      val invoke = node.transactionInfo[DebugStateChanges](invokeId)
      val continuations =
        node
          .blockSeq(invoke.height, completionHeight)
          .flatMap(_.transactions)
          .filter(tx => tx._type == ContinuationTransaction.typeId && tx.invokeScriptTransactionId.contains(invokeId))
      val continuationIds = continuations.map(_.id)

      continuations.zipWithIndex
        .foreach {
          case (c, i) =>
            c.version.value shouldBe TxVersion.V1
            c.step.value shouldBe i
            c.extraFeePerStep.value shouldBe extraFeePerStep
            c.feeAssetId shouldBe feeAssetInfo.map(_._1)
            c.call shouldBe invoke.call
            c.dApp shouldBe invoke.dApp

            val expectedFeeInWaves = if (i == continuations.size - 1) pureInvokeFee + actions * smartFee else pureInvokeFee
            val expectedFeeInAttachedAsset =
              feeAssetInfo.fold(expectedFeeInWaves) { case (_, sponsorship) => Sponsorship.fromWaves(expectedFeeInWaves, sponsorship) }
            val totalExpectedFee = expectedFeeInAttachedAsset + extraFeePerStep
            c.fee shouldBe totalExpectedFee

            val expectedStatus = if (i == continuations.size - 1) endStatus else "succeeded"
            c.applicationStatus.value shouldBe expectedStatus

            val txInfo = node.transactionInfo[DebugStateChanges](c.id)
            txInfo.height should (be >= invoke.height and be <= completionHeight)
            txInfo.continuationTransactionIds.value shouldBe continuationIds
        }

      invoke.extraFeePerStep.value shouldBe extraFeePerStep
      invoke.applicationStatus.value shouldBe endStatus
      invoke.timestamp +: continuations.map(_.timestamp) shouldBe sorted
      invoke.continuationTransactionIds.value shouldBe continuationIds
    }
  }

  private def assertBalances(startHeight: Int, endHeight: Int, expectingFee: Long): Unit = {
    val startCallerBalance    = sender.balanceAtHeight(caller.toAddress.toString, startHeight - 1)
    val resultCallerBalance   = sender.balanceAtHeight(caller.toAddress.toString, endHeight)
    val callerBalanceDecrease = startCallerBalance - resultCallerBalance

    val minersBalanceIncrease =
      nodes.flatMap { node =>
        val startBalance  = sender.balanceAtHeight(node.address, startHeight - 1)
        val resultBalance = sender.balanceAtHeight(node.address, endHeight)
        if (resultBalance == startBalance)
          Nil
        else
          Seq(node.address -> (resultBalance - startBalance))
      }.toMap

    val blockReward = miner.lastBlock().reward.value
    val (blockRewardDistribution, _) =
      miner
        .blockSeq(startHeight, endHeight)
        .foldLeft((Map[String, Long](), 0L)) {
          case ((balances, previousBlockReward), block) =>
            val transactionsReward = block.transactions.map(_ => pureInvokeFee).sum * 2 / 5
            val totalReward        = previousBlockReward + transactionsReward + blockReward
            val result             = balances |+| Map(block.generator -> totalReward)
            (result, transactionsReward * 3 / 2)
        }

    callerBalanceDecrease shouldBe expectingFee
    blockRewardDistribution.values.sum shouldBe expectingFee + (endHeight - startHeight + 1) * blockReward
    blockRewardDistribution should contain theSameElementsAs minersBalanceIncrease
  }

  private def testPartialRollback(startHeight: Int, invoke: Transaction): Unit = {
    nodes.rollback(startHeight + 1, returnToUTX = false)

    sender.transactionStatus(invoke.id).applicationStatus.value shouldBe "script_execution_in_progress"
    sender.getData(dApp.toAddress.toString) shouldBe Nil

    waitForContinuation(invoke.id, shouldBeFailed = false)
    assertContinuationChain(invoke.id, sender.height, shouldBeFailed = false, None)
  }

  private def testFullRollback(startHeight: Int, invoke: Transaction): Unit = {
    nodes.rollback(startHeight - 1, returnToUTX = false)
    nodes.waitForHeight(sender.height + 1)

    sender.getData(dApp.toAddress.toString) shouldBe Nil
    sender.transactionsByAddress(dApp.toAddress.toString, limit = 10).find(_.id == invoke.id) shouldBe None
    sender.transactionsByAddress(caller.toAddress.toString, limit = 10).find(_.id == invoke.id) shouldBe None
    sender.blockSeq(startHeight, sender.height).flatMap(_.transactions) shouldBe Nil
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
