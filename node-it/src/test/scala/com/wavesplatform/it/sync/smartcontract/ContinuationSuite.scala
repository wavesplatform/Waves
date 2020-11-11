package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.DebugStateChanges
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BOOLEAN
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.ContinuationTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{CreateAliasTransaction, DataTransaction, TxVersion, smart}
import monix.eval.Coeval
import org.scalatest.OptionValues

class ContinuationSuite extends BaseTransactionSuite with OptionValues {
  private val activationHeight = 10

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

  private val script = {
    val scriptText =
      s"""
         |{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |
         | @Callable(inv)
         | func foo(fail: Boolean) = {
         |  let a =
         |    getInteger(Address(base58''), "key") == unit &&
         |    !(${List.fill(150)("sigVerify(base64'',base64'',base64'')").mkString("||")})
         |  if (fail && a)
         |    then
         |      throw("fail")
         |    else
         |      [BooleanEntry("a", a), BinaryEntry("sender", inv.caller.bytes)]
         | }
         |
       """.stripMargin
    ScriptCompiler.compile(scriptText, dummyEstimator).explicitGet()._1.bytes().base64
  }

  private val minSponsoredAssetFee = 10000
  private val sponsoredAssetAmount = 10.waves
  private lazy val sponsoredAssetId = {
    val id = sender.issue(thirdKeyPair, quantity = sponsoredAssetAmount, waitForTx = true).id
    sender.sponsorAsset(thirdKeyPair, id, baseFee = minSponsoredAssetFee, waitForTx = true)
    sender.transfer(thirdKeyPair, caller.toAddress.stringRepr, sponsoredAssetAmount, assetId = Some(id), waitForTx = true)
    id
  }

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

  test("successful continuation") {
    val invoke = sender
      .invokeScript(
        caller,
        dApp.toAddress.toString,
        func = Some("foo"),
        args = List(CONST_BOOLEAN(false)),
        payment = Seq(Payment(1.waves, Waves)),
        fee = 1.waves,
        version = TxVersion.V3,
        waitForTx = true
      )
      ._1
    waitForContinuation(invoke.id, shouldBeFailed = false)
    assertContinuationChain(invoke.id, sender.height, shouldBeFailed = false, feeAssetInfo = None)
    nodes.foreach { node =>
      node.getDataByKey(dApp.toAddress.toString, "a") shouldBe BooleanDataEntry("a", true)
      node.getDataByKey(dApp.toAddress.toString, "sender") shouldBe BinaryDataEntry("sender", ByteStr(Base58.decode(caller.toAddress.toString)))
    }
    sender.transactionsByAddress(dApp.toAddress.toString, limit = 10).find(_.id == invoke.id) shouldBe defined
    sender.transactionsByAddress(caller.toAddress.toString, limit = 10).find(_.id == invoke.id) shouldBe defined
  }

  test("successful continuation with sponsored asset") {
    val invoke = sender
      .invokeScript(
        caller,
        dApp.toAddress.toString,
        func = Some("foo"),
        args = List(CONST_BOOLEAN(false)),
        payment = Seq(Payment(1.waves, Waves)),
        fee = 1.waves,
        feeAssetId = Some(sponsoredAssetId),
        version = TxVersion.V3,
        waitForTx = true
      )
      ._1
    waitForContinuation(invoke.id, shouldBeFailed = false)
    assertContinuationChain(invoke.id, sender.height, shouldBeFailed = false, feeAssetInfo = Some((sponsoredAssetId, minSponsoredAssetFee)))
    nodes.foreach { node =>
      node.getDataByKey(dApp.toAddress.toString, "a") shouldBe BooleanDataEntry("a", true)
      node.getDataByKey(dApp.toAddress.toString, "sender") shouldBe BinaryDataEntry("sender", ByteStr(Base58.decode(caller.toAddress.toString)))
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
        payment = Seq(Payment(1.waves, Waves)),
        fee = 1.waves,
        version = TxVersion.V3,
        waitForTx = true
      )
      ._1
    waitForContinuation(invoke.id, shouldBeFailed = true)
    assertContinuationChain(invoke.id, sender.height, shouldBeFailed = true, feeAssetInfo = None)
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
        payment = Seq(Payment(1.waves, Waves)),
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

    sender.waitForHeight(sender.height + 2)
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
      payment = Seq(Payment(1.waves, Waves)),
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
      _.blockSeq(sender.height - 2, sender.height)
        .flatMap(_.transactions)
        .exists ( tx =>
          tx._type == ContinuationTransaction.typeId &&
          tx.applicationStatus.contains(if (shouldBeFailed) "script_execution_failed" else "succeeded") &&
          tx.invokeScriptTransactionId.contains(invokeId)
        )
    )(
      _.forall(identity)
    )

  private def assertContinuationChain(invokeId: String, completionHeight: Int, shouldBeFailed: Boolean, feeAssetInfo: Option[(String, Long)]): Unit = {
    import play.api.libs.json._
    nodes.foreach {
      node =>
        val invokeRaw = node.transactionInfo[JsObject](invokeId)
        val invoke = invokeRaw.as[DebugStateChanges]
        invoke.applicationStatus.value shouldBe "script_execution_in_progress"

        val continuations =
          node
            .blockSeq(invoke.height, completionHeight)
            .flatMap(_.transactions)
            .filter(tx => tx._type == ContinuationTransaction.typeId && tx.invokeScriptTransactionId.contains(invokeId))
        for((t,b) <- invoke.continuationTransactionIds.get.zip(continuations.map(_.id))) {
          t shouldBe b
          val cont = node.transactionInfo[JsObject](t)
          (cont \ "version").as[Int] shouldBe 1
          (cont \ "height").asOpt[Int].nonEmpty shouldBe true
          (cont \ "extraFeePerStep").asOpt[Long].nonEmpty shouldBe true
          (cont \ "call").as[JsObject] shouldBe (invokeRaw \ "call").as[JsObject]
          (cont \ "dApp").as[String] shouldBe (invokeRaw \ "dApp").as[String]
        }

      val pureInvokeFee = invokeFee - smartFee
      val correctedFee  = feeAssetInfo.fold(pureInvokeFee) { case (_, sponsorship) => Sponsorship.fromWaves(pureInvokeFee, sponsorship) }
      continuations.foreach(_.fee shouldBe correctedFee)
      // TODO continuations.foreach(_.feeAssetId shouldBe feeAssetInfo.map(_._1))
      continuations.dropRight(1).foreach(_.applicationStatus.value shouldBe "script_execution_in_progress")
      continuations.last.applicationStatus.value shouldBe (if (shouldBeFailed) "script_execution_failed" else "succeeded")
      continuations.map(_.nonce.value) shouldBe continuations.indices
      invoke.timestamp +: continuations.map(_.timestamp) shouldBe sorted
    }
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
