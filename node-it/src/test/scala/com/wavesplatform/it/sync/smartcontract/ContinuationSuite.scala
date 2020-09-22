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
  private val activationHeight = 5

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
         | func foo() = {
         |  let a =
         |    getInteger(Address(base58''), "key") == unit &&
         |    !(${List.fill(150)("sigVerify(base64'',base64'',base64'')").mkString("||")})
         |  [BooleanEntry("a", a), BinaryEntry("sender", inv.caller.bytes)]
         | }
         |
       """.stripMargin
    ScriptCompiler.compile(scriptText, dummyEstimator).explicitGet()._1.bytes().base64
  }

  test("can't set continuation before activation") {
    assertBadRequestAndMessage(
      sender.setScript(dApp, Some(script), setScriptFee),
      "State check failed. Reason: Contract function (foo) is too complex: 30630 > 4000"
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
    val invoke = sender.invokeScript(
      caller,
      dApp.toAddress.toString,
      func = Some("foo"),
      args = Nil,
      payment = Seq(Payment(1.waves, Waves)),
      fee = 1.waves,
      version = TxVersion.V2,
      waitForTx = true
    )
    waitForContinuation(invoke._1.id)
    assertContinuationChain(invoke._1.id, sender.height)
    nodes.foreach { node =>
      node.getDataByKey(dApp.toAddress.toString, "a") shouldBe BooleanDataEntry("a", true)
      node.getDataByKey(dApp.toAddress.toString, "sender") shouldBe BinaryDataEntry("sender", ByteStr(Base58.decode(caller.toAddress.toString)))
    }
  }

  test("hold transactions from DApp address until continuation is completed") {
    val startHeight = sender.height
    val invoke = sender.invokeScript(
      caller,
      dApp.toAddress.toString,
      func = Some("foo"),
      args = Nil,
      fee = 1.waves,
      version = TxVersion.V2,
      waitForTx = true
    )
    sendTransactions(dApp)
    waitForContinuation(invoke._1.id)
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
      args = Nil,
      fee = 1.waves,
      version = TxVersion.V2,
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
      args = Nil,
      payment = Seq(Payment(1.waves, Waves)),
      fee = invokeFee,
      version = TxVersion.V2
    )

    assertBadRequestAndMessage(
      invokeScriptTx,
      "Fee in WAVES for InvokeScriptTransaction (900000 in WAVES) " +
        "with 8 invocation steps " +
        "does not exceed minimal value of 4000000 WAVES."
    )
  }

  private def waitForContinuation(invokeId: String): Boolean =
    nodes.waitFor(
      s"chain of continuation for InvokeScript Transaction with id = $invokeId is completed"
    )(
      _.blockSeq(sender.height - 2, sender.height)
        .flatMap(_.transactions)
        .exists { tx =>
          tx._type == ContinuationTransaction.typeId &&
          tx.applicationStatus.contains("succeeded") &&
          tx.invokeScriptTransactionId.contains(invokeId)
        }
    )(
      _.forall(identity)
    )

  private def assertContinuationChain(invokeId: String, completionHeight: Int): Unit =
    nodes.foreach {
      node =>
        val invoke = node.transactionInfo[DebugStateChanges](invokeId)
        val continuations =
          node
            .blockSeq(invoke.height, completionHeight)
            .flatMap(_.transactions)
            .filter(tx => tx._type == ContinuationTransaction.typeId && tx.invokeScriptTransactionId.contains(invokeId))

        invoke.applicationStatus.value shouldBe "script_execution_in_progress"
        continuations.dropRight(1).foreach(_.applicationStatus.value shouldBe "script_execution_in_progress")
        continuations.last.applicationStatus.value shouldBe "succeeded"
        continuations.map(_.nonce.value) shouldBe continuations.indices
        invoke.timestamp +: continuations.map(_.timestamp) shouldBe sorted
    }

  private def assertAbsenceOfTransactions(fromHeight: Int, toHeight: Int): Unit =
    nodes.foreach {
      node =>
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

  private def assertExistenceOfTransactions(txSender: KeyPair, fromHeight: Int, toHeight: Int): Unit = {
    nodes.foreach {
      node =>
        val transactions = node.blockSeq(fromHeight, toHeight).flatMap(_.transactions)
        val publicKey    = txSender.toAddress.toString
        transactions.exists(tx => tx._type == DataTransaction.typeId && tx.sender.get == publicKey) shouldBe true
        transactions.exists(tx => tx._type == TransferTransaction.typeId && tx.sender.get == publicKey) shouldBe true
        transactions.exists(tx => tx._type == CreateAliasTransaction.typeId && tx.sender.get == publicKey) shouldBe true
    }
  }

  private def sendTransactions(txSender: KeyPair) = {
    sender.transfer(txSender, thirdAddress, amount = 1, smartMinFee)
    sender.createAlias(txSender, s"alias${System.currentTimeMillis()}", smartMinFee)
    val data = List(StringDataEntry("key", "value"))
    sender.putData(txSender, data, calcDataFee(data, TxVersion.V1) + smartFee)
  }
}
