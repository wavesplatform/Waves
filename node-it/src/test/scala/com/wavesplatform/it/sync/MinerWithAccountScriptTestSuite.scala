package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.{BaseFunSuite, NodeConfigs}
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms.CONST_STRING
import com.wavesplatform.lang.v1.compiler.TestCompiler

class MinerWithAccountScriptTestSuite extends BaseFunSuite {
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.preactivatedFeatures(Seq(BlockchainFeatures.RideV6.id.toInt -> 0)*))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

  val allowedRecipient: String = KeyPair("allowedRecipient".getBytes).toAddress('I').toString
  val dataKey                  = "testKey"

  test("scripted account are allowed to mine blocks after RideV6 feature activation") {
    Seq(
      (dAppScriptWithVerifier, true, true),
      (dAppScriptWithoutVerifier, true, false),
      (accountScript, false, true)
    ).foreach { case (script, hasCallable, hasVerifier) =>
      val setScript = notMiner.setScript(miner.keyPair, Some(script.bytes().base64))
      nodes.waitForHeightAriseAndTxPresent(setScript.id)
      if (hasCallable) {
        val (invokeByNotMiner, _) = notMiner.invokeScript(notMiner.keyPair, miner.address, Some("c"), List(CONST_STRING("notMiner").explicitGet()))
        nodes.waitForTransaction(invokeByNotMiner.id)
        notMiner.getDataByKey(miner.address, dataKey).value shouldBe "notMiner"

        val (invokeByMiner, _) = notMiner.invokeScript(notMiner.keyPair, miner.address, Some("c"), List(CONST_STRING("miner").explicitGet()))
        nodes.waitForTransaction(invokeByMiner.id)
        notMiner.getDataByKey(miner.address, dataKey).value shouldBe "miner"
      }

      val transfer       = notMiner.transfer(miner.keyPair, allowedRecipient, 1, fee = smartMinFee)
      val transferTxInfo = nodes.waitForTransaction(transfer.id)
      nodes.waitForHeightArise()

      if (hasVerifier) {
        assertApiError(
          notMiner.transfer(miner.keyPair, notMiner.address, 1, fee = smartMinFee)
        ) { e =>
          e.message shouldBe "Transaction is not allowed by account-script"
        }
      }

      notMiner.blockAt(transferTxInfo.height).generator shouldBe miner.address
      miner.blockAt(transferTxInfo.height).generator shouldBe miner.address
    }
  }

  private def verifierScriptStr: String =
    s"""
       |match tx {
       |    case t: TransferTransaction => t.recipient == Address(base58'$allowedRecipient')
       |    case _ => true
       |}
       |""".stripMargin

  private def callableFuncStr: String =
    s"""
       |@Callable(i)
       |func c(value: String) = {
       |  [StringEntry("$dataKey", value)]
       |}""".stripMargin

  private def accountScript: ExprScript =
    TestCompiler(V6).compileExpression(verifierScriptStr)

  private def dAppScriptWithVerifier: ContractScriptImpl = {
    val expr =
      s"""
         |$callableFuncStr
         |
         |@Verifier(tx)
         |func v() = {
         |  $verifierScriptStr
         |}
         |""".stripMargin
    TestCompiler(V6).compileContract(expr)
  }

  private def dAppScriptWithoutVerifier: ContractScriptImpl =
    TestCompiler(V6).compileContract(callableFuncStr)
}
