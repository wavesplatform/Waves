package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{ARR, CONST_LONG, CONST_STRING, FUNCTION_CALL, REF}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.{ENOUGH_AMT, assertDiffAndState, assertDiffEi}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ListParamInvokeTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB with Inside {
  property("unactivated") {
    forAll(paymentPreconditions()) {
      case (genesis, setScript, invoke, dAppAddress) =>
        assertDiffAndState(Seq(TestBlock.create(genesis :+ setScript)), TestBlock.create(Seq(invoke)), fs) {
          case (_, blockchain) =>
            blockchain.accountData(dAppAddress, "entry1").get.value shouldBe "value1"
            blockchain.accountData(dAppAddress, "entry2").get.value shouldBe "value2"
        }
    }
  }
  property("pass list args") {
    forAll(paymentPreconditions()) {
      case (genesis, setScript, invoke, dAppAddress) =>
        assertDiffAndState(Seq(TestBlock.create(genesis :+ setScript)), TestBlock.create(Seq(invoke)), fs) {
          case (_, blockchain) =>
            blockchain.accountData(dAppAddress, "entry1").get.value shouldBe "value1"
            blockchain.accountData(dAppAddress, "entry2").get.value shouldBe "value2"
        }
    }
  }

  private val fs = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id -> 0,
      BlockchainFeatures.SmartAssets.id -> 0,
      BlockchainFeatures.Ride4DApps.id -> 0,
      BlockchainFeatures.FeeSponsorship.id -> 0,
      BlockchainFeatures.MultiPaymentInvokeScript.id -> 0,
    )
  )

  private def paymentPreconditions(): Gen[(List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, Address)] =
    for {
      master  <- accountGen
      invoker <- accountGen
      ts      <- timestampGen
      fee     <- ciFee(1)
    } yield {
      val functionCall =
        Some(
          FUNCTION_CALL(
            User("f"),
            List(ARR(IndexedSeq(
              CONST_STRING("value1").explicitGet(),
              CONST_STRING("value2").explicitGet()
            )))
          )
        )
      for {
        genesis  <- GenesisTransaction.create(master, ENOUGH_AMT, ts)
        genesis2 <- GenesisTransaction.create(invoker, ENOUGH_AMT, ts)
        setDApp  <- SetScriptTransaction.selfSigned(1.toByte, master, Some(dApp), fee, ts + 2)
        ci       <- InvokeScriptTransaction.selfSigned(1.toByte, invoker, master, functionCall, Nil, fee, Waves, ts + 3)
      } yield (List(genesis, genesis2), setDApp, ci, master.toAddress)
    }.explicitGet()

  private def dApp: Script = {
    val script =
      s"""
         | {-# STDLIB_VERSION 4       #-}
         | {-# CONTENT_TYPE   DAPP    #-}
         | {-# SCRIPT_TYPE    ACCOUNT #-}
         |
         | # func foldEntries(acc: List[StringEntry], elem: String) =
         | #   StringEntry("entry" + (acc.size() + 1).toString(), elem) :: acc
         | # FOLD<10>(args, nil, foldEntries)
         |
         | @Callable(i)
         | func f(args: List[String]) =
         |  [
         |    StringEntry("entry1", args[0]),
         |    StringEntry("entry2", args[1])
         |  ]
         |
       """.stripMargin

    val expr = Parser.parseContract(script).get.value
    val contract = compileContractFromExpr(expr, V4)
    ContractScript(V4, contract).explicitGet()
  }
}
