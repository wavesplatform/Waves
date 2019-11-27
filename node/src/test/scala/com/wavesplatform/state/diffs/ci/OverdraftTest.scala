package com.wavesplatform.state.diffs.ci

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.{ValidationError, directives}
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{StdLibVersion, V1, V2, V3, V4}
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BYTESTR
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader}
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.FeeValidation.FeeConstants
import com.wavesplatform.state.diffs.{ENOUGH_AMT, FeeValidation, assertDiffEi, produce}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.assets.{IssueTransaction, SponsorFeeTransaction}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.collection.immutable

class OverdraftTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB with Inside {
  private val InvokeFee    = FeeConstants(InvokeScriptTransaction.typeId) * FeeValidation.FeeUnit
  private val SetScriptFee = FeeConstants(SetScriptTransaction.typeId)    * FeeValidation.FeeUnit
  private val IssueFee     = FeeConstants(IssueTransaction.typeId)        * FeeValidation.FeeUnit

  private val dAppVersionsWithActivatedV4: List[(StdLibVersion, Boolean)] =
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V3)
      .map((_, true))
      .toList

  private val allDAppVersions: Gen[(StdLibVersion, Boolean)] =
    Gen.oneOf((V3, false) :: dAppVersionsWithActivatedV4)

  property("insufficient fee") {
    forAll(
      for {
        (version, activation)     <- allDAppVersions
        (genesis, setDApp, ci, _) <- paymentPreconditions(withEnoughFee = false, withPayment = false, emptyResultDApp(version))
      } yield (genesis, setDApp, ci, activation)
    ) {
      case (genesis, setDApp, ci, activation) =>
        assertDiffEi(Seq(TestBlock.create(genesis :+ setDApp)), TestBlock.create(Seq(ci)), features(activation)) {
          _ should produce(
            s"Fee in WAVES for InvokeScriptTransaction (1 in WAVES) with 0 total scripts invoked does not exceed minimal value of $InvokeFee WAVES"
          )
        }
    }
  }

  property("overdraft") {
    forAll(
      for {
        (version, activation)     <- allDAppVersions
        (genesis, setDApp, ci, _) <- paymentPreconditions(withEnoughFee = true, withPayment = false, payingDApp(version))
      } yield (genesis, setDApp, ci, activation)
    ) {
      case (genesis, setDApp, ci, activation) =>
        assertDiffEi(Seq(TestBlock.create(genesis :+ setDApp)), TestBlock.create(Seq(ci)), features(activation)) {
          _ shouldBe 'right
        }
    }
  }

  property("overdraft with payment V3") {
    forAll(
      for {
        activation                    <- Gen.oneOf(false, true)
        (genesis, setDApp, ci, issue) <- paymentPreconditions(withEnoughFee = true, withPayment = true, payingDApp(V3))
      } yield (genesis, setDApp, ci, activation, issue)
    ) {
      case (genesis, setDApp, ci, activation, issue) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ List(setDApp, issue))), TestBlock.create(Seq(ci)), features(activation)) {
          _ should produce("leads to negative waves balance to (at least) temporary negative state")
        }
    }
  }

  property("overdraft with payment V4") {
    forAll(
      for {
        (genesis, setDApp, ci, issue) <- paymentPreconditions(withEnoughFee = true, withPayment = true, payingDApp(V4))
      } yield (genesis, setDApp, ci, issue)
    ) {
      case (genesis, setDApp, ci, issue) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ List(setDApp, issue))), TestBlock.create(Seq(ci)), features(withV4 = true)) {
          _ shouldBe 'right
        }
    }
  }

  private def paymentPreconditions(
    withEnoughFee: Boolean,
    withPayment: Boolean,
    dApp: Script
  ): Gen[(List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, IssueTransaction)] =
    for {
      master  <- accountGen
      invoker <- accountGen
      ts      <- timestampGen
      issue   <- issueV2TransactionGen(invoker, Gen.const(None), feeParam = Some(IssueFee))
    } yield {
      val fee = if (withEnoughFee) InvokeFee else 1
      val (payment, invokerBalance) =
        if (withPayment)
          (List(Payment(issue.quantity, IssuedAsset(issue.id.value()))), IssueFee)
        else
          (Nil, 0L)
      for {
        genesis  <- GenesisTransaction.create(master, ENOUGH_AMT, ts)
        genesis2 <- GenesisTransaction.create(invoker, invokerBalance, ts)
        setDApp  <- SetScriptTransaction.selfSigned(1.toByte, master, Some(dApp), SetScriptFee, ts + 2)
        ci       <- InvokeScriptTransaction.selfSigned(1.toByte, invoker, master, None, payment, fee, Waves, ts + 3)
      } yield (List(genesis, genesis2), setDApp, ci, issue)
    }.explicitGet()

  private def emptyResultDApp(version: StdLibVersion): Script = {
    val body = if (version >= V4) "[]" else "WriteSet([])"
    dApp(body, version)
  }

  private def payingDApp(version: StdLibVersion): Script = {
    val transfer = s"ScriptTransfer(i.caller, $InvokeFee, unit)"
    val body = if (version >= V4) s"[$transfer]" else s"TransferSet([$transfer])"
    dApp(body, version)
  }

  private def dApp(body: String, version: StdLibVersion): Script = {
    val script =
      s"""
         | {-# STDLIB_VERSION $version #-}
         | {-# CONTENT_TYPE   DAPP     #-}
         | {-# SCRIPT_TYPE    ACCOUNT  #-}
         |
         | @Callable(i)
         | func default() = $body
         |
       """.stripMargin

    val expr = Parser.parseContract(script).get.value
    val contract = compileContractFromExpr(expr, version)
    ContractScript(version, contract).explicitGet()
  }

  private def features(withV4: Boolean) = {
    val v4ForkO = if (withV4) Seq(BlockchainFeatures.MultiPaymentInvokeScript) else Seq()
    val parameters = Seq(
      BlockchainFeatures.SmartAccounts,
      BlockchainFeatures.SmartAssets,
      BlockchainFeatures.Ride4DApps,
    ) ++ v4ForkO
    TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = parameters.map(_.id -> 0).toMap)
  }
}
