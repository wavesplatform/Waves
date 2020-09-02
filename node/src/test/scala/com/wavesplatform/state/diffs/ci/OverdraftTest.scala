package com.wavesplatform.state.diffs.ci

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V4}
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.FeeValidation.FeeConstants
import com.wavesplatform.state.diffs.{ENOUGH_AMT, FeeValidation, produce}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class OverdraftTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithState with Inside {
  private val InvokeFee    = FeeConstants(InvokeScriptTransaction.typeId) * FeeValidation.FeeUnit
  private val SetScriptFee = FeeConstants(SetScriptTransaction.typeId) * FeeValidation.FeeUnit
  private val IssueFee     = FeeConstants(IssueTransaction.typeId) * FeeValidation.FeeUnit

  private val dAppVersionsWithActivation: List[(StdLibVersion, Boolean)] =
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V3)
      .map((_, true))
      .toList

  private val allDAppVersions: Gen[(StdLibVersion, Boolean)] =
    Gen.oneOf((V3, false) :: dAppVersionsWithActivation)

  property("insufficient fee") {
    forAll(
      for {
        (version, activation)     <- allDAppVersions
        (genesis, setDApp, ci, _) <- paymentPreconditions(withEnoughFee = false, withPayment = false, emptyResultDApp(version))
      } yield (genesis, setDApp, ci, activation)
    ) {
      case (genesis, setDApp, ci, activation) =>
        assertDiffEi(Seq(TestBlock.create(genesis :+ setDApp)), TestBlock.create(Seq(ci)), features(activation)) { r =>
          if (activation) r should produce("AccountBalanceError")
          else
            r should produce(
              s"Fee in WAVES for InvokeScriptTransaction (1 in WAVES) does not exceed minimal value of $InvokeFee WAVES"
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
        assertDiffEi(Seq(TestBlock.create(genesis :+ setDApp)), TestBlock.create(Seq(ci)), features(activation)) { r =>
          if (activation) r should produce("AccountBalanceError")
          else r.explicitGet()
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
        (version, activation)         <- Gen.oneOf(dAppVersionsWithActivation.filter(_._1 >= V4))
        (genesis, setDApp, ci, issue) <- paymentPreconditions(withEnoughFee = true, withPayment = true, payingDApp(version))
      } yield (genesis, setDApp, ci, issue, activation)
    ) {
      case (genesis, setDApp, ci, issue, activation) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ List(setDApp, issue))), TestBlock.create(Seq(ci)), features(activation)) {
          _ should produce("AccountBalanceError")
        }
    }
  }

  property("attach unexisting tokens using multiple payment") {
    forAll(
      for {
        (version, activation)         <- Gen.oneOf(dAppVersionsWithActivation)
        (genesis, setDApp, ci, issue) <- splitPaymentPreconditions(version)
      } yield (genesis, setDApp, ci, issue, activation)
    ) {
      case (genesis, setDApp, ci, issue, activation) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ List(setDApp, issue))), TestBlock.create(Seq(ci)), features(activation)) {
          _ should produce("Attempt to transfer unavailable funds: Transaction application leads to negative asset")
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
          (List(Payment(issue.quantity, IssuedAsset(issue.id()))), IssueFee)
        else
          (Nil, 0L)
      for {
        genesis  <- GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts)
        genesis2 <- GenesisTransaction.create(invoker.toAddress, invokerBalance, ts)
        setDApp  <- SetScriptTransaction.selfSigned(1.toByte, master, Some(dApp), SetScriptFee, ts + 2)
        ci       <- InvokeScriptTransaction.selfSigned(1.toByte, invoker, master.toAddress, None, payment, fee, Waves, ts + 3)
      } yield (List(genesis, genesis2), setDApp, ci, issue)
    }.explicitGet()

  private def splitPaymentPreconditions(
      version: StdLibVersion
  ): Gen[(List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, IssueTransaction)] =
    for {
      master  <- accountGen
      invoker <- accountGen
      ts      <- timestampGen
      issue   <- issueV2TransactionGen(invoker, Gen.const(None), feeParam = Some(IssueFee))
    } yield {
      val count    = ContractLimits.MaxAttachedPaymentAmount
      val payments = (1 to count).map(_ => Payment(issue.quantity / count + 1, IssuedAsset(issue.id())))
      for {
        genesis  <- GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts)
        genesis2 <- GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts)
        setDApp  <- SetScriptTransaction.selfSigned(1.toByte, master, Some(payingAssetDApp(version, issue.assetId)), SetScriptFee, ts + 2)
        ci       <- InvokeScriptTransaction.selfSigned(1.toByte, invoker, master.toAddress, None, payments, InvokeFee, Waves, ts + 3)
      } yield (List(genesis, genesis2), setDApp, ci, issue)
    }.explicitGet()

  private def emptyResultDApp(version: StdLibVersion): Script = {
    val body = if (version >= V4) "[]" else "WriteSet([])"
    dApp(body, version)
  }

  private def payingDApp(version: StdLibVersion): Script = {
    val transfer = s"ScriptTransfer(i.caller, $InvokeFee, unit)"
    val body     = if (version >= V4) s"[$transfer]" else s"TransferSet([$transfer])"
    dApp(body, version)
  }

  private def payingAssetDApp(version: StdLibVersion, assetId: ByteStr): Script = {
    val transfer = s"ScriptTransfer(i.caller, $InvokeFee, base58'${assetId.toString}')"
    val body     = if (version >= V4) s"[$transfer]" else s"TransferSet([$transfer])"
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

    val expr     = Parser.parseContract(script).get.value
    val contract = compileContractFromExpr(expr, version)
    ContractScript(version, contract).explicitGet()
  }

  private def features(withV4: Boolean) = {
    val v4ForkO = if (withV4) Seq(BlockchainFeatures.BlockV5) else Seq()
    val parameters = Seq(
      BlockchainFeatures.SmartAccounts,
      BlockchainFeatures.SmartAssets,
      BlockchainFeatures.Ride4DApps
    ) ++ v4ForkO
    TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = parameters.map(_.id -> 0).toMap)
  }
}
