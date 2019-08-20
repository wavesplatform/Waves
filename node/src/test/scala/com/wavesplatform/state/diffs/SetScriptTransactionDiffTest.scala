package com.wavesplatform.state.diffs

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class SetScriptTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  private val fs = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.Ride4DApps.id -> 0))

  val preconditionsAndSetScript: Gen[(GenesisTransaction, SetScriptTransaction)] = for {
    master  <- accountGen
    ts      <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    fee    <- smallFeeGen
    script <- Gen.option(scriptGen)
  } yield (genesis, SetScriptTransaction.selfSigned(master, script, fee, ts).explicitGet())

  property("setting script results in account state") {
    forAll(preconditionsAndSetContract) {
      case (genesis, setScript) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript)), fs) {
          case (blockDiff, newState) =>
            newState.accountScript(setScript.sender) shouldBe setScript.script
        }
    }
  }

  val preconditionsAndSetContract: Gen[(GenesisTransaction, SetScriptTransaction)] = for {
    version <- Gen.oneOf(SetScriptTransaction.supportedVersions.toSeq)
    master  <- accountGen
    ts      <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    fee <- smallFeeGen
    script = ContractScript(
      V3,
      DApp(
        DAppMeta(),
        List.empty,
        List(CallableFunction(CallableAnnotation("sender"), Terms.FUNC("foo", List("a"), FUNCTION_CALL(Native(203), List(REF("a"), REF("sender")))))),
        None
      )
    )
  } yield (genesis, SetScriptTransaction.selfSigned(master, script.toOption, fee, ts).explicitGet())

  property("setting contract results in account state") {
    forAll(preconditionsAndSetContract) {
      case (genesis, setScript) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript)), fs) {
          case (blockDiff, newState) =>
            newState.accountScript(setScript.sender) shouldBe setScript.script
        }
    }
  }

  property("Script with BlockV2 only works after Ride4DApps feature activation") {
    import com.wavesplatform.lagonaki.mocks.TestBlock.{create => block}

    val settingsUnactivated = TestFunctionalitySettings.Enabled.copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.Ride4DApps.id -> 3
      ))
    val settingsActivated = TestFunctionalitySettings.Enabled.copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.Ride4DApps.id -> 0
      ))
    val setup = for {
      master <- accountGen
      ts     <- positiveLongGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      expr    = BLOCK(LET("x", CONST_LONG(3)), CONST_BOOLEAN(true))
      script  = ExprScript(V1, expr, checkSize = false).explicitGet()
      tx      = SetScriptTransaction.selfSigned(master, Some(script), 100000, ts + 1).explicitGet()
    } yield (genesis, tx)

    forAll(setup) {
      case (genesis, tx) =>
        assertDiffEi(Seq(block(Seq(genesis))), block(Seq(tx)), settingsUnactivated) { blockDiffEi =>
          blockDiffEi should produce("RIDE 4 DAPPS feature has not been activated yet")
        }

        assertDiffEi(Seq(block(Seq(genesis))), block(Seq(tx)), settingsActivated) { blockDiffEi =>
          blockDiffEi shouldBe 'right
        }
    }
  }
}
