package com.wavesplatform.state.diffs

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.Version
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract.{CallableAnnotation, ContractFunction}
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{FUNCTION_CALL, REF}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.v1.ScriptV2

class SetScriptTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  private val fs = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.Ride4DApps.id -> 0))

  val preconditionsAndSetScript: Gen[(GenesisTransaction, SetScriptTransaction)] = for {
    version <- Gen.oneOf(SetScriptTransaction.supportedVersions.toSeq)
    master  <- accountGen
    ts      <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    fee    <- smallFeeGen
    script <- Gen.option(scriptGen)
  } yield (genesis, SetScriptTransaction.selfSigned(version, master, script, fee, ts).explicitGet())

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
    script = ScriptV2(
      Version.V3,
      Contract(
        List.empty,
        List(
          ContractFunction(CallableAnnotation("sender"),
                           None,
                           Terms.FUNC("foo", List("a"), FUNCTION_CALL(Native(203), List(REF("a"), REF("sender")))))),
        None
      )
    )
  } yield (genesis, SetScriptTransaction.selfSigned(version, master, Some(script), fee, ts).explicitGet())

  property("setting contract results in account state") {
    forAll(preconditionsAndSetContract) {
      case (genesis, setScript) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript)), fs) {
          case (blockDiff, newState) =>
            newState.accountScript(setScript.sender) shouldBe setScript.script
        }
    }
  }
}
