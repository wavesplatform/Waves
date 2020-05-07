package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.directives.values.{DApp => DAppType}
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.{ContractCompiler, ExpressionCompiler, Terms}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.PropSpec
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class SetScriptTransactionDiffTest extends PropSpec with PropertyChecks with TransactionGen with NoShrink with WithState {

  private val fs = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.Ride4DApps.id -> 0)
  )

  val preconditionsAndSetScript: Gen[(GenesisTransaction, SetScriptTransaction)] = for {
    master <- accountGen
    ts     <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
    fee    <- smallFeeGen
    script <- Gen.option(scriptGen)
  } yield (genesis, SetScriptTransaction.selfSigned(1.toByte, master, script, fee, ts).explicitGet())

  val preconditionsAndSetContract: Gen[(GenesisTransaction, SetScriptTransaction)] =
    preconditionsAndSetCustomContract(
      ContractScript(
        V3,
        DApp(
          DAppMeta(),
          List.empty,
          List(
            CallableFunction(CallableAnnotation("sender"), Terms.FUNC("foo", List("a"), FUNCTION_CALL(Native(203), List(REF("a"), REF("sender")))))
          ),
          None
        )
      ).explicitGet()
    )

  private def preconditionsAndSetCustomContract(script: Script): Gen[(GenesisTransaction, SetScriptTransaction)] =
    for {
      version <- Gen.oneOf(SetScriptTransaction.supportedVersions.toSeq)
      master  <- accountGen
      ts      <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      fee <- smallFeeGen
    } yield (genesis, SetScriptTransaction.selfSigned(1.toByte, master, Some(script), fee, ts).explicitGet())

  property("setting script results in account state") {
    forAll(preconditionsAndSetContract) {
      case (genesis, setScript) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript)), fs) {
          case (blockDiff, newState) =>
            newState.accountScript(setScript.sender.toAddress).map(_.script) shouldBe setScript.script
        }
    }
  }

  property("setting contract results in account state") {
    forAll(preconditionsAndSetContract) {
      case (genesis, setScript) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript)), fs) {
          case (blockDiff, newState) =>
            newState.accountScript(setScript.sender.toAddress).map(_.script) shouldBe setScript.script
        }
    }
  }

  property("Script with BlockV2 only works after Ride4DApps feature activation") {
    import com.wavesplatform.lagonaki.mocks.TestBlock.{create => block}

    val settingsUnactivated = TestFunctionalitySettings.Enabled.copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.Ride4DApps.id -> 3
      )
    )
    val settingsActivated = TestFunctionalitySettings.Enabled.copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.Ride4DApps.id -> 0
      )
    )
    val setup = for {
      master <- accountGen
      ts     <- positiveLongGen
      genesis = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      expr    = BLOCK(LET("x", CONST_LONG(3)), CONST_BOOLEAN(true))
      script  = ExprScript(V1, expr, checkSize = false).explicitGet()
      tx      = SetScriptTransaction.selfSigned(1.toByte, master, Some(script), 100000, ts + 1).explicitGet()
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

  property("verifier complexity limit 3000 from V4") {
    val exprV3WithComplexityBetween3000And4000 = {
      val ctx = {
        val directives = DirectiveSet(V3, Account, Expression).explicitGet()
        PureContext.build(Global, V3).withEnvironment[Environment] |+|
          CryptoContext.build(Global, V3).withEnvironment[Environment] |+|
          WavesContext.build(directives)
      }

      val script =
        """
        | {-#STDLIB_VERSION 3 #-}
        | {-#SCRIPT_TYPE ACCOUNT #-}
        | {-#CONTENT_TYPE EXPRESSION #-}
        |
        | rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') &&
        | rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') &&
        | rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') &&
        | rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') &&
        | rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') &&
        | rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') &&
        | rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') &&
        | rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') &&
        | rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') &&
        | rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK')
      """.stripMargin

      val expr = ExpressionCompiler.compile(script, ctx.compilerContext).explicitGet()
      ExprScript(V3, expr).explicitGet()
    }

    val contractV3WithComplexityBetween3000And4000 = {
      val ctx = {
        val directives = DirectiveSet(V3, Account, DAppType).explicitGet()
        PureContext.build(Global, V3).withEnvironment[Environment] |+|
          CryptoContext.build(Global, V3).withEnvironment[Environment] |+|
          WavesContext.build(directives)
      }

      val script =
        """
          | {-#STDLIB_VERSION 3 #-}
          | {-#SCRIPT_TYPE ACCOUNT #-}
          | {-#CONTENT_TYPE DAPP #-}
          |
          | @Verifier(tx)
          | func verify() =
          |   rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') &&
          |   rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') &&
          |   rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') &&
          |   rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') &&
          |   rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') &&
          |   rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') &&
          |   rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') &&
          |   rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') &&
          |   rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') &&
          |   rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK')
      """.stripMargin

      val dApp = ContractCompiler.compile(script, ctx.compilerContext, V3).explicitGet()
      ContractScript(V3, dApp).explicitGet()
    }

    val exprV4WithComplexityBetween3000And4000 = {
      val ctx = {
        val directives = DirectiveSet(V4, Account, Expression).explicitGet()
        PureContext.build(Global, V4).withEnvironment[Environment] |+|
          CryptoContext.build(Global, V4).withEnvironment[Environment] |+|
          WavesContext.build(directives)
      }

      val script =
        """
        | {-#STDLIB_VERSION 4 #-}
        | {-#SCRIPT_TYPE ACCOUNT #-}
        | {-#CONTENT_TYPE EXPRESSION #-}
        |
        | groth16Verify_15inputs(base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') || groth16Verify_1inputs(base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK')
      """.stripMargin

      val expr = ExpressionCompiler.compile(script, ctx.compilerContext).explicitGet()
      ExprScript(V4, expr).explicitGet()
    }

    val contractV4WithComplexityBetween3000And4000 = {
      val ctx = {
        val directives = DirectiveSet(V4, Account, DAppType).explicitGet()
        PureContext.build(Global, V4).withEnvironment[Environment] |+|
          CryptoContext.build(Global, V4).withEnvironment[Environment] |+|
          WavesContext.build(directives)
      }

      val script =
        """
        | {-#STDLIB_VERSION 4 #-}
        | {-#SCRIPT_TYPE ACCOUNT #-}
        | {-#CONTENT_TYPE DAPP #-}
        |
        | @Verifier(tx)
        | func verify() =
        |   groth16Verify_15inputs(base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') || groth16Verify_1inputs(base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK')
        |
      """.stripMargin

      val dApp = ContractCompiler.compile(script, ctx.compilerContext, V4).explicitGet()
      ContractScript(V4, dApp).explicitGet()
    }

    val contractV4WithCallableComplexityBetween3000And4000 = {
      val ctx = {
        val directives = DirectiveSet(V4, Account, DAppType).explicitGet()
        PureContext.build(Global, V4).withEnvironment[Environment] |+|
          CryptoContext.build(Global, V4).withEnvironment[Environment] |+|
          WavesContext.build(directives)
      }

      val script =
        """
        | {-#STDLIB_VERSION 4 #-}
        | {-#SCRIPT_TYPE ACCOUNT #-}
        | {-#CONTENT_TYPE DAPP #-}
        |
        | @Callable(i)
        | func default() = {
        |   if(groth16Verify_15inputs(base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK'))
        |     then [] else []
        | }
        |
      """.stripMargin

      val dApp = ContractCompiler.compile(script, ctx.compilerContext, V4).explicitGet()
      ContractScript(V4, dApp).explicitGet()
    }

    val rideV3Activated = TestFunctionalitySettings.Enabled.copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.Ride4DApps.id -> 0
      )
    )

    val rideV4Activated = TestFunctionalitySettings.Enabled.copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.Ride4DApps.id -> 0,
        BlockchainFeatures.BlockV5.id    -> 0
      )
    )

    def assertSuccess(script: Script, settings: FunctionalitySettings): Unit = {
      forAll(preconditionsAndSetCustomContract(script)) {
        case (genesis, setScript) =>
          assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript)), settings) {
            case (blockDiff, newState) =>
              newState.accountScript(setScript.sender.toAddress).map(_.script) shouldBe setScript.script
          }
      }
    }

    def assertFailure(script: Script, settings: FunctionalitySettings, errorMessage: String): Unit = {
      forAll(preconditionsAndSetCustomContract(script)) {
        case (genesis, setScript) =>
          assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript)), settings)(
            _ should produce(errorMessage)
          )
      }
    }

    forAll(preconditionsAndSetCustomContract(exprV4WithComplexityBetween3000And4000)) {
      case (genesis, setScript) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript)), rideV4Activated)(
          _ should produce("Script is too complex: 3807 > 3000")
        )
    }

    forAll(preconditionsAndSetCustomContract(contractV4WithComplexityBetween3000And4000)) {
      case (genesis, setScript) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript)), rideV4Activated)(
          _ should produce("Contract verifier is too complex: 3807 > 3000")
        )
    }

    assertSuccess(exprV3WithComplexityBetween3000And4000, rideV3Activated)
    assertSuccess(contractV3WithComplexityBetween3000And4000, rideV3Activated)

    assertFailure(exprV3WithComplexityBetween3000And4000, rideV4Activated, "Script is too complex: 3049 > 3000")
    assertFailure(contractV3WithComplexityBetween3000And4000, rideV4Activated, "Contract verifier is too complex: 3049 > 3000")

    assertSuccess(contractV4WithCallableComplexityBetween3000And4000, rideV4Activated)
  }
}
