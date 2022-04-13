package com.wavesplatform.state.diffs

import com.google.common.primitives.Ints
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures._
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.{Terms, TestCompiler}
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.test.{PropSpec, _}
import com.wavesplatform.transaction.{GenesisTransaction, TxHelpers, TxVersion}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import org.scalatest.Assertion

class SetScriptTransactionDiffTest extends PropSpec with WithDomain {

  private val fs = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.Ride4DApps.id -> 0)
  )

  val preconditionsAndSetContract: (GenesisTransaction, SetScriptTransaction) =
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

  private def preconditionsAndSetCustomContract(script: Script): (GenesisTransaction, SetScriptTransaction) = {
    val master = TxHelpers.signer(1)

    val genesis   = TxHelpers.genesis(master.toAddress)
    val setScript = TxHelpers.setScript(master, script)

    (genesis, setScript)
  }

  property("setting script results in account state") {
    val (genesis, setScript) = preconditionsAndSetContract
    assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript)), fs) {
      case (_, newState) =>
        newState.accountScript(setScript.sender.toAddress).map(_.script) shouldBe setScript.script
    }
  }

  property("setting contract results in account state") {
    val (genesis, setScript) = preconditionsAndSetContract
    assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript)), fs) {
      case (_, newState) =>
        newState.accountScript(setScript.sender.toAddress).map(_.script) shouldBe setScript.script
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
    val setup = {
      val master = TxHelpers.signer(1)

      val genesis = TxHelpers.genesis(master.toAddress)
      val expr    = BLOCK(LET("x", CONST_LONG(3)), CONST_BOOLEAN(true))
      val script  = ExprScript(V1, expr, checkSize = false).explicitGet()
      val tx      = TxHelpers.setScript(master, script)

      (genesis, tx)
    }

    val (genesis, tx) = setup
    assertDiffEi(Seq(block(Seq(genesis))), block(Seq(tx)), settingsUnactivated) { blockDiffEi =>
      blockDiffEi should produce("RIDE 4 DAPPS feature has not been activated yet")
    }

    assertDiffEi(Seq(block(Seq(genesis))), block(Seq(tx)), settingsActivated) { blockDiffEi =>
      blockDiffEi.explicitGet()
    }
  }

  property("verifier complexity limit 3000 from V4") {
    val exprV3WithComplexityBetween2000And3000 =
      TestCompiler(V3).compileExpression(
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
          | rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK')
        """.stripMargin
      )

    val contractV3WithComplexityBetween2000And3000 = {
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
          |   rsaVerify(SHA256, base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK')
      """.stripMargin

      TestCompiler(V3).compileContract(script)
    }

    val exprV4WithComplexityBetween2000And3000 =
      TestCompiler(V4).compileExpression(
        """
          | {-#STDLIB_VERSION 4 #-}
          | {-#SCRIPT_TYPE ACCOUNT #-}
          | {-#CONTENT_TYPE EXPRESSION #-}
          |
          | groth16Verify_5inputs(base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') || groth16Verify_1inputs(base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK')
        """.stripMargin
      )

    val contractV4WithComplexityBetween2000And3000 = {
      val script =
        """
        | {-#STDLIB_VERSION 4 #-}
        | {-#SCRIPT_TYPE ACCOUNT #-}
        | {-#CONTENT_TYPE DAPP #-}
        |
        | @Verifier(tx)
        | func verify() =
        |   groth16Verify_5inputs(base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK') || groth16Verify_1inputs(base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK')
        |
      """.stripMargin

      TestCompiler(V4).compileContract(script)
    }

    val contractV4WithCallableComplexityBetween3000And4000 = {
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

      TestCompiler(V4).compileContract(script)
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
      val (genesis, setScript) = preconditionsAndSetCustomContract(script)
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript)), settings) {
        case (_, newState) =>
          newState.accountScript(setScript.sender.toAddress).map(_.script) shouldBe setScript.script
      }
    }

    def assertFailure(script: Script, settings: FunctionalitySettings, errorMessage: String): Unit = {
      val (genesis, setScript) = preconditionsAndSetCustomContract(script)
      assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript)), settings)(
        _ should produce(errorMessage)
      )
    }

    assertSuccess(exprV3WithComplexityBetween2000And3000, rideV3Activated)
    assertSuccess(contractV3WithComplexityBetween2000And3000, rideV3Activated)

    assertFailure(exprV3WithComplexityBetween2000And3000, rideV4Activated, "Script is too complex: 2134 > 2000")
    assertFailure(exprV4WithComplexityBetween2000And3000, rideV4Activated, "Script is too complex: 2807 > 2000")
    assertFailure(contractV3WithComplexityBetween2000And3000, rideV4Activated, "Contract verifier is too complex: 2134 > 2000")
    assertFailure(contractV4WithComplexityBetween2000And3000, rideV4Activated, "Contract verifier is too complex: 2807 > 2000")

    assertSuccess(contractV4WithCallableComplexityBetween3000And4000, rideV4Activated)
  }

  property("estimation overflow") {
    val body = {
      val n = 65
      s"""
         | func f0() = true
         | ${(0 until n).map(i => s"func f${i + 1}() = if (f$i()) then f$i() else f$i()").mkString("\n")}
         | f$n()
       """.stripMargin
    }

    val verifier = TestCompiler(V3).compileExpression(body)

    // due to complexity of natural callable with the expression is not negative
    val callable     = CallableFunction(CallableAnnotation("i"), FUNC("call", Nil, verifier.expr.asInstanceOf[EXPR]))
    val dAppCallable = ContractScriptImpl(V4, DApp(DAppMeta(), Nil, List(callable), None))

    val dAppVerifier = TestCompiler(V3).compileContract(
      s"""
         | @Verifier(tx)
         | func verify() = {
         |   $body
         | }
       """.stripMargin
    )

    val sender   = TxHelpers.signer(1)
    val balances = AddrWithBalance.enoughBalances(sender)

    def settings(checkNegative: Boolean = false, checkSumOverflow: Boolean = false): FunctionalitySettings = {
      TestFunctionalitySettings
        .withFeatures(BlockV5)
        .copy(
          estimationOverflowFixHeight = if (checkNegative) 0 else 999,
          estimatorSumOverflowFixHeight = if (checkSumOverflow) 0 else 999
        )
    }

    def assert(script: Script, checkNegativeMessage: String): Assertion = {
      def setScript() = TxHelpers.setScript(sender, script)

      withDomain(domainSettingsWithFS(settings()), balances) { db =>
        val tx = setScript()
        db.appendBlock(tx)
        db.liquidDiff.errorMessage(tx.id()) shouldBe None
      }

      withDomain(domainSettingsWithFS(settings(checkNegative = true)), balances) { db =>
        db.appendBlockE(setScript()) should produce(checkNegativeMessage)
      }

      withDomain(domainSettingsWithFS(settings(checkSumOverflow = true)), balances) { db =>
        db.appendBlockE(setScript()) should produce("Illegal script")
      }
    }

    Seq(
      (verifier, "Unexpected negative verifier complexity"),
      (dAppVerifier, "Unexpected negative verifier complexity"),
      (dAppCallable, "Unexpected negative callable `call` complexity")
    ).foreach { case (script, message) => assert(script, message) }
  }

  property("illegal recursion in scripts is allowed before sumOverflow height") {
    /*
      func a1() = true

      @Verifier(tx)
      func a1() = a1()
     */
    val verifier = "AAIFAAAAAAAAAA0IAhoJCgJhMRIDYTExAAAAAQEAAAACYTEAAAAABgAAAAAAAAABAAAAAnR4AQAAAAJhMQAAAAAJAQAAAAJhMQAAAAA1A+Ee"

    /*
      func a1() = true
      func a1() = a1()

      @Verifier(tx)
      func a2() = a1()
     */
    val userFunctions =
      "AAIFAAAAAAAAAA0IAhoJCgJhMRIDYTExAAAAAgEAAAACYTEAAAAABgEAAAACYTEAAAAACQEAAAACYTEAAAAAAAAAAAAAAAEAAAACdHgBAAAAAmEyAAAAAAkBAAAAAmExAAAAAIGVAL4="

    /*
      func a1() = true
      func a2() = {
        func a3() = {
          func a11() = a1()
          a11()
        }

        a3()
      }

      @Verifier(tx)
      func a4() = a2()
     */
    val innerOverlapWithVerifier =
      "AAIFAAAAAAAAAA0IAhoJCgJhMRIDYTExAAAAAgEAAAACYTEAAAAABgEAAAACYTIAAAAACgEAAAACYTMAAAAACgEAAAACYTEAAAAACQEAAAACYTEAAAAACQEAAAACYTEAAAAACQEAAAACYTMAAAAAAAAAAAAAAAEAAAACdHgBAAAAAmE0AAAAAAkBAAAAAmEyAAAAAEjFcsE="

    /*
      func a1() = true
      func a2() = {
        func a3() = {
          func a11() = a1()
          a11()
        }

        a3()
      }

      @Callable(i)
      func a4() = {
        strict a0 = a2()
        []
      }
     */
    val innerOverlapWithCallable =
      "AAIFAAAAAAAAAA8IAhIAGgkKAmExEgNhMTEAAAACAQAAAAJhMQAAAAAGAQAAAAJhMgAAAAAKAQAAAAJhMwAAAAAKAQAAAAJhMQAAAAAJAQAAAAJhMQAAAAAJAQAAAAJhMQAAAAAJAQAAAAJhMwAAAAAAAAABAAAAAWkBAAAAAmE0AAAAAAQAAAACYTAJAQAAAAJhMgAAAAADCQAAAAAAAAIFAAAAAmEwBQAAAAJhMAUAAAADbmlsCQAAAgAAAAECAAAAJFN0cmljdCB2YWx1ZSBpcyBub3QgZXF1YWwgdG8gaXRzZWxmLgAAAABEHCSy"
    val keyPairs = Vector.tabulate(8)(i => KeyPair(Ints.toByteArray(i)))
    val balances = keyPairs.map(acc => AddrWithBalance(acc.toAddress, 10.waves))

    def setScript(keyPairIndex: Int, script: String): SetScriptTransaction =
      TxHelpers.setScript(keyPairs(keyPairIndex), Script.fromBase64String(script).explicitGet(), version = TxVersion.V2)

    val settings =
      DomainPresets.RideV5.copy(
        blockchainSettings = DomainPresets.RideV5.blockchainSettings.copy(
          functionalitySettings = DomainPresets.RideV5.blockchainSettings.functionalitySettings.copy(
            estimatorSumOverflowFixHeight = 3
          )
        )
      )

    withDomain(settings, balances) { d =>
      d.appendBlock(
        setScript(0, verifier),
        setScript(1, userFunctions),
        setScript(2, innerOverlapWithVerifier),
        setScript(3, innerOverlapWithCallable)
      )

      d.appendBlockE(setScript(4, verifier)) should produce("shadows preceding declaration")
      d.appendBlockE(setScript(5, userFunctions)) should produce("shadows preceding declaration")
      d.appendBlockE(setScript(6, innerOverlapWithVerifier)) should produce("shadows preceding declaration")
      d.appendBlockE(setScript(7, innerOverlapWithCallable)) should produce("shadows preceding declaration")
    }
  }
}
