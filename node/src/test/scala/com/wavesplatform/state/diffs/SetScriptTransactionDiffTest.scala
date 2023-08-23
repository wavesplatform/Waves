package com.wavesplatform.state.diffs

import com.google.common.primitives.Ints
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.*
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction, VerifierAnnotation, VerifierFunction}
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.utils.compilerContext
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.{ExpressionCompiler, Terms, TestCompiler}
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.lang.v1.parser.Parser.LibrariesOffset.NoLibraries
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.{GenesisTransaction, TxHelpers, TxVersion}
import monix.eval.Coeval
import org.scalatest.Assertion

import scala.util.Try

class SetScriptTransactionDiffTest extends PropSpec with WithDomain {

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
  private[this] def exactSizeContract(version: StdLibVersion, size: Int): ContractScriptImpl =
    new ContractScriptImpl(
      version,
      TxHelpers
        .script(s"""
                   |{-# STDLIB_VERSION ${version.id} #-}
                   |{-# CONTENT_TYPE DAPP #-}
                   |""".stripMargin)
        .asInstanceOf[ContractScriptImpl]
        .expr
    ) {
      override val bytes: Coeval[ByteStr] = Coeval.evalOnce(ByteStr(new Array[Byte](size)))
    }

  private[this] def exactSizeExpr(version: StdLibVersion, size: Int): ExprScript = new ExprScript {
    val stdLibVersion: StdLibVersion     = version
    val isFreeCall: Boolean              = false
    val expr: EXPR                       = TxHelpers.exprScript(V6)("true").expr
    val bytes: Coeval[ByteStr]           = Coeval(ByteStr(new Array[Byte](size)))
    val containsBlockV2: Coeval[Boolean] = Coeval(false)
    val containsArray: Boolean           = false
  }

  property("limit 32kb/8kb before V6") {
    withDomain(DomainPresets.RideV5) { d =>
      d.helpers.creditWavesToDefaultSigner()

      d.appendAndAssertSucceed(TxHelpers.setScript(TxHelpers.defaultSigner, exactSizeContract(V5, 32768), version = TxVersion.V2))
      d.appendAndCatchError(TxHelpers.setScript(TxHelpers.defaultSigner, exactSizeContract(V5, 32769), version = TxVersion.V2))
        .toString should include(
        "32769 bytes > 32768 bytes"
      )

      d.appendAndAssertSucceed(TxHelpers.setScript(TxHelpers.defaultSigner, exactSizeExpr(V5, 8192), version = TxVersion.V2))
      d.appendAndCatchError(TxHelpers.setScript(TxHelpers.defaultSigner, exactSizeExpr(V5, 8193), version = TxVersion.V2)).toString should include(
        "Script is too large: 8193 bytes > 8192 bytes"
      )
    }
  }

  property("limit 160kb/8kb after V6") {
    withDomain(DomainPresets.RideV6) { d =>
      d.helpers.creditWavesToDefaultSigner()

      val setScript160kb = TxHelpers.setScript(TxHelpers.defaultSigner, exactSizeContract(V6, 160 * 1024), 0.16.waves, version = TxVersion.V2)
      d.commonApi.calculateWavesFee(setScript160kb) shouldBe 0.16.waves
      d.appendAndAssertSucceed(setScript160kb)

      d.appendAndCatchError(TxHelpers.setScript(TxHelpers.defaultSigner, exactSizeContract(V6, 160 * 1024 + 1), 0.161.waves, version = TxVersion.V2))
        .toString should include(
        "Script is too large: 163841 bytes > 163840 bytes"
      )

      d.appendAndAssertSucceed(TxHelpers.setScript(TxHelpers.defaultSigner, exactSizeExpr(V6, 8 * 1024), 0.008.waves, version = TxVersion.V2))
      d.appendAndCatchError(TxHelpers.setScript(TxHelpers.defaultSigner, exactSizeExpr(V6, 8 * 1024 + 1), 0.009.waves, version = TxVersion.V2))
        .toString should include(
        "Script is too large: 8193 bytes > 8192 bytes"
      )
    }

    def byteVectorsList(size: Int): String =
      (1 to size).map(_ => s"base64'${ByteStr(new Array[Byte](1000)).base64Raw}'").mkString("[", ", ", "]")

    intercept[RuntimeException](TxHelpers.exprScript(V6)(s"""
                                                            |strict a = ${byteVectorsList(9)}
                                                            |true
                                                            |""".stripMargin)).toString should include(
      "Script is too large: 9140 bytes > 8192 bytes"
    )
  }

  val scriptSizes = Table(
    ("StdLibVersion", "scriptSize", "fee"),
    (V3, 1024, 0.001.waves),
    (V3, 1025, 0.002.waves),
    (V3, 32 * 1024, 0.032.waves),
    (V4, 1024, 0.001.waves),
    (V4, 1025, 0.002.waves),
    (V4, 32 * 1024, 0.032.waves),
    (V5, 1024, 0.001.waves),
    (V5, 1025, 0.002.waves),
    (V5, 32 * 1024, 0.032.waves),
    (V6, 1024, 0.001.waves),
    (V6, 1025, 0.002.waves),
    (V6, 32 * 1024, 0.032.waves),
    (V6, 160 * 1024, 0.16.waves)
  )

  property("lowered contract fee after V6") {
    withDomain(DomainPresets.RideV6) { d =>
      forAll(scriptSizes) { case (ver, size, fee) =>
        val script = exactSizeContract(ver, size)
        val sstx   = TxHelpers.setScript(TxHelpers.defaultSigner, script, version = TxVersion.V2, fee = fee - 1)
        d.appendBlockE(sstx) should produce("does not exceed minimal value")
        val setScriptTransaction = TxHelpers.setScript(TxHelpers.defaultSigner, script, version = TxVersion.V2, fee = fee)
        d.appendBlock(setScriptTransaction)
        d.commonApi.calculateWavesFee(setScriptTransaction) shouldBe fee
      }
    }
  }

  property("cannot use transaction constructors in V6") {
    val orderConstructor =
      "Order(base58'', base58'', AssetPair(base58'', base58''), Buy, 1, 1, 1, 1, 1, unit, Address(base58''), base58'', base58'', [])"

    val constructors = Seq(
      "GenesisTransaction(1, Address(base58''), base58'', 1, 1, 1)",
      "PaymentTransaction(1, Address(base58''), base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "TransferTransaction(unit, 1, unit, Alias(\"\"), base58'', base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "IssueTransaction(1, \"\", \"\", true, 1, unit, base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "ReissueTransaction(1, base58'', true, base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "BurnTransaction(1, base58'', base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "SetScriptTransaction(unit, base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "SponsorFeeTransaction(base58'', 5, base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "LeaseTransaction(1, Address(base58''), base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "LeaseCancelTransaction(base58'', base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "CreateAliasTransaction(\"\", base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      s"ExchangeTransaction($orderConstructor, $orderConstructor, 1, 1, 1, 1, base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "UpdateAssetInfoTransaction(base58'', \"\", \"\", base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "DataTransaction([], base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "MassTransferTransaction(base58'', 1, [], 1, base58'', base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "SetAssetScriptTransaction(unit, base58'', base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "InvokeScriptTransaction(Address(base58''), unit, \"\", [], base58'', 1, 1, 1, Address(base58''), base58'', base58'', [], [])"
    )

    for (constructor <- constructors) withClue("\\w+Transaction".r.findFirstIn(constructor).get) {
      val scriptText =
        s"""
           |@Callable(i)
           |func test() = {
           |  strict transfer = $constructor
           |  []
           |}""".stripMargin
      val scriptV5 = Try(TxHelpers.scriptV5(scriptText))
      scriptV5 shouldBe Symbol("success")

      val scriptV6 = scriptV5.get.copy(stdLibVersion = V6)

      intercept[RuntimeException](TxHelpers.scriptV6(scriptText)).toString should include("Can't find a function")

      withDomain(DomainPresets.RideV6) { d =>
        d.helpers.creditWavesToDefaultSigner()
        d.appendAndAssertSucceed(TxHelpers.setScript(TxHelpers.defaultSigner, scriptV5.get))
        d.appendAndCatchError(TxHelpers.setScript(TxHelpers.defaultSigner, scriptV6))
          .toString should include regex "function 'User\\(\\w+\\)' not found".r
      }
    }

    withClue("InvokeExpression") {
      intercept[RuntimeException](TxHelpers.scriptV6(s"""
                                                        |@Callable(i)
                                                        |func test() = {
                                                        |  strict tx = InvokeExpressionTransaction(base58'', unit, base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])
                                                        |  []
                                                        |}""".stripMargin)).toString should include("Can't find a function")
    }
  }

  property("setting script results in account state") {
    val (genesis, setScript) = preconditionsAndSetContract
    assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript))) { case (_, newState) =>
      newState.accountScript(setScript.sender.toAddress).map(_.script) shouldBe setScript.script
    }
  }

  property("setting contract results in account state") {
    val (genesis, setScript) = preconditionsAndSetContract
    assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript))) { case (_, newState) =>
      newState.accountScript(setScript.sender.toAddress).map(_.script) shouldBe setScript.script
    }
  }

  property("Script with BlockV2 only works after Ride4DApps feature activation") {
    import com.wavesplatform.lagonaki.mocks.TestBlock.create as block

    val settingsUnactivated = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures =
      Map(
        BlockchainFeatures.Ride4DApps.id -> 3
      )
    )
    val settingsActivated = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures =
      Map(
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

    val rideV3Activated = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures =
      Map(
        BlockchainFeatures.Ride4DApps.id -> 0
      )
    )

    val rideV4Activated = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures =
      Map(
        BlockchainFeatures.Ride4DApps.id -> 0,
        BlockchainFeatures.BlockV5.id    -> 0
      )
    )

    def assertSuccess(script: Script, settings: FunctionalitySettings): Unit = {
      val (genesis, setScript) = preconditionsAndSetCustomContract(script)
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript)), settings) { case (_, newState) =>
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

  property("free call is prohibited") {
    val freeCall = TestCompiler(V6).compileFreeCall("[]")
    val account  = accountGen.sample.get
    SetScriptTransaction.selfSigned(1.toByte, account, Some(freeCall), MinIssueFee, System.currentTimeMillis()) shouldBe Left(
      GenericError("Script type for Set Script Transaction should not be CALL")
    )
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
        .copy(estimationOverflowFixHeight = if (checkNegative) 0 else 999, estimatorSumOverflowFixHeight = if (checkSumOverflow) 0 else 999)
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
      DomainPresets.RideV5.copy(blockchainSettings =
        DomainPresets.RideV5.blockchainSettings.copy(
          functionalitySettings = DomainPresets.RideV5.blockchainSettings.functionalitySettings.copy(estimatorSumOverflowFixHeight = 3)
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

  property("unions are forbidden as @Callable arguments for RIDE 6 scripts and allowed for RIDE 4 and 5") {
    def checkForExpr(expr: String, version: StdLibVersion): Assertion = {
      val compileVersion = if (version == V6) V5 else version
      val script         = ContractScriptImpl(version, TestCompiler(compileVersion).compile(expr).explicitGet())

      val tx = SetScriptTransaction.selfSigned(
        TxVersion.V1,
        accountGen.sample.get,
        Some(script),
        100000000,
        1526287561757L
      )

      if (version == V6) {
        tx shouldBe Left(GenericError("Union type is not allowed in callable function arguments of script"))
      } else {
        tx.toOption shouldBe defined
      }
    }

    val exprWithPlainUnion =
      """
        |{-# STDLIB_VERSION 5 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |
        |@Callable(i)
        |func test(a: Int|String) = []
        |""".stripMargin

    val exprWithListUnion =
      """
        |{-# STDLIB_VERSION 5 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |
        |@Callable(i)
        |func test(a: List[Int|String]) = []
        |""".stripMargin

    checkForExpr(exprWithPlainUnion, V4)
    checkForExpr(exprWithListUnion, V4)

    checkForExpr(exprWithPlainUnion, V5)
    checkForExpr(exprWithListUnion, V5)

    checkForExpr(exprWithPlainUnion, V6)
    checkForExpr(exprWithListUnion, V6)
  }

  property("synchronous calls are not allowed in dApp verifier function") {
    def dAppVerifier(syncCall: String): String =
      s"""
         |@Verifier(tx)
         |func verify() = {
         |strict a = $syncCall(Address(base58'123'), "test", [], [])
         |true
         |}
         |""".stripMargin

    def dAppVerifierRec(syncCallId: Short): Script =
      ContractScript(
        V5,
        DApp(
          DAppMeta(),
          List(
            FUNC(
              name = "f",
              args = List.empty,
              body = BLOCK(
                LET("a", FUNCTION_CALL(FunctionHeader.Native(syncCallId), List.empty)),
                FUNCTION_CALL(FunctionHeader.User("f"), List.empty)
              )
            )
          ),
          List.empty,
          Some(VerifierFunction(VerifierAnnotation("tx"), FUNC("v", List.empty, FUNCTION_CALL(FunctionHeader.User("f"), List.empty))))
        )
      ).explicitGet()

    withDomain(DomainPresets.RideV5) { d =>
      val dApp     = accountGen.sample.get
      val ts: Long = System.currentTimeMillis()
      val fee      = 0.01.waves
      val genesis  = GenesisTransaction.create(dApp.toAddress, ENOUGH_AMT, ts).explicitGet()

      val scriptWithInvoke    = TestCompiler(V5).compileContract(dAppVerifier("invoke"))
      val setScriptWithInvoke = SetScriptTransaction.selfSigned(TxVersion.V2, dApp, Some(scriptWithInvoke), fee, ts).explicitGet()

      val scriptWithReentrantInvoke    = TestCompiler(V5).compileContract(dAppVerifier("reentrantInvoke"))
      val setScriptWithReentrantInvoke = SetScriptTransaction.selfSigned(TxVersion.V2, dApp, Some(scriptWithReentrantInvoke), fee, ts).explicitGet()

      val setScriptWithInvokeRec =
        SetScriptTransaction.selfSigned(TxVersion.V2, dApp, Some(dAppVerifierRec(FunctionIds.CALLDAPP)), fee, ts).explicitGet()
      val setScriptWithReentrantInvokeRec =
        SetScriptTransaction.selfSigned(TxVersion.V2, dApp, Some(dAppVerifierRec(FunctionIds.CALLDAPPREENTRANT)), fee, ts).explicitGet()

      d.appendBlock(genesis)
      d.appendBlockE(setScriptWithInvoke) should produce("DApp-to-dApp invocations are not allowed from verifier")
      d.appendBlockE(setScriptWithReentrantInvoke) should produce("DApp-to-dApp invocations are not allowed from verifier")
      d.appendBlockE(setScriptWithInvokeRec) should produce("DApp-to-dApp invocations are not allowed from verifier")
      d.appendBlockE(setScriptWithReentrantInvokeRec) should produce("DApp-to-dApp invocations are not allowed from verifier")
    }
  }

  property("synchronous calls are not allowed in account script") {
    def getScriptWithSyncCall(syncCall: String): ExprScript = {
      val expr =
        s"""
           |strict a = $syncCall(Address(base58'123'), "test", [], [])
           |true
           |""".stripMargin

      ExpressionCompiler
        .compileBoolean(expr, NoLibraries, compilerContext(DirectiveSet(V5, Call, Expression).explicitGet()))
        .flatMap(ExprScript(V5, _))
        .explicitGet()
    }

    withDomain(DomainPresets.RideV5) { d =>
      val smartAcc = accountGen.sample.get
      val ts: Long = System.currentTimeMillis()
      val genesis  = GenesisTransaction.create(smartAcc.toAddress, ENOUGH_AMT, ts).explicitGet()

      val setScriptWithInvoke =
        SetScriptTransaction.selfSigned(TxVersion.V2, smartAcc, Some(getScriptWithSyncCall("invoke")), 0.01.waves, ts).explicitGet()
      val setScriptWithReentrantInvoke =
        SetScriptTransaction.selfSigned(TxVersion.V2, smartAcc, Some(getScriptWithSyncCall("reentrantInvoke")), 0.01.waves, ts).explicitGet()

      d.appendBlock(genesis)
      d.appendBlockE(setScriptWithInvoke) should produce("function 'Native(1020)' not found")
      d.appendBlockE(setScriptWithReentrantInvoke) should produce("function 'Native(1021)' not found")
    }
  }
}
