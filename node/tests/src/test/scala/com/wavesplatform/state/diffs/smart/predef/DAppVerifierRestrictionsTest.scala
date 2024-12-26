package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{VerifierAnnotation, VerifierFunction}
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{BLOCK, CONST_BOOLEAN, DECLARATION, EXPR, FUNC, FUNCTION_CALL, LET, REF}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.TxHelpers.{defaultSigner, setScript}
import org.scalatest.EitherValues

class DAppVerifierRestrictionsTest extends PropSpec with WithDomain with EitherValues {

  import DomainPresets.*

  private def verifierContract1(syncCall: String) = TestCompiler(V5).compileContract(
    s"""
       | {-# STDLIB_VERSION 5       #-}
       | {-# CONTENT_TYPE   DAPP    #-}
       | {-# SCRIPT_TYPE    ACCOUNT #-}
       |
       | @Verifier(tx)
       | func verifier() = {
       |   strict r = $syncCall(Address(base58''), "default", [], [])
       |   true
       | }
     """.stripMargin
  )

  private def verifierContract2(syncCall: String) = TestCompiler(V5).compileContract(
    s"""
       | {-# STDLIB_VERSION 5       #-}
       | {-# CONTENT_TYPE   DAPP    #-}
       | {-# SCRIPT_TYPE    ACCOUNT #-}
       |
       | func call() = {
       |   let foo = addressFromStringValue("").$syncCall("", [], [])
       |   foo
       | }
       |
       | @Verifier(tx)
       | func verifier() = call() == call()
     """.stripMargin
  )

  private def verifierContract3(syncCall: String) = TestCompiler(V5).compileContract(
    s"""
       | {-# STDLIB_VERSION 5       #-}
       | {-# CONTENT_TYPE   DAPP    #-}
       | {-# SCRIPT_TYPE    ACCOUNT #-}
       |
       | let x = {
       |   let foo = addressFromStringValue("").$syncCall("", [], [])
       |   foo
       | }
       |
       | @Verifier(tx)
       | func verifier() = x == x
     """.stripMargin
  )

  private def verifierContract4(syncCall: String) = TestCompiler(V5).compileContract(
    s"""
       | {-# STDLIB_VERSION 5       #-}
       | {-# CONTENT_TYPE   DAPP    #-}
       | {-# SCRIPT_TYPE    ACCOUNT #-}
       |
       | @Verifier(tx)
       | func verifier() = {
       |   let x = {
       |     let y = addressFromStringValue("").$syncCall("", [], [])
       |     y == y
       |   }
       |   x == x
       | }
     """.stripMargin
  )

  private def verifierContract5(syncCall: String) = TestCompiler(V5).compileContract(
    s"""
       | {-# STDLIB_VERSION 5       #-}
       | {-# CONTENT_TYPE   DAPP    #-}
       | {-# SCRIPT_TYPE    ACCOUNT #-}
       |
       | @Verifier(tx)
       | func verifier() = {
       |   func f() = {
       |     func g() = addressFromStringValue("").$syncCall("", [], [])
       |     g() == g()
       |   }
       |   f() == f()
       | }
     """.stripMargin
  )

  private def declarationsContract(syncCall: String, callFromCallable: Boolean) = TestCompiler(V5).compileContract(
    s"""
       | {-# STDLIB_VERSION 5       #-}
       | {-# CONTENT_TYPE   DAPP    #-}
       | {-# SCRIPT_TYPE    ACCOUNT #-}
       |
       | func call() = {
       |   let foo = addressFromStringValue("").$syncCall("", [], [])
       |   foo
       | }
       |
       | let x = {
       |   let foo = addressFromStringValue("").$syncCall("", [], [])
       |   foo
       | }
       |
       | @Callable(i)
       | func call1() = ${if (callFromCallable) "if (call() == call()) then [] else []" else "[]"}
       |
       | @Callable(i)
       | func call2() = if (x == x) then [] else []
       |
       | @Verifier(tx)
       | func verify() = sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
       |
     """.stripMargin
  )

  private def scenario(contract: String => Script) = {
    val account1 = TxHelpers.signer(1)
    val account2 = TxHelpers.signer(2)

    val balances = AddrWithBalance.enoughBalances(account1, account2)

    val setInvoke          = TxHelpers.setScript(account1, contract("invoke"))
    val setReentrantInvoke = TxHelpers.setScript(account2, contract("reentrantInvoke"))

    (balances, setInvoke, setReentrantInvoke)
  }

  property("sync calls are prohibited from dApp verifier both before and after fix") {
    for {
      contract <- List(verifierContract1 _, verifierContract2 _, verifierContract3 _, verifierContract4 _, verifierContract5 _)
      features <- List(RideV5, RideV6)
    } {
      val (balances, setInvoke, setReentrantInvoke) = scenario(contract)
      withDomain(features, balances) { d =>
        (the[RuntimeException] thrownBy d.appendBlock(setInvoke)).getMessage should include(
          s"DApp-to-dApp invocations are not allowed from verifier"
        )
        (the[RuntimeException] thrownBy d.appendBlock(setReentrantInvoke)).getMessage should include(
          s"DApp-to-dApp invocations are not allowed from verifier"
        )
      }
    }
  }

  property("sync calls are prohibited from declarations before fix") {
    List(true, false).foreach { callFromCallable =>
      val (balances, setInvoke, setReentrantInvoke) = scenario(declarationsContract(_, callFromCallable))
      withDomain(RideV5, balances) { d =>
        (the[RuntimeException] thrownBy d.appendBlock(setInvoke)).getMessage should include(s"DApp-to-dApp invocations are not allowed from verifier")
        (the[RuntimeException] thrownBy d.appendBlock(setReentrantInvoke)).getMessage should include(
          s"DApp-to-dApp invocations are not allowed from verifier"
        )
      }
    }
  }

  property("sync calls are NOT prohibited from declarations after fix") {
    List(true, false).foreach { callFromCallable =>
      val (balances, setInvoke, setReentrantInvoke) = scenario(declarationsContract(_, callFromCallable))
      withDomain(RideV6, balances) { d =>
        d.appendBlock(setInvoke)
        d.appendBlock(setReentrantInvoke)
        d.blockchain.transactionSucceeded(setInvoke.id.value()) shouldBe true
        d.blockchain.transactionSucceeded(setReentrantInvoke.id.value()) shouldBe true
      }
    }
  }

  property("dApp verifier estimation don't stuck on special cases") {
    def createScript(decs: List[DECLARATION], verifierBody: EXPR): Script =
      ContractScript(
        V5,
        DApp(
          DAppMeta(),
          decs,
          List.empty,
          Some(VerifierFunction(VerifierAnnotation("tx"), FUNC("v", List.empty, verifierBody)))
        )
      ).explicitGet()

    def estimateScript(script: Script): Option[Long] = {
      val estimator = ScriptEstimatorV3.latest

      Script.estimate(script, estimator, fixEstimateOfVerifier = false, useContractVerifierLimit = false).toOption
    }

    val script1 = createScript(
      List(LET("a1", CONST_BOOLEAN(true)), LET("a1", REF("a1"))),
      REF("a1")
    )
    val script2 = createScript(
      List(LET("a1", REF("a1"))),
      REF("a1")
    )
    val script3 = createScript(
      List(LET("a1", BLOCK(LET("a1", CONST_BOOLEAN(true)), REF("a1")))),
      REF("a1")
    )
    val script4 = createScript(
      List(LET("a1", BLOCK(FUNC("f", List("a1"), REF("a1")), CONST_BOOLEAN(true)))),
      REF("a1")
    )
    val script5 = createScript(
      List(FUNC("f", List.empty, BLOCK(FUNC("f", List.empty, CONST_BOOLEAN(true)), FUNCTION_CALL(FunctionHeader.User("f"), List.empty)))),
      FUNCTION_CALL(FunctionHeader.User("f"), List.empty)
    )

    estimateScript(script1) shouldBe defined
    estimateScript(script2) shouldBe defined
    estimateScript(script3) shouldBe defined
    estimateScript(script4) shouldBe defined
    estimateScript(script5) shouldBe defined
  }

  property("An old buggy check that was in effect before RideV6 should not cause an error if a variable is not used in a @Callable") {
    val script = TestCompiler(V5).compileContract(
      """
        | func call() = {
        |   let asset = invoke(addressFromStringValue(""), "", nil, nil)
        |   nil
        | }
        |
        | @Verifier(tx)
        | func verify() = sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
      """.stripMargin
    )
    withDomain(RideV5, AddrWithBalance.enoughBalances(defaultSigner))(_.appendAndAssertSucceed(setScript(defaultSigner, script)))
  }
}
