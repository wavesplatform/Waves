package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.TestTime
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.test._
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import org.scalatest.EitherValues

class DAppVerifierRestrictionsTest extends PropSpec with WithDomain with EitherValues {

  import DomainPresets._

  private val time = new TestTime
  private def ts   = time.getTimestamp()

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

  private def scenario(contract: String => Script) =
    for {
      account1 <- accountGen
      account2 <- accountGen
      fee      <- ciFee()
      genesis1           = GenesisTransaction.create(account1.toAddress, ENOUGH_AMT, ts).explicitGet()
      genesis2           = GenesisTransaction.create(account2.toAddress, ENOUGH_AMT, ts).explicitGet()
      setInvoke          = SetScriptTransaction.selfSigned(1.toByte, account1, Some(contract("invoke")), fee, ts).explicitGet()
      setReentrantInvoke = SetScriptTransaction.selfSigned(1.toByte, account2, Some(contract("reentrantInvoke")), fee, ts).explicitGet()
    } yield (List(genesis1, genesis2), setInvoke, setReentrantInvoke)

  property("sync calls are prohibited from dApp verifier both before and after fix") {
    for {
      contract <- List(verifierContract1 _, verifierContract2 _, verifierContract3 _, verifierContract4 _, verifierContract5 _)
      features <- List(RideV5, RideV6)
    } {
      val (genesis, setInvoke, setReentrantInvoke) = scenario(contract).sample.get
      withDomain(features) { d =>
        d.appendBlock(genesis: _*)
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
      val (genesis, setInvoke, setReentrantInvoke) = scenario(declarationsContract(_, callFromCallable)).sample.get
      withDomain(RideV5) { d =>
        d.appendBlock(genesis: _*)
        (the[RuntimeException] thrownBy d.appendBlock(setInvoke)).getMessage should include(s"DApp-to-dApp invocations are not allowed from verifier")
        (the[RuntimeException] thrownBy d.appendBlock(setReentrantInvoke)).getMessage should include(
          s"DApp-to-dApp invocations are not allowed from verifier"
        )
      }
    }
  }

  property("sync calls are NOT prohibited from declarations after fix") {
    List(true, false).foreach { callFromCallable =>
      val (genesis, setInvoke, setReentrantInvoke) = scenario(declarationsContract(_, callFromCallable)).sample.get
      withDomain(RideV6) { d =>
        d.appendBlock(genesis: _*)
        d.appendBlock(setInvoke)
        d.appendBlock(setReentrantInvoke)
        d.blockchain.transactionMeta(setInvoke.id.value()).get._2 shouldBe true
        d.blockchain.transactionMeta(setReentrantInvoke.id.value()).get._2 shouldBe true
      }
    }
  }
}
