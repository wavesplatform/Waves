package com.wavesplatform.transaction.smart.script

import cats.implicits._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.state.diffs._
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ScriptCompilerV1Test extends PropSpec with PropertyChecks with Matchers with Inside {

  property("compile script with specified version") {
    val script = scriptWithVersion("1".some)
    ScriptCompiler(script, isAssetScript = false) shouldBe Right((ExprScript(V1, expectedExpr).explicitGet(), 13))
  }

  property("use version 2 if not specified") {
    val script = scriptWithVersion(none)
    ScriptCompiler(script, isAssetScript = false) shouldBe Right((ExprScript(V2, expectedExpr).explicitGet(), 13))
  }

  property("fails on unsupported version") {
    val script = scriptWithVersion("8".some)
    ScriptCompiler(script, isAssetScript = false) shouldBe Left("Illegal directive value 8 for key STDLIB_VERSION")
  }

  property("fails on incorrect version value") {
    val script = scriptWithVersion("oOooOps".some)
    ScriptCompiler(script, isAssetScript = false) shouldBe Left("Illegal directive value oOooOps for key STDLIB_VERSION")
  }

  property("fails on incorrect content type value") {
    val script = scriptWithContentType("oOooOps".some)
    ScriptCompiler(script, isAssetScript = false) shouldBe Left("Illegal directive value oOooOps for key CONTENT_TYPE")
  }

  property("fails on incorrect script type value") {
    val script = scriptWithScriptType("oOooOps".some)
    ScriptCompiler.compile(script) shouldBe Left("Illegal directive value oOooOps for key SCRIPT_TYPE")
  }

  property("fails with right error position") {
    val script =
      """
        | {-# STDLIB_VERSION 3 #-}
        | {-# STDLIB_VERSION 3 #-}
        | let a = 1000
        | a > b
      """.stripMargin
    ScriptCompiler.compile(script) shouldBe Left("Compilation failed: A definition of 'b' is not found in 72-73")
  }

  property("fails with contract for asset") {
    val script =
      """
        | {-# STDLIB_VERSION 3 #-}
        | {-# CONTENT_TYPE DAPP #-}
        | {-# SCRIPT_TYPE ASSET #-}
      """.stripMargin
    ScriptCompiler.compile(script) should produce("Inconsistent set of directives")
  }

  property("fails with contract with wrong stdlib") {
    val script =
      """
        | {-# STDLIB_VERSION 2 #-}
        | {-# CONTENT_TYPE DAPP #-}
        | {-# SCRIPT_TYPE ACCOUNT #-}
      """.stripMargin
    ScriptCompiler.compile(script) should produce("Inconsistent set of directives")
  }

  property("default V3 (+account+expression) contains `tx`") {
    ScriptCompiler
      .compile(
        s"""
           |
           |{-# STDLIB_VERSION 3 #-}
           |match tx {
           |  case tx:TransferTransaction => true
           |  case _ => false
           |}""".stripMargin,
      ) shouldBe 'right
  }

  property("account script with 'this' address link") {
    ScriptCompiler
      .compile(
        s"""
           |
           |{-# STDLIB_VERSION 3 #-}
           |
           |let a = this
           |
           |match tx {
           |  case tx:TransferTransaction => true
           |  case _ => false
           |}""".stripMargin,
      ) shouldBe 'right
  }

  property("asset script with 'this' address link") {
    ScriptCompiler
      .compile(
        s"""
           |
           |{-# STDLIB_VERSION 3 #-}
           |{-# SCRIPT_TYPE ASSET #-}
           |
           |let a = this
           |
           |match tx {
           |  case tx:TransferTransaction => true
           |  case _ => false
           |}""".stripMargin,
      ) shouldBe 'right
  }

  property("binary operations priority && ||") {
    val script =
      """
        | {-# STDLIB_VERSION 3 #-}
        | {-# CONTENT_TYPE EXPRESSION #-}
        | {-# SCRIPT_TYPE ACCOUNT #-}
        |
        | let a = true
        | let b = true
        | let c = false
        | let d = true
        |
        | a && b || c && d
      """.stripMargin

    val resultExpr = LET_BLOCK(
      LET("a", TRUE),
      LET_BLOCK(
        LET("b", TRUE),
        LET_BLOCK(
          LET("c", FALSE),
          LET_BLOCK(
            LET("d", TRUE),
            IF(
              IF(
                REF("a"),
                REF("b"),
                FALSE
              ),
              TRUE,
              IF(
                REF("c"),
                REF("d"),
                FALSE
              )
            )
          )
        )
      )
    )
    ScriptCompiler.compile(script) shouldBe Right((ExprScript(V3, resultExpr).explicitGet(), 35))
  }

  property("binary operations priority == > <") {
    val script =
      """
        | {-# STDLIB_VERSION 3 #-}
        | {-# CONTENT_TYPE EXPRESSION #-}
        | {-# SCRIPT_TYPE ACCOUNT #-}
        |
        | let a = 1
        | let b = 2
        |
        | a > b == true
      """.stripMargin

    ScriptCompiler.compile(script) shouldBe 'left
  }

  property("max dapp complexity") {
    def scriptWithHighComplexity(assigns: Int): String =
      s"""
         | {-# STDLIB_VERSION 3       #-}
         | {-# CONTENT_TYPE   DAPP    #-}
         | {-# SCRIPT_TYPE    ACCOUNT #-}
         |
         | @Verifier(tx)
         | func verify() = {
         |   let a0 = [base58'']
         |   ${1 to assigns map (i => s"let a$i = [a${i - 1}[0] + a0[1]]") mkString " "}
         |   let c0 = 1
         |   let c1 = c0 + c0
         |   let c2 = c0 + c0
         |   let c3 = c0 + c0
         |   a$assigns == a$assigns && true && true && true && true
         | }
         |
      """.stripMargin

    val count = 343

    inside(ScriptCompiler.compile(scriptWithHighComplexity(count))) {
      case Right((_, complexity)) => complexity shouldBe 10000
    }

    inside(ScriptCompiler.compile(scriptWithHighComplexity(count + 1))) {
      case Left(msg) => msg shouldBe "Contract function (verify) is too complex: 10029 > 10000"
    }
  }

  property("transactionByID complexity") {
    def transactionByIdComplexity(version: Int) = {
      val scriptWithoutTransactionById =
        s"""
          | {-# STDLIB_VERSION $version #-}
          |
          | let a = base64''
          | a == a
          |
      """.stripMargin

      val scriptWithTransactionById =
        s"""
          | {-# STDLIB_VERSION $version #-}
          |
          | let a = transactionById(base64'')
          | a == a
          |
      """.stripMargin

      val c1 = ScriptCompiler.compile(scriptWithoutTransactionById).explicitGet()._2
      val c2 = ScriptCompiler.compile(scriptWithTransactionById).explicitGet()._2
      c2 - c1
    }

    transactionByIdComplexity(2) shouldBe 100
  }

  private val expectedExpr = LET_BLOCK(
    LET("x", CONST_LONG(10)),
    FUNCTION_CALL(
      PureContext.eq.header,
      List(
        CONST_LONG(20),
        FUNCTION_CALL(
          FunctionHeader.Native(SUM_LONG),
          List(REF("x"), REF("x"))
        )
      )
    )
  )

  private def scriptWithVersion(versionStr: Option[String]): String = {
    val directive =
      versionStr
        .map(v => s"{-# STDLIB_VERSION $v #-}")
        .getOrElse("")

    s"""
       | $directive
       |
       | let x = 10
       | 20 == x + x
       |
      """.stripMargin
  }

  private def scriptWithContentType(contentTypeStr: Option[String]): String = {
    val directive =
      contentTypeStr
        .map(v => s"{-# CONTENT_TYPE $v #-}")
        .getOrElse("")

    s"""
       | $directive
       |
       | let x = 10
       | 20 == x + x
       |
      """.stripMargin
  }

  private def scriptWithScriptType(scriptTypeStr: Option[String]): String = {
    val directive =
      scriptTypeStr
        .map(v => s"{-# SCRIPT_TYPE $v #-}")
        .getOrElse("")

    s"""
       | $directive
       |
       | let x = 10
       | 20 == x + x
       |
      """.stripMargin
  }
}
