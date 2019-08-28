package com.wavesplatform.transaction.smart.script

import cats.implicits._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.state.diffs._
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ScriptCompilerV1Test extends PropSpec with PropertyChecks with Matchers with Inside {
  private val estimator = ScriptEstimatorV2
  
  property("compile script with specified version") {
    val script = scriptWithVersion("1".some)
    ScriptCompiler(script, isAssetScript = false, estimator) shouldBe Right((ExprScript(V1, expectedExpr).explicitGet(), 13))
  }

  property("use version 2 if not specified") {
    val script = scriptWithVersion(none)
    ScriptCompiler(script, isAssetScript = false, estimator) shouldBe Right((ExprScript(V2, expectedExpr).explicitGet(), 13))
  }

  property("fails on unsupported version") {
    val script = scriptWithVersion("8".some)
    ScriptCompiler(script, isAssetScript = false, estimator) shouldBe Left("Illegal directive value 8 for key STDLIB_VERSION")
  }

  property("fails on incorrect version value") {
    val script = scriptWithVersion("oOooOps".some)
    ScriptCompiler(script, isAssetScript = false, estimator) shouldBe Left("Illegal directive value oOooOps for key STDLIB_VERSION")
  }

  property("fails on incorrect content type value") {
    val script = scriptWithContentType("oOooOps".some)
    ScriptCompiler(script, isAssetScript = false, estimator) shouldBe Left("Illegal directive value oOooOps for key CONTENT_TYPE")
  }

  property("fails on incorrect script type value") {
    val script = scriptWithScriptType("oOooOps".some)
    ScriptCompiler.compile(script, estimator) shouldBe Left("Illegal directive value oOooOps for key SCRIPT_TYPE")
  }

  property("fails with right error position") {
    val script =
      """
        | {-# STDLIB_VERSION 3 #-}
        | let a = 1000
        | a > b
      """.stripMargin
    ScriptCompiler.compile(script, estimator) shouldBe Left("Compilation failed: A definition of 'b' is not found in 46-47")
  }

  property("fails with contract for asset") {
    val script =
      """
        | {-# STDLIB_VERSION 3 #-}
        | {-# CONTENT_TYPE DAPP #-}
        | {-# SCRIPT_TYPE ASSET #-}
      """.stripMargin
    ScriptCompiler.compile(script, estimator) should produce("Inconsistent set of directives")
  }

  property("fails with contract with wrong stdlib") {
    val script =
      """
        | {-# STDLIB_VERSION 2 #-}
        | {-# CONTENT_TYPE DAPP #-}
        | {-# SCRIPT_TYPE ACCOUNT #-}
      """.stripMargin
    ScriptCompiler.compile(script, estimator) should produce("Inconsistent set of directives")
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
        estimator
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
        estimator
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
        estimator
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
    ScriptCompiler.compile(script, estimator) shouldBe Right((ExprScript(V3, resultExpr).explicitGet(), 35))
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

    ScriptCompiler.compile(script, estimator) shouldBe 'left
  }

  property("complexity border") {
    def buildDirectives(
                         version:     StdLibVersion,
                         contentType: ContentType,
                         scriptType:  ScriptType
                       ): String =
    s"""
       | {-# STDLIB_VERSION ${version.value}     #-}
       | {-# CONTENT_TYPE   ${contentType.value} #-}
       | {-# SCRIPT_TYPE    ${scriptType.value}  #-}
     """.stripMargin

    def buildScript(
                     assigns:      Int,
                     conjunctions: Int,
                     withVerifier: Boolean
                   ): String =
      s"""
         | func script() = {
         |   let a0 = base58''
         |   ${1 to assigns map (i => s"let a$i = a${i - 1} + a0") mkString " "}
         |   a$assigns == base58'' ${"&& true " * conjunctions}
         | }
         |
         | ${if (withVerifier) "@Verifier(tx) func verify() = " else ""}
         | script()
      """.stripMargin

    def checkComplexityBorder(
                               version:     StdLibVersion,
                               contentType: ContentType,
                               scriptType:  ScriptType,
                               complexity:  Int
                             ): Unit = {

      val directives = buildDirectives(version, contentType, scriptType)
      val (assigns, conjunctions) = (version, contentType, scriptType) match {
        case (V3, DApp, Account) => (209, 2)
        case (V3, Expression, _) => (209, 7)
        case ( _, Expression, _) => (103, 14)
        case _ => ???
      }
      val withVerifier = contentType == DApp
      val validScript          = directives + buildScript(assigns, conjunctions,     withVerifier)
      val exceedingLimitScript = directives + buildScript(assigns, conjunctions + 1, withVerifier)

      inside(ScriptCompiler.compile(validScript, estimator)) { case Right((_, c)) => c shouldBe complexity }
      ScriptCompiler.compile(exceedingLimitScript, estimator) should produce(s"${complexity + 2} > $complexity")
    }

    checkComplexityBorder(V3, DApp,       Account, 4000)
    checkComplexityBorder(V3, Expression, Asset,   4000)
    checkComplexityBorder(V3, Expression, Account, 4000)
    checkComplexityBorder(V2, Expression, Asset,   2000)
    checkComplexityBorder(V2, Expression, Account, 2000)
    checkComplexityBorder(V1, Expression, Asset,   2000)
    checkComplexityBorder(V1, Expression, Account, 2000)
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

      val c1 = ScriptCompiler.compile(scriptWithoutTransactionById, estimator).explicitGet()._2
      val c2 = ScriptCompiler.compile(scriptWithTransactionById, estimator).explicitGet()._2
      c2 - c1
    }

    transactionByIdComplexity(2) shouldBe 100
  }

  property("library") {
    val script =
      """
        | {-# STDLIB_VERSION 3 #-}
        | {-# SCRIPT_TYPE ACCOUNT #-}
        | {-# CONTENT_TYPE LIBRARY #-}
        |
        | let a = 1
        | let b = 2
        |
        | func sq(a: Int) = a * a
      """.stripMargin

    ScriptCompiler.compile(script, estimator) shouldBe 'right
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
