package com.wavesplatform.transaction.smart.script

import cats.implicits._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.StdLibVersion
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import com.wavesplatform.state.diffs._

class ScriptCompilerV1Test extends PropSpec with PropertyChecks with Matchers {

  property("compile script with specified version") {
    val script = scriptWithVersion("1".some)
    ScriptCompiler(script, isAssetScript = false) shouldBe Right((ExprScript(StdLibVersion.V1, expectedExpr).explicitGet(), 13))
  }

  property("use version 2 if not specified") {
    val script = scriptWithVersion(none)
    ScriptCompiler(script, isAssetScript = false) shouldBe Right((ExprScript(StdLibVersion.V2, expectedExpr).explicitGet(), 13))
  }

  property("fails on unsupported version") {
    val script = scriptWithVersion("8".some)
    ScriptCompiler(script, isAssetScript = false) shouldBe Left("Unsupported language version")
  }

  property("fails on incorrect version value") {
    val script = scriptWithVersion("oOooOps".some)
    ScriptCompiler(script, isAssetScript = false) shouldBe Left("Can't parse language version")
  }

  property("fails on incorrect content type value") {
    val script = scriptWithContentType("oOooOps".some)
    ScriptCompiler(script, isAssetScript = false) shouldBe Left("Wrong content type")
  }

  property("fails on incorrect script type value") {
    val script = scriptWithScriptType("oOooOps".some)
    ScriptCompiler.compile(script) shouldBe Left("Wrong script type")
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
