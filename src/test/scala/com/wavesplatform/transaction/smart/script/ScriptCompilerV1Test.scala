package com.wavesplatform.transaction.smart.script

import cats.implicits._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ScriptCompilerV1Test extends PropSpec with PropertyChecks with Matchers {

  property("compile script with specified version") {
    val script = scriptWithVersion("1".some)
    ScriptCompiler(script, isAssetScript = false) shouldBe Right((expectedScript, 13))
  }

  property("use version 1 if not specified") {
    val script = scriptWithVersion(none)
    ScriptCompiler(script, isAssetScript = false) shouldBe Right((expectedScript, 13))
  }

  property("fails on unsupported version") {
    val script = scriptWithVersion("8".some)
    ScriptCompiler(script, isAssetScript = false) shouldBe Left("Unsupported language version")
  }

  property("fails on incorrect version value") {
    val script = scriptWithVersion("oOooOps".some)
    ScriptCompiler(script, isAssetScript = false) shouldBe Left("Can't parse language version")
  }

  private val expectedExpr = BLOCK(
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

  private val expectedScript = ExprScript(expectedExpr).explicitGet()

  private def scriptWithVersion(versionStr: Option[String]): String = {
    val directive =
      versionStr
        .map(v => s"{-# LANGUAGE_VERSION $v #-}")
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
