package com.wavesplatform.lang.compiler

import cats.Id
import cats.implicits.*
import cats.kernel.Monoid
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.directives.{Directive, DirectiveParser}
import com.wavesplatform.lang.script.ScriptPreprocessor
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, EVALUATED}
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGenParser
import com.wavesplatform.test.*

class ScriptPreprocessorTest extends PropSpec with ScriptGenParser {
  private val evaluator = new EvaluatorV1[Id, NoContext]()

  private def processAndEval(src: String, libraries: Map[String, String]): Either[String, EVALUATED] =
    for {
      directives <- DirectiveParser(src)
      ds         <- Directive.extractDirectives(directives)
      linked     <- ScriptPreprocessor(src, libraries, ds.imports)
      r          <- eval(linked)
    } yield r

  private def eval(code: String): Either[String, EVALUATED] = {
    val untyped  = Parser.parseExpr(code).get.value
    val ctx: CTX[NoContext] = Monoid.combineAll(Seq(PureContext.build(V3, useNewPowPrecision = true)))
    val typed    = ExpressionCompiler(ctx.compilerContext, untyped)
    typed.flatMap(v => evaluator[EVALUATED](ctx.evaluationContext, v._1).leftMap(_.toString))
  }

  property("multiple libraries") {
    val script =
      """
        | {-# SCRIPT_TYPE ACCOUNT #-}
        | {-# IMPORT lib1,lib2,lib3 #-}
        | let a = 5
        | multiply(inc(a), dec(a)) == (5 + 1) * (5 - 1)
      """.stripMargin

    val libraries =
      Map(
        "lib1" ->
          """
            | {-# SCRIPT_TYPE  ACCOUNT #-}
            | {-# CONTENT_TYPE LIBRARY #-}
            | func inc(a: Int) = a + 1
          """.stripMargin,
        "lib2" ->
          """
            | {-# SCRIPT_TYPE  ACCOUNT #-}
            | {-# CONTENT_TYPE LIBRARY #-}
            | func dec(a: Int) = a - 1
          """.stripMargin,
        "lib3" ->
          """
            | {-# SCRIPT_TYPE  ACCOUNT #-}
            | {-# CONTENT_TYPE LIBRARY #-}
            | func multiply(a: Int, b: Int) = a * b
          """.stripMargin
      )

    processAndEval(script, libraries) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("multiple libraries list with spaces") {
    val script =
      """
        | {-# SCRIPT_TYPE ACCOUNT #-}
        | {-# IMPORT lib1, lib2 , lib3 #-}
        | let a = 5
        | multiply(inc(a), dec(a)) == (5 + 1) * (5 - 1)
      """.stripMargin

    val libraries =
      Map(
        "lib1" ->
          """
            | {-# SCRIPT_TYPE  ACCOUNT #-}
            | {-# CONTENT_TYPE LIBRARY #-}
            | func inc(a: Int) = a + 1
          """.stripMargin,
        "lib2" ->
          """
            | {-# SCRIPT_TYPE  ACCOUNT #-}
            | {-# CONTENT_TYPE LIBRARY #-}
            | func dec(a: Int) = a - 1
          """.stripMargin,
        "lib3" ->
          """
            | {-# SCRIPT_TYPE  ACCOUNT #-}
            | {-# CONTENT_TYPE LIBRARY #-}
            | func multiply(a: Int, b: Int) = a * b
          """.stripMargin
      )

    processAndEval(script, libraries) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("library without CONTENT_TYPE LIBRARY") {
    val script =
      """
        | {-# SCRIPT_TYPE ACCOUNT #-}
        | {-# IMPORT lib1,lib2,lib3 #-}
        | let a = 5
        | multiply(inc(a), dec(a)) == (5 + 1) * (5 - 1)
      """.stripMargin

    val libraries =
      Map(
        "lib1" ->
          """
            | {-# SCRIPT_TYPE  ACCOUNT #-}
            | {-# CONTENT_TYPE LIBRARY #-}
            | func inc(a: Int) = a + 1
          """.stripMargin,
        "lib2" ->
          """
            | {-# SCRIPT_TYPE  ACCOUNT #-}
            | {-# CONTENT_TYPE EXPRESSION #-}
            | func dec(a: Int) = a - 1
          """.stripMargin,
        "lib3" ->
          """
            | {-# SCRIPT_TYPE  ACCOUNT #-}
            | {-# CONTENT_TYPE LIBRARY #-}
            | func multiply(a: Int, b: Int) = a * b
          """.stripMargin
      )

    processAndEval(script, libraries) shouldBe Left("CONTENT_TYPE of `lib2` is not LIBRARY")
  }

  property("unresolved libraries") {
    val script =
      """
        | {-# SCRIPT_TYPE ACCOUNT #-}
        | {-# IMPORT lib1,lib2,lib3,lib4 #-}
        | let a = 5
        | multiply(inc(a), dec(a)) == (5 + 1) * (5 - 1)
      """.stripMargin

    val libraries =
      Map(
        "lib1" ->
          """
            | {-# SCRIPT_TYPE  ACCOUNT #-}
            | {-# CONTENT_TYPE LIBRARY #-}
            | func inc(a: Int) = a + 1
          """.stripMargin,
        "lib2" ->
          """
            | {-# SCRIPT_TYPE  ASSET   #-}
            | {-# CONTENT_TYPE LIBRARY #-}
            | func dec(a: Int) = a - 1
          """.stripMargin
      )

    processAndEval(script, libraries) shouldBe Left("Unresolved imports: `lib3`, `lib4`")
  }
}
