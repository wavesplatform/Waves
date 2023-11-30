package com.wavesplatform.lang.compiler

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.Common.multiplierFunction
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.{V3, V5}
import com.wavesplatform.lang.v1.compiler.{ExpressionCompiler, TestCompiler}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.GlobalValNames
import com.wavesplatform.lang.v1.parser.BinaryOperation.SUM_OP
import com.wavesplatform.lang.v1.parser.Expressions
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import com.wavesplatform.test.*

class ErrorTest extends PropSpec {

  import com.wavesplatform.lang.v1.parser.Expressions._

  errorTests(
    "can't define LET with the same name as predefined constant" -> "already defined in the scope" -> BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, GlobalValNames.Unit), CONST_LONG(AnyPos, 2)),
      TRUE(AnyPos)
    ),
    "can't define LET with the same name as predefined function" -> "function with this name is already defined" -> BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "drop"), CONST_LONG(AnyPos, 2)),
      TRUE(AnyPos)
    ),
    "BINARY_OP with wrong types" -> "Can't find a function overload '+'" -> BINARY_OP(
      AnyPos,
      TRUE(AnyPos),
      SUM_OP,
      CONST_LONG(AnyPos, 1)
    ),
    "IF clause must be boolean" -> "Unexpected type, required: Boolean" -> IF(AnyPos, CONST_LONG(AnyPos, 0), TRUE(AnyPos), FALSE(AnyPos)),
    "FUNCTION_CALL with wrong amount of arguments" -> "requires 2 arguments" -> FUNCTION_CALL(
      AnyPos,
      PART.VALID(AnyPos, multiplierFunction.name),
      List(CONST_LONG(AnyPos, 0))
    ),
    "FUNCTION_CALL with wrong type of argument" -> "Non-matching types" -> FUNCTION_CALL(
      AnyPos,
      PART.VALID(AnyPos, multiplierFunction.name),
      List(CONST_LONG(AnyPos, 0), FALSE(AnyPos))
    ),
    "FUNCTION_CALL with uncommon types for parameter T" -> "Can't match inferred types" -> FUNCTION_CALL(
      AnyPos,
      PART.VALID(AnyPos, functionWithTwoPrarmsOfTheSameType.name),
      List(CONST_LONG(AnyPos, 1), CONST_BYTESTR(AnyPos, PART.VALID(AnyPos, ByteStr.empty)))
    ),
    "User functions: wrong arg type" -> "Non-matching types" ->
      Expressions.BLOCK(
        AnyPos,
        Expressions.FUNC(
          AnyPos,
          Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "x")),
          Expressions.PART.VALID(AnyPos, "id"),
          Seq((Expressions.PART.VALID(AnyPos, "x"), Single(Expressions.PART.VALID(AnyPos, "Int"), None)))
        ),
        Expressions.FUNCTION_CALL(AnyPos, Expressions.PART.VALID(AnyPos, "id"), List(Expressions.TRUE(AnyPos)))
      ),
    "User functions: wrong arg amount" -> "requires 1 arguments" ->
      Expressions.BLOCK(
        AnyPos,
        Expressions.FUNC(
          AnyPos,
          Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "x")),
          Expressions.PART.VALID(AnyPos, "id"),
          Seq((Expressions.PART.VALID(AnyPos, "x"), Single(Expressions.PART.VALID(AnyPos, "Int"), None)))
        ),
        Expressions.FUNCTION_CALL(
          AnyPos,
          Expressions.PART.VALID(AnyPos, "id"),
          List(Expressions.CONST_LONG(AnyPos, 1L), Expressions.CONST_LONG(AnyPos, 1L))
        )
      ),
    "User functions: can't use same arg names" -> "argument names" ->
      Expressions.BLOCK(
        AnyPos,
        Expressions.FUNC(
          AnyPos,
          Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "x")),
          Expressions.PART.VALID(AnyPos, "id"),
          Seq(
            (Expressions.PART.VALID(AnyPos, "x"), Single(Expressions.PART.VALID(AnyPos, "Int"), None)),
            (Expressions.PART.VALID(AnyPos, "x"), Single(Expressions.PART.VALID(AnyPos, "Int"), None))
          )
        ),
        CONST_LONG(AnyPos, 1)
      )
  )

  property("undefined variable in FOLD macro") {
    TestCompiler(V5).compile(
      s"""
         |{-# STDLIB_VERSION 5 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
         |func calc() = {
         |
         |  # 'let misplaced = 2' should be defined here
         |  
         |  func fold(totals: Int, r: String) = {
         |    let misplaced = 0
         |    1
         |  }
         |  FOLD<7>([], misplaced, fold)
         |}
         |""".stripMargin
    ) shouldBe Left("Compilation failed: [A definition of 'misplaced' is not found in 234-243]")
  }

  property("not allow using List[T] where T is not Any in pattern matching") {
    def errorMsg(pos: String): String =
      s"Compilation failed: [Unexpected generic match type: only List[Any] is allowed in $pos]"

    val invalidPatternsWithError = Seq(
      "List[Int]"              -> errorMsg("213-216"),
      "(List[Int], List[Int])" -> errorMsg("214-217"),
      "(List[Int], List[Any])" -> errorMsg("214-217"),
      "(List[Any], List[Int])" -> errorMsg("225-228"),
      "List[Int] | List[Int]"  -> errorMsg("213-216"),
      "List[Int] | List[Any]"  -> errorMsg("213-216"),
      "List[Any] | List[Int]"  -> errorMsg("225-228")
    )
    val validPatterns = Seq(
      "List[Any]",
      "(List[Any], List[Any])",
      "List[Any] | List[Any]"
    )

    invalidPatternsWithError.foreach { case (pattern, expectedError) =>
      createPatternMatchScript(pattern) shouldBe Left(expectedError)
    }

    validPatterns.foreach { pattern =>
      createPatternMatchScript(pattern).isRight shouldBe true
    }
  }

  private def createPatternMatchScript(pattern: String): Either[String, DApp] =
    TestCompiler(V5).compile(
      s"""
         |{-# STDLIB_VERSION 5 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
         |func invokeProcess(contract: Address) = {
         |  strict result = invoke(contract, "process", [], [])
         |  match (result) {
         |    case r: $pattern => r
         |    case _ => throw("Incorrect invoke result")
         |  }
         |}
         |""".stripMargin
    )

  private def errorTests(exprs: ((String, String), Expressions.EXPR)*): Unit = exprs.foreach { case ((label, error), input) =>
    property(s"Error: $label") {
      ExpressionCompiler(compilerContext, V3, input) should produce(error)
    }
  }

}
