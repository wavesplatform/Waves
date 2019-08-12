package com.wavesplatform.lang.compiler

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.Common.{NoShrink, multiplierFunction, produce}
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.parser.BinaryOperation.SUM_OP
import com.wavesplatform.lang.v1.parser.Expressions
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ErrorTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  import com.wavesplatform.lang.v1.parser.Expressions._

  errorTests(
    "can't define LET with the same name as predefined constant" -> "already defined in the scope" -> BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "unit"), CONST_LONG(AnyPos, 2), Seq.empty),
      TRUE(AnyPos)
    ),
    "can't define LET with the same name as predefined function" -> "function with this name is already defined" -> BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "drop"), CONST_LONG(AnyPos, 2), Seq.empty),
      TRUE(AnyPos)
    ),
    "BINARY_OP with wrong types" -> "Compilation failed: Can't find a function overload '+'" -> BINARY_OP(AnyPos,
                                                                                                          TRUE(AnyPos),
                                                                                                          SUM_OP,
                                                                                                          CONST_LONG(AnyPos, 1)),
    "IF clause must be boolean"                    -> "Unexpected type, required: Boolean" -> IF(AnyPos, CONST_LONG(AnyPos, 0), TRUE(AnyPos), FALSE(AnyPos)),
    "FUNCTION_CALL with wrong amount of arguments" -> "requires 2 arguments" -> FUNCTION_CALL(
      AnyPos,
      PART.VALID(AnyPos, multiplierFunction.name),
      List(CONST_LONG(AnyPos, 0))
    ),
    "FUNCTION_CALL with wrong type of argument" -> "Compilation failed: Non-matching types" -> FUNCTION_CALL(
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
          Expressions.PART.VALID(AnyPos, "id"),
          Seq((Expressions.PART.VALID(AnyPos, "x"), Seq((Expressions.PART.VALID(AnyPos, "Int"), None)))),
          Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "x"))
        ),
        Expressions.FUNCTION_CALL(AnyPos, Expressions.PART.VALID(AnyPos, "id"), List(Expressions.TRUE(AnyPos)))
      ),
    "User functions: wrong arg amount" -> "requires 1 arguments" ->
      Expressions.BLOCK(
        AnyPos,
        Expressions.FUNC(
          AnyPos,
          Expressions.PART.VALID(AnyPos, "id"),
          Seq((Expressions.PART.VALID(AnyPos, "x"), Seq((Expressions.PART.VALID(AnyPos, "Int"), None)))),
          Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "x"))
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
          Expressions.PART.VALID(AnyPos, "id"),
          Seq(
            (Expressions.PART.VALID(AnyPos, "x"), Seq((Expressions.PART.VALID(AnyPos, "Int"), None))),
            (Expressions.PART.VALID(AnyPos, "x"), Seq((Expressions.PART.VALID(AnyPos, "Int"), None)))
          ),
          Expressions.REF(AnyPos, Expressions.PART.VALID(AnyPos, "x"))
        ),
        CONST_LONG(AnyPos, 1)
      ),
  )

  private def errorTests(exprs: ((String, String), Expressions.EXPR)*): Unit = exprs.foreach {
    case ((label, error), input) =>
      property(s"Error: $label") {
        ExpressionCompiler(compilerContext, input) should produce(error)
      }
  }

}
