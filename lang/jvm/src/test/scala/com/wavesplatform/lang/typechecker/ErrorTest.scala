package com.wavesplatform.lang.typechecker

import com.wavesplatform.lang.Common.{NoShrink, multiplierFunction, produce}
import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.parser.BinaryOperation.SUM_OP
import com.wavesplatform.lang.v1.parser.Expressions
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector

class ErrorTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  import com.wavesplatform.lang.v1.parser.Expressions._

  errorTests(
    "can't define LET with the same name as already defined in scope" -> "already defined in the scope" -> BLOCK(
      0,
      0,
      LET(0, 0, PART.VALID(0, 0, "X"), CONST_LONG(0, 0, 1), Seq.empty),
      BLOCK(0, 0, LET(0, 0, PART.VALID(0, 0, "X"), CONST_LONG(0, 0, 2), Seq.empty), TRUE(0, 0))
    ),
    "can't define LET with the same name as predefined constant" -> "already defined in the scope" -> BLOCK(
      0,
      0,
      LET(0, 0, PART.VALID(0, 0, "None"), CONST_LONG(0, 0, 2), Seq.empty),
      TRUE(0, 0)
    ),
    "can't define LET with the same name as predefined function" -> "function with such name is predefined" -> BLOCK(
      0,
      0,
      LET(0, 0, PART.VALID(0, 0, "Some"), CONST_LONG(0, 0, 2), Seq.empty),
      TRUE(0, 0)
    ),
    "BINARY_OP with wrong types"                   -> "Typecheck failed: Can't find a function '+'" -> BINARY_OP(0, 0, TRUE(0, 0), SUM_OP, CONST_LONG(0, 0, 1)),
    "IF can't find common"                         -> "Can't find common type" -> IF(0, 0, TRUE(0, 0), TRUE(0, 0), CONST_LONG(0, 0, 0)),
    "IF clause must be boolean"                    -> "IF clause is expected to be BOOLEAN" -> IF(0, 0, CONST_LONG(0, 0, 0), TRUE(0, 0), FALSE(0, 0)),
    "FUNCTION_CALL with wrong amount of arguments" -> "requires 2 arguments" -> FUNCTION_CALL(
      0,
      0,
      PART.VALID(0, 0, multiplierFunction.name),
      List(CONST_LONG(0, 0, 0))
    ),
    "FUNCTION_CALL with upper type" -> "Non-matching types" -> FUNCTION_CALL(
      0,
      0,
      PART.VALID(0, 0, unitOnNone.name),
      List(FUNCTION_CALL(0, 0, PART.VALID(0, 0, "Some"), List(CONST_LONG(0, 0, 3))))
    ),
    "FUNCTION_CALL with wrong type of argument" -> "Typecheck failed: Non-matching types: expected: LONG, actual: BOOLEAN" -> FUNCTION_CALL(
      0,
      0,
      PART.VALID(0, 0, multiplierFunction.name),
      List(CONST_LONG(0, 0, 0), FALSE(0, 0))
    ),
    "FUNCTION_CALL with uncommon types for parameter T" -> "Can't match inferred types" -> FUNCTION_CALL(
      0,
      0,
      PART.VALID(0, 0, functionWithTwoPrarmsOfTheSameType.name),
      List(CONST_LONG(0, 0, 1), CONST_BYTEVECTOR(0, 0, PART.VALID(0, 0, ByteVector.empty)))
    )
  )

  private def errorTests(exprs: ((String, String), Expressions.EXPR)*): Unit = exprs.foreach {
    case ((label, error), input) =>
      property(s"Error: $label") {
        CompilerV1(typeCheckerContext, input) should produce(error)
      }
  }

}
