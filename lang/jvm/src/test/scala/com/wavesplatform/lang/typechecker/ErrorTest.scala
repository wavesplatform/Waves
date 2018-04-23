package com.wavesplatform.lang.typechecker

import com.wavesplatform.lang.Common.{NoShrink, multiplierFunction, produce}
import com.wavesplatform.lang.v1.Terms.{SUM_OP, Untyped}
import com.wavesplatform.lang.v1.TypeChecker
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector

class ErrorTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  import Untyped._

  errorTests(
    "can't define LET with the same name as already defined in scope" -> "already defined in the scope" -> BLOCK(LET("X", CONST_LONG(1)),
                                                                                                                 BLOCK(LET("X", CONST_LONG(2)),
                                                                                                                       TRUE)),
    "can't define LET with the same name as predefined constant" -> "already defined in the scope" -> BLOCK(LET("None", CONST_LONG(2)), TRUE),
    "can't define LET with the same name as predefined function" -> "function with such name is predefined" -> BLOCK(LET("Some", CONST_LONG(2)),
                                                                                                                     TRUE),
    "BINARY_OP with wrong types"                   -> "Typecheck failed: Can't find a function '+'" -> BINARY_OP(TRUE, SUM_OP, CONST_LONG(1)),
    "IF can't find common"                         -> "Can't find common type" -> IF(TRUE, TRUE, CONST_LONG(0)),
    "FUNCTION_CALL with wrong amount of arguments" -> "requires 2 arguments" -> FUNCTION_CALL(multiplierFunction.name, List(CONST_LONG(0))),
    "FUNCTION_CALL with upper type"                -> "Non-matching types" -> FUNCTION_CALL(unitOnNone.name, List(FUNCTION_CALL("Some", List(CONST_LONG(3))))),
    "FUNCTION_CALL with wrong type of argument"    -> "Typecheck failed: Non-matching types: expected: LONG, actual: BOOLEAN" -> FUNCTION_CALL(
      multiplierFunction.name,
      List(CONST_LONG(0), FALSE)),
    "FUNCTION_CALL with uncommon types for parameter T" -> "Can't match inferred types" -> FUNCTION_CALL(functionWithTwoPrarmsOfTheSameType.name,
                                                                                                         List(CONST_LONG(1),
                                                                                                              CONST_BYTEVECTOR(ByteVector.empty)))
  )

  private def errorTests(exprs: ((String, String), Untyped.EXPR)*): Unit = exprs.foreach {
    case ((label, error), input) =>
      property(s"Error: $label") {
        TypeChecker(typeCheckerContext, input) should produce(error)
      }
  }

}
