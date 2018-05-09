package com.wavesplatform.lang.v1.testing

import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.FunctionHeaderType
import com.wavesplatform.lang.v1.compiler.Terms.Typed._
import com.wavesplatform.lang.v1.compiler.Terms._
import org.scalacheck._
import scodec.bits.ByteVector

trait TypedScriptGen {

  def BOOLEANgen(gas: Int): Gen[EXPR] =
    if (gas > 0) Gen.oneOf(CONST_BOOLEANgen, BLOCK_BOOLEANgen(gas - 1), IF_BOOLEANgen(gas - 1), FUNCTION_CALLgen(BOOLEAN))
    else Gen.const(TRUE)

  def CONST_BOOLEANgen: Gen[EXPR] = Gen.oneOf(FALSE, TRUE)

  def BLOCK_BOOLEANgen(gas: Int): Gen[EXPR] =
    for {
      let  <- LETgen((gas - 3) / 3)
      body <- Gen.oneOf(BOOLEANgen((gas - 3) / 3), BLOCK_BOOLEANgen((gas - 3) / 3))
    } yield BLOCK(let, body, BOOLEAN)

  def IF_BOOLEANgen(gas: Int): Gen[EXPR] =
    for {
      cnd <- BOOLEANgen((gas - 3) / 3)
      t   <- BOOLEANgen((gas - 3) / 3)
      f   <- BOOLEANgen((gas - 3) / 3)
    } yield IF(cnd, t, f, BOOLEAN)

  def LONGgen(gas: Int): Gen[EXPR] =
    if (gas > 0) Gen.oneOf(CONST_LONGgen, BLOCK_LONGgen(gas - 1), IF_LONGgen(gas - 1), FUNCTION_CALLgen(LONG)) else CONST_LONGgen

  def CONST_LONGgen: Gen[EXPR] = Gen.choose(Long.MinValue, Long.MaxValue).map(CONST_LONG)

  def BLOCK_LONGgen(gas: Int): Gen[EXPR] =
    for {
      let  <- LETgen((gas - 3) / 3)
      body <- Gen.oneOf(LONGgen((gas - 3) / 3), BLOCK_LONGgen((gas - 3) / 3))
    } yield BLOCK(let, body, LONG)

  def IF_LONGgen(gas: Int): Gen[EXPR] =
    for {
      cnd <- BOOLEANgen((gas - 3) / 3)
      t   <- LONGgen((gas - 3) / 3)
      f   <- LONGgen((gas - 3) / 3)
    } yield IF(cnd, t, f, LONG)

  def STRINGgen: Gen[EXPR] = Gen.identifier.map(CONST_STRING)

  def BYTESTRgen: Gen[EXPR] = Gen.identifier.map(x => CONST_BYTEVECTOR(ByteVector(x.getBytes)))

  def REFgen(tpe: TYPE): Gen[EXPR] = Gen.identifier.map(REF(_, tpe))

  def FUNCTION_CALLgen(resultType: TYPE): Gen[EXPR] =
    Gen.const(
      FUNCTION_CALL(
        function = FunctionHeader("+", List(FunctionHeaderType.LONG, FunctionHeaderType.LONG)),
        args = List(CONST_LONG(1), CONST_LONG(1)),
        resultType
      )
    )

  def LETgen(gas: Int): Gen[LET] =
    for {
      name  <- Gen.identifier
      value <- BOOLEANgen((gas - 3) / 3)
    } yield LET(name, value)

}
