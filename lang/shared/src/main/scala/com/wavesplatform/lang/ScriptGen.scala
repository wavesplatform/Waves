package com.wavesplatform.lang

import com.wavesplatform.lang.Terms.Untyped._
import com.wavesplatform.lang.Terms._
import org.scalacheck._

trait ScriptGen {

  private def CONST_INTgen: Gen[EXPR] = Gen.choose(Int.MinValue, Int.MaxValue).map(CONST_INT)

  private def SUMgen(gas: Int): Gen[EXPR] = for {
    i1 <- INTGen((gas - 2) / 2)
    i2 <- INTGen((gas - 2) / 2)
  } yield BINARY_OP(i1, SUM_OP, i2)

  private def INTGen(gas: Int): Gen[EXPR] = if (gas > 0) Gen.oneOf(CONST_INTgen, SUMgen(gas - 1), IF_INTgen(gas - 1)) else CONST_INTgen

  private def GEgen(gas: Int): Gen[EXPR] = for {
    i1 <- INTGen((gas - 2) / 2)
    i2 <- INTGen((gas - 2) / 2)
  } yield BINARY_OP(i1, GE_OP, i2)


  private def GTgen(gas: Int): Gen[EXPR] = for {
    i1 <- INTGen((gas - 2) / 2)
    i2 <- INTGen((gas - 2) / 2)
  } yield BINARY_OP(i1, GT_OP, i2)

  private def EQ_INTgen(gas: Int): Gen[EXPR] = for {
    i1 <- INTGen((gas - 2) / 2)
    i2 <- INTGen((gas - 2) / 2)
  } yield BINARY_OP(i1, EQ_OP, i2)

  private def ANDgen(gas: Int): Gen[EXPR] = for {
    i1 <- BOOLgen((gas - 2) / 2)
    i2 <- BOOLgen((gas - 2) / 2)
  } yield BINARY_OP(i1, AND_OP, i2)


  private def ORgen(gas: Int): Gen[EXPR] = for {
    i1 <- BOOLgen((gas - 2) / 2)
    i2 <- BOOLgen((gas - 2) / 2)
  } yield BINARY_OP(i1, OR_OP, i2)

  def BOOLgen(gas: Int): Gen[EXPR] = if (gas > 0) Gen.oneOf(GEgen(gas - 1), GTgen(gas - 1), EQ_INTgen(gas - 1), ANDgen(gas - 1), ORgen(gas - 1), IF_BOOLgen(gas - 1)) else Gen.const(TRUE)

  private def IF_BOOLgen(gas: Int): Gen[EXPR] = for {
    cnd <- BOOLgen((gas - 3) / 3)
    t <- BOOLgen((gas - 3) / 3)
    f <- BOOLgen((gas - 3) / 3)
  } yield IF(cnd, t, f)

  private def IF_INTgen(gas: Int): Gen[EXPR] = for {
    cnd <- BOOLgen((gas - 3) / 3)
    t <- INTGen((gas - 3) / 3)
    f <- INTGen((gas - 3) / 3)
  } yield IF(cnd, t, f)

}
