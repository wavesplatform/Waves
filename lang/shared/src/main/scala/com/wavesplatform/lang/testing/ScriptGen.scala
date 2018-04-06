package com.wavesplatform.lang.testing

import com.wavesplatform.lang.Terms.Untyped._
import com.wavesplatform.lang.Terms._
import org.scalacheck._

trait ScriptGen {

  def CONST_LONGgen: Gen[EXPR] = Gen.choose(Long.MinValue, Long.MaxValue).map(CONST_LONG)

  def BOOLgen(gas: Int): Gen[EXPR] =
    if (gas > 0) Gen.oneOf(GEgen(gas - 1), GTgen(gas - 1), EQ_INTgen(gas - 1), ANDgen(gas - 1), ORgen(gas - 1), IF_BOOLgen(gas - 1))
    else Gen.const(TRUE)

  private def SUMgen(gas: Int): Gen[EXPR] =
    for {
      i1 <- INTGen((gas - 2) / 2)
      i2 <- INTGen((gas - 2) / 2)
    } yield BINARY_OP(i1, SUM_OP, i2)

  private def INTGen(gas: Int): Gen[EXPR] = if (gas > 0) Gen.oneOf(CONST_LONGgen, SUMgen(gas - 1), IF_INTgen(gas - 1)) else CONST_LONGgen

  private def GEgen(gas: Int): Gen[EXPR] =
    for {
      i1 <- INTGen((gas - 2) / 2)
      i2 <- INTGen((gas - 2) / 2)
    } yield BINARY_OP(i1, GE_OP, i2)

  private def GTgen(gas: Int): Gen[EXPR] =
    for {
      i1 <- INTGen((gas - 2) / 2)
      i2 <- INTGen((gas - 2) / 2)
    } yield BINARY_OP(i1, GT_OP, i2)

  private def EQ_INTgen(gas: Int): Gen[EXPR] =
    for {
      i1 <- INTGen((gas - 2) / 2)
      i2 <- INTGen((gas - 2) / 2)
    } yield BINARY_OP(i1, EQ_OP, i2)

  private def ANDgen(gas: Int): Gen[EXPR] =
    for {
      i1 <- BOOLgen((gas - 2) / 2)
      i2 <- BOOLgen((gas - 2) / 2)
    } yield BINARY_OP(i1, AND_OP, i2)

  private def ORgen(gas: Int): Gen[EXPR] =
    for {
      i1 <- BOOLgen((gas - 2) / 2)
      i2 <- BOOLgen((gas - 2) / 2)
    } yield BINARY_OP(i1, OR_OP, i2)

  private def IF_BOOLgen(gas: Int): Gen[EXPR] =
    for {
      cnd <- BOOLgen((gas - 3) / 3)
      t   <- BOOLgen((gas - 3) / 3)
      f   <- BOOLgen((gas - 3) / 3)
    } yield IF(cnd, t, f)

  private def IF_INTgen(gas: Int): Gen[EXPR] =
    for {
      cnd <- BOOLgen((gas - 3) / 3)
      t   <- INTGen((gas - 3) / 3)
      f   <- INTGen((gas - 3) / 3)
    } yield IF(cnd, t, f)

  private val spaceChars: Seq[Char] = Vector('\u0020', '\u0009', '\u000D')

  def whitespaceChar: Gen[Char] = Gen.oneOf(spaceChars)
  val whitespaces: Gen[String]  = Gen.listOf(whitespaceChar).map(_.mkString)
}
