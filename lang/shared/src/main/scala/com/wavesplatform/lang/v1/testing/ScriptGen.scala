package com.wavesplatform.lang.v1.testing

import com.wavesplatform.lang.v1.Terms.Untyped._
import com.wavesplatform.lang.v1.Terms._
import org.scalacheck._

trait ScriptGen {

  def CONST_LONGgen: Gen[EXPR] = Gen.choose(Long.MinValue, Long.MaxValue).map(CONST_LONG)

  def BOOLgen(gas: Int): Gen[EXPR] =
    if (gas > 0) Gen.oneOf(GEgen(gas - 1), GTgen(gas - 1), EQ_INTgen(gas - 1), ANDgen(gas - 1), ORgen(gas - 1), IF_BOOLgen(gas - 1))
    else Gen.const(TRUE)

  def SUMgen(gas: Int): Gen[EXPR] =
    for {
      i1 <- INTGen((gas - 2) / 2)
      i2 <- INTGen((gas - 2) / 2)
    } yield BINARY_OP(i1, SUM_OP, i2)

  def INTGen(gas: Int): Gen[EXPR] = if (gas > 0) Gen.oneOf(CONST_LONGgen, SUMgen(gas - 1), IF_INTgen(gas - 1)) else CONST_LONGgen

  def GEgen(gas: Int): Gen[EXPR] =
    for {
      i1 <- INTGen((gas - 2) / 2)
      i2 <- INTGen((gas - 2) / 2)
    } yield BINARY_OP(i1, GE_OP, i2)

  def GTgen(gas: Int): Gen[EXPR] =
    for {
      i1 <- INTGen((gas - 2) / 2)
      i2 <- INTGen((gas - 2) / 2)
    } yield BINARY_OP(i1, GT_OP, i2)

  def EQ_INTgen(gas: Int): Gen[EXPR] =
    for {
      i1 <- INTGen((gas - 2) / 2)
      i2 <- INTGen((gas - 2) / 2)
    } yield BINARY_OP(i1, EQ_OP, i2)

  def ANDgen(gas: Int): Gen[EXPR] =
    for {
      i1 <- BOOLgen((gas - 2) / 2)
      i2 <- BOOLgen((gas - 2) / 2)
    } yield BINARY_OP(i1, AND_OP, i2)

  def ORgen(gas: Int): Gen[EXPR] =
    for {
      i1 <- BOOLgen((gas - 2) / 2)
      i2 <- BOOLgen((gas - 2) / 2)
    } yield BINARY_OP(i1, OR_OP, i2)

  def IF_BOOLgen(gas: Int): Gen[EXPR] =
    for {
      cnd <- BOOLgen((gas - 3) / 3)
      t   <- BOOLgen((gas - 3) / 3)
      f   <- BOOLgen((gas - 3) / 3)
    } yield IF(cnd, t, f)

  def IF_INTgen(gas: Int): Gen[EXPR] =
    for {
      cnd <- BOOLgen((gas - 3) / 3)
      t   <- INTGen((gas - 3) / 3)
      f   <- INTGen((gas - 3) / 3)
    } yield IF(cnd, t, f)

  def STRgen: Gen[EXPR] =
    Gen.identifier.map(CONST_STRING)

  def LETgen(gas: Int): Gen[LET] =
    for {
      name  <- Gen.identifier
      value <- BOOLgen((gas - 3) / 3)
    } yield LET(name, value)

  def REFgen: Gen[EXPR] =
    Gen.identifier.map(REF)

  def BLOCKgen(gas: Int): Gen[EXPR] =
    for {
      let  <- LETgen((gas - 3) / 3)
      body <- Gen.oneOf(BOOLgen((gas - 3) / 3), BLOCKgen((gas - 3) / 3)) // BLOCKGen wasn't add to BOOLGen since issue: NODE-700
    } yield BLOCK(let, body)

  private val spaceChars: Seq[Char] = " \t\n\r"

  val whitespaceChar: Gen[Char] = Gen.oneOf(spaceChars)
  val whitespaces: Gen[String] = for {
    n  <- Gen.choose(1, 5)
    xs <- Gen.listOfN(n, whitespaceChar)
  } yield xs.mkString

  def withWhitespaces(expr: String): Gen[String] =
    for {
      pred <- whitespaces
      post <- whitespaces
    } yield pred + expr + post

  def toString(expr: EXPR): Gen[String] = expr match {
    case CONST_LONG(x)   => withWhitespaces(s"$x")
    case REF(x)          => withWhitespaces(s"$x")
    case CONST_STRING(x) => withWhitespaces(s"""\"$x\"""")
    case TRUE            => withWhitespaces("true")
    case FALSE           => withWhitespaces("false")
    case BINARY_OP(x, op: BINARY_OP_KIND, y) =>
      for {
        arg1 <- toString(x)
        arg2 <- toString(y)
      } yield s"($arg1${opsToFunctions(op)}$arg2)"
    case IF(cond, x, y) =>
      for {
        c <- toString(cond)
        t <- toString(x)
        f <- toString(y)
      } yield s"(if ($c) then $t else $f)"
    case BLOCK(let, body) =>
      for {
        v <- toString(let.value)
        b <- toString(body)
      } yield s"let ${let.name} = $v$b\n"
    case _ => ???
  }
}

trait ScriptGenParser extends ScriptGen {
  override def BOOLgen(gas: Int): Gen[EXPR] = {
    if (gas > 0) Gen.oneOf(GEgen(gas - 1), GTgen(gas - 1), EQ_INTgen(gas - 1), ANDgen(gas - 1), ORgen(gas - 1), IF_BOOLgen(gas - 1), REFgen)
    else Gen.const(TRUE)
  }

  override def INTGen(gas: Int): Gen[EXPR] = if (gas > 0) Gen.oneOf(CONST_LONGgen, SUMgen(gas - 1), IF_INTgen(gas - 1), REFgen) else CONST_LONGgen
}
