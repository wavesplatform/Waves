package com.wavesplatform.lang.v1.testing

import com.wavesplatform.lang.v1.parser.BinaryOperation
import com.wavesplatform.lang.v1.parser.Expressions._
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import org.scalacheck._

trait ScriptGen {

  def CONST_LONGgen: Gen[(EXPR, Long)] = Gen.choose(Long.MinValue, Long.MaxValue).map(v => (CONST_LONG(v), v))

  def BOOLgen(gas: Int): Gen[(EXPR,Boolean)] =
    if (gas > 0) Gen.oneOf(GEgen(gas - 1), GTgen(gas - 1), BGEgen(gas - 1), BGTgen(gas - 1), EQ_INTgen(gas - 1), ANDgen(gas - 1), ORgen(gas - 1), IF_BOOLgen(gas - 1))
    else Gen.const((TRUE, true))

  def SUMgen(gas: Int): Gen[(EXPR, Long)] =
    for {
      (i1, v1) <- INTGen((gas - 2) / 2)
      (i2, v2) <- INTGen((gas - 2) / 2)
      if((BigInt(v1) + BigInt(v2)).isValidLong)
    } yield (BINARY_OP(i1, SUM_OP, i2), (v1 + v2))

  def SUBgen(gas: Int): Gen[(EXPR, Long)] =
    for {
      (i1, v1) <- INTGen((gas - 2) / 2)
      (i2, v2) <- INTGen((gas - 2) / 2)
      if((BigInt(v1) - BigInt(v2)).isValidLong)
    } yield (BINARY_OP(i1, SUB_OP, i2), (v1 - v2))

  def INTGen(gas: Int): Gen[(EXPR, Long)] = if (gas > 0) Gen.oneOf(CONST_LONGgen, SUMgen(gas - 1), SUBgen(gas - 1), IF_INTgen(gas - 1), INTGen(gas-1).filter(v => (-BigInt(v._2)).isValidLong).map(e => (FUNCTION_CALL("-",List(e._1)), -e._2))) else CONST_LONGgen

  def BGEgen(gas: Int): Gen[(EXPR, Boolean)] =
    for {
      dir <- Gen.oneOf(true, false)
      (i1, v1) <- BOOLgen((gas - 2) / 2)
      (i2, v2) <- BOOLgen((gas - 2) / 2)
    } yield if(dir) {
       (BINARY_OP(i1, GE_OP, i2), (v1 >= v2))
      } else {
       (BINARY_OP(i2, LE_OP, i1), (v1 <= v2))
      } 

  def BGTgen(gas: Int): Gen[(EXPR, Boolean)] =
    for {
      dir <- Gen.oneOf(true, false)
      (i1, v1) <- BOOLgen((gas - 2) / 2)
      (i2, v2) <- BOOLgen((gas - 2) / 2)
    } yield if(dir) {
       (BINARY_OP(i1, GT_OP, i2), (v1 > v2))
      } else {
       (BINARY_OP(i2, LT_OP, i1), (v1 < v2))
      } 

  def GEgen(gas: Int): Gen[(EXPR, Boolean)] =
    for {
      dir <- Gen.oneOf(true, false)
      (i1, v1) <- INTGen((gas - 2) / 2)
      (i2, v2) <- INTGen((gas - 2) / 2)
    } yield if(dir) {
       (BINARY_OP(i1, GE_OP, i2), (v1 >= v2))
      } else {
       (BINARY_OP(i2, LE_OP, i1), (v1 <= v2))
      } 

  def GTgen(gas: Int): Gen[(EXPR, Boolean)] =
    for {
      dir <- Gen.oneOf(true, false)
      (i1, v1) <- INTGen((gas - 2) / 2)
      (i2, v2) <- INTGen((gas - 2) / 2)
    } yield if(dir) {
       (BINARY_OP(i1, GT_OP, i2), (v1 > v2))
      } else {
       (BINARY_OP(i2, LT_OP, i1), (v1 < v2))
      }

  def EQ_INTgen(gas: Int): Gen[(EXPR, Boolean)] =
    for {
      (i1, v1) <- INTGen((gas - 2) / 2)
      (i2, v2) <- INTGen((gas - 2) / 2)
    } yield (BINARY_OP(i1, EQ_OP, i2), (v1 == v2))

  def ANDgen(gas: Int): Gen[(EXPR, Boolean)] =
    for {
      (i1, v1) <- BOOLgen((gas - 2) / 2)
      (i2, v2) <- BOOLgen((gas - 2) / 2)
    } yield (BINARY_OP(i1, AND_OP, i2), (v1 && v2))

  def ORgen(gas: Int): Gen[(EXPR, Boolean)] =
    for {
      (i1, v1) <- BOOLgen((gas - 2) / 2)
      (i2, v2) <- BOOLgen((gas - 2) / 2)
    } yield (BINARY_OP(i1, OR_OP, i2), (v1 || v2))

  def IF_BOOLgen(gas: Int): Gen[(EXPR, Boolean)] =
    for {
      (cnd, vcnd) <- BOOLgen((gas - 3) / 3)
      (t, vt)   <- BOOLgen((gas - 3) / 3)
      (f, vf)   <- BOOLgen((gas - 3) / 3)
    } yield (IF(cnd, t, f), if(vcnd) { vt } else { vf })

  def IF_INTgen(gas: Int): Gen[(EXPR, Long)] =
    for {
      (cnd, vcnd) <- BOOLgen((gas - 3) / 3)
      (t, vt)   <- INTGen((gas - 3) / 3)
      (f, vf)   <- INTGen((gas - 3) / 3)
    } yield (IF(cnd, t, f), if(vcnd) { vt } else { vf })

  def STRgen: Gen[EXPR] =
    Gen.identifier.map(CONST_STRING)

  def LETgen(gas: Int): Gen[LET] =
    for {
      name  <- Gen.identifier
      (value, _) <- BOOLgen((gas - 3) / 3)
    } yield LET(name, value)

  def REFgen: Gen[EXPR] =
    Gen.identifier.map(REF)

  def BLOCKgen(gas: Int): Gen[EXPR] =
    for {
      let  <- LETgen((gas - 3) / 3)
      body <- Gen.oneOf(BOOLgen((gas - 3) / 3).map(_._1), BLOCKgen((gas - 3) / 3)) // BLOCKGen wasn't add to BOOLGen since issue: NODE-700
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
    } yield s" $expr " //pred + expr + post

  def toString(expr: EXPR): Gen[String] = expr match {
    case CONST_LONG(x)   => withWhitespaces(s"$x")
    case REF(x)          => withWhitespaces(s"$x")
    case CONST_STRING(x) => withWhitespaces(s"""\"$x\"""")
    case TRUE            => withWhitespaces("true")
    case FALSE           => withWhitespaces("false")
    case FUNCTION_CALL("-", List(CONST_LONG(v))) if (v>=0) => s"-($v)"
    case FUNCTION_CALL(op, List(e)) => toString(e).map(e => s"$op$e")
    case BINARY_OP(x, LE_OP, y) =>
      for {
        arg2 <- toString(x)
        arg1 <- toString(y)
      } yield s"($arg1<=$arg2)"
    case BINARY_OP(x, LT_OP, y) =>
      for {
        arg2 <- toString(x)
        arg1 <- toString(y)
      } yield s"($arg1<$arg2)"
    case BINARY_OP(x, op: BinaryOperation, y) =>
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
      } yield s"let ${let.name} = $v $b\n"
    case _ => ???
  }
}

trait ScriptGenParser extends ScriptGen {
  override def BOOLgen(gas: Int): Gen[(EXPR, Boolean)] = {
    if (gas > 0) Gen.oneOf(GEgen(gas - 1), GTgen(gas - 1), EQ_INTgen(gas - 1), ANDgen(gas - 1), ORgen(gas - 1), IF_BOOLgen(gas - 1), REFgen.map(r => (r, false)), BOOLgen(gas-1).map(e => (FUNCTION_CALL("!", List(e._1)), !e._2)))
    else Gen.const((TRUE, true))
  }

  override def INTGen(gas: Int): Gen[(EXPR, Long)] = if (gas > 0) Gen.oneOf(CONST_LONGgen, SUMgen(gas - 1), IF_INTgen(gas - 1), REFgen.map(r => (r, 0L))) else CONST_LONGgen
}
