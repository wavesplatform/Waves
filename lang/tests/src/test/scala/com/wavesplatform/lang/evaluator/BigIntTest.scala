package com.wavesplatform.lang.evaluator

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common.produce
import com.wavesplatform.lang.directives.values.{StdLibVersion, V5}
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BIGINT, CONST_BOOLEAN, CONST_LONG, CONST_STRING}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{PureContext, unit}

import scala.math.BigDecimal.RoundingMode._
import scala.util.Random

class BigIntTest extends EvaluatorSpec {
  implicit val startVersion: StdLibVersion = V5

  private val maxValue = s"""parseBigIntValue("${PureContext.BigIntMax}")"""
  private val minValue = s"""parseBigIntValue("${PureContext.BigIntMin}")"""

  property("BigInt") {
    eval("toBigInt(fraction(9223372036854775807, -2, -4)) == (toBigInt(9223372036854775807) * toBigInt(-2)) / toBigInt(-4)") shouldBe Right(
      CONST_BOOLEAN(true)
    )
    eval(s"""
      func f0(i: BigInt) = i*i
      ${(0 to 6).map(n => "func f" ++ (n + 1).toString ++ "(i: BigInt) = f" ++ n.toString ++ "(i*i)").mkString("\n")}
      f7(2.toBigInt())
      """) shouldBe Right(CONST_BIGINT(BigInt(2).pow(256)))
    eval(s"""
      func f0(i: BigInt) = i*i
      ${(0 to 7).map(n => "func f" ++ (n + 1).toString ++ "(i: BigInt) = f" ++ n.toString ++ "(i*i)").mkString("\n")}
      f8(2.toBigInt())
      """) should produce("is out of range")
    eval(s"""-toBigInt(1)""") shouldBe Right(CONST_BIGINT(BigInt(-1)))
    eval("toBigInt(base58'2Ana1pUpv2ZbMVkwF5FXapYeBEjdxDatLn7nvJkhgTSXbs59SyZSx866bXirPgj8QQVB57uxHJBG1YFvkRbFj4T').toString()") shouldBe Right(
      CONST_STRING(
        "52785833603464895924505196455835395749861094195642486808108138863402869537852026544579466671752822414281401856143643660416162921950916138504990605852480"
      ).explicitGet()
    )
    eval("toBigInt(base58'3ZRuyAbxDY78aZf8nGASFCZKPhJ5LuDAuALLdoAqCqaFuAjqgmZ47WsE11LKt2JuF5Pasqx65bzvjWMzHB2b4vuC').toString()") shouldBe Right(
      CONST_STRING(
        "6703903964971298549787012499102923063739682910296196688861780721860882015036773488400937149083451713845015929093243025426876941405973284973216824503042047"
      ).explicitGet()
    )
    eval("toBigInt(base58'3ZRuyAbxDY78aZf8nGASFCZKPhJ5LuDAuALLdoAqCqaFuAjqgmZ47WsE11LKt2JuF5Pasqx65bzvjWMzHB2b4vuCa').toString()") should produce(
      "Too big ByteVector for BigInt (65 > 64 bytes)"
    )
    eval("let bin = base58'2Ana1pUpv2ZbMVkwF5FXapYeBEjdxDatLn7nvJkhgTSXbs59SyZSx866bXirPgj8QQVB57uxHJBG1YFvkRbFj4T' ; toBytes(toBigInt(bin)) == bin") shouldBe Right(
      CONST_BOOLEAN(true)
    )
    eval(
      "toBigInt(base58'a' + base58'2Ana1pUpv2ZbMVkwF5FXapYeBEjdxDatLn7nvJkhgTSXbs59SyZSx866bXirPgj8QQVB57uxHJBG1YFvkRbFj4T' + base58'a', base58'a'.size(), 64).toString()"
    ) shouldBe Right(
      CONST_STRING(
        "52785833603464895924505196455835395749861094195642486808108138863402869537852026544579466671752822414281401856143643660416162921950916138504990605852480"
      ).explicitGet()
    )
    eval(maxValue) shouldBe Right(CONST_BIGINT(PureContext.BigIntMax))
    eval(minValue) shouldBe Right(CONST_BIGINT(PureContext.BigIntMin))
    eval(
      """parseBigIntValue("6703903964971298549787012499102923063739682910296196688861780721860882015036773488400937149083451713845015929093243025426876941405973284973216824503042048")"""
    ) should produce("too big")
    eval(
      """parseBigIntValue("-6703903964971298549787012499102923063739682910296196688861780721860882015036773488400937149083451713845015929093243025426876941405973284973216824503042049")"""
    ) should produce("too big")
    eval(
      """parseBigInt("-6703903964971298549787012499102923063739682910296196688861780721860882015036773488400937149083451713845015929093243025426876941405973284973216824503042048")"""
    ) shouldBe Right(CONST_BIGINT(PureContext.BigIntMin))
    eval(
      """parseBigInt("6703903964971298549787012499102923063739682910296196688861780721860882015036773488400937149083451713845015929093243025426876941405973284973216824503042048")"""
    ) shouldBe Right(unit)
    eval(s"""fraction(parseBigIntValue("${PureContext.BigIntMax}"), toBigInt(-2), toBigInt(-3))""") shouldBe Right(
      CONST_BIGINT(PureContext.BigIntMax * 2 / 3)
    )
    eval(s"""fraction(toBigInt(100), toBigInt(2), toBigInt(0))""") shouldBe Left("Fraction: division by zero")
    eval(s"""parseBigIntValue("${Long.MaxValue}").toInt()""") shouldBe Right(CONST_LONG(Long.MaxValue))
    eval(s"""parseBigIntValue("${Long.MinValue}").toInt()""") shouldBe Right(CONST_LONG(Long.MinValue))
    eval(s"""(parseBigIntValue("${Long.MaxValue}")+toBigInt(1)).toInt()""") should produce("out of integers range")
  }

  property("BigInt fraction rounding") {
    for {
      s1 <- List(-1, 0, 1)
      s2 <- List(-1, 1)
      r  <- List(("DOWN", DOWN), ("CEILING", CEILING), ("FLOOR", FLOOR), ("HALFUP", HALF_UP), ("HALFEVEN", HALF_EVEN))
    } {
      eval(s"""fraction(toBigInt(${10 * s1}), toBigInt(1), toBigInt(${3 * s2}), ${r._1})""") shouldBe
        Right(CONST_BIGINT(BigInt(BigDecimal((10.0 * s1) / (3.0 * s2)).setScale(0, r._2).toLong)))
      eval(s"""fraction(toBigInt(${9 * s1}), toBigInt(1), toBigInt(${2 * s2}), ${r._1})""") shouldBe
        Right(CONST_BIGINT(BigInt(BigDecimal((9.0 * s1) / (2.0 * s2)).setScale(0, r._2).toLong)))
      eval(s"""fraction(toBigInt(${11 * s1}), toBigInt(1), toBigInt(${2 * s2}), ${r._1})""") shouldBe
        Right(CONST_BIGINT(BigInt(BigDecimal((11.0 * s1) / (2.0 * s2)).setScale(0, r._2).toLong)))
      eval(s"""fraction(toBigInt(100), toBigInt(2), toBigInt(0), ${r._1})""") shouldBe
        Left("Fraction: division by zero")
    }
  }

  property("BigInt fraction returning limits") {
    eval(s"""fraction($maxValue, $maxValue, $maxValue)""") shouldBe Right(CONST_BIGINT(PureContext.BigIntMax))
    eval(s"""fraction($minValue, $minValue, $minValue)""") shouldBe Right(CONST_BIGINT(PureContext.BigIntMin))
    eval(s"""fraction($maxValue, $maxValue, $maxValue, CEILING)""") shouldBe Right(CONST_BIGINT(PureContext.BigIntMax))
    eval(s"""fraction($minValue, $minValue, $minValue, CEILING)""") shouldBe Right(CONST_BIGINT(PureContext.BigIntMin))
  }

  property("BigInt comparison") {
    eval("toBigInt(16) > toBigInt(2)") shouldBe Right(CONST_BOOLEAN(true))
    eval("toBigInt(1) > toBigInt(2)") shouldBe Right(CONST_BOOLEAN(false))
    eval("toBigInt(16) >= toBigInt(2)") shouldBe Right(CONST_BOOLEAN(true))
    eval("toBigInt(1) >= toBigInt(2)") shouldBe Right(CONST_BOOLEAN(false))
    eval("toBigInt(16) >= toBigInt(16)") shouldBe Right(CONST_BOOLEAN(true))
    eval("toBigInt(16) < toBigInt(2)") shouldBe Right(CONST_BOOLEAN(false))
    eval("toBigInt(16) <= toBigInt(2)") shouldBe Right(CONST_BOOLEAN(false))
    eval("toBigInt(16) <= toBigInt(16)") shouldBe Right(CONST_BOOLEAN(true))
  }

  property("BigInt comparison result") {
    eval("if (toBigInt(16) > toBigInt(2)) then 1 else 0") shouldBe Right(CONST_LONG(1))
    eval("if (toBigInt(16) < toBigInt(2)) then 1 else 0") shouldBe Right(CONST_LONG(0))
  }

  property("BigInt min and max") {
    eval("[toBigInt(16), toBigInt(8)].max()") shouldBe Right(CONST_BIGINT(BigInt(16)))
    eval("[toBigInt(16), toBigInt(8)].min()") shouldBe Right(CONST_BIGINT(BigInt(8)))
  }

  property("BigInt math functions") {
    eval("pow(toBigInt(2), 0, toBigInt(510), 0, 0, DOWN)") shouldBe Right(CONST_BIGINT(BigInt(2) pow 510))
    eval("pow(toBigInt(2), 0, toBigInt(511), 0, 0, DOWN)") should produce("Result out of 512-bit range on BigInt pow calculation")
    eval("pow(toBigInt(-2), 0, toBigInt(511), 0, 0, DOWN)") shouldBe Right(CONST_BIGINT(BigInt(-2) pow 511))
    eval("pow(toBigInt(-2), 0, toBigInt(512), 0, 0, DOWN)") should produce("Result out of 512-bit range on BigInt pow calculation")
    eval("pow(toBigInt(12), 1, toBigInt(3456), 3, 2, DOWN)") shouldBe Right(CONST_BIGINT(BigInt(187)))
    eval("pow(toBigInt(0), 1, toBigInt(3456), 3, 2, DOWN)") shouldBe Right(CONST_BIGINT(BigInt(0)))
    eval("pow(toBigInt(20), 1, toBigInt(-1), 0, 4, DOWN)") shouldBe Right(CONST_BIGINT(BigInt(5000)))
    eval("pow(toBigInt(-20), 1, toBigInt(-1), 0, 4, DOWN)") shouldBe Right(CONST_BIGINT(BigInt(-5000)))
    eval("pow(toBigInt(0), 1, toBigInt(-1), 0, 4, DOWN)") should produce("Division by zero on BigInt pow calculation")
    eval("pow(toBigInt(2), 0, toBigInt(512), 0, 0, DOWN)") should produce("Result out of 512-bit range on BigInt pow calculation")
    eval("log(toBigInt(16), 0, toBigInt(2), 0, 0, CEILING)") shouldBe Right(CONST_BIGINT(BigInt(4)))
    eval("log(toBigInt(1), 4, toBigInt(1), 1, 0, HALFEVEN)") shouldBe Right(CONST_BIGINT(BigInt(4)))
    eval("log(toBigInt(16), 0, toBigInt(-2), 0, 0, CEILING)") should produce("Illegal log(x) for x <= 0: x = -2 on BigInt log calculation")
    eval("log(toBigInt(-16), 0, toBigInt(2), 0, 0, CEILING)") should produce("Illegal log(x) for x <= 0: x = -16 on BigInt log calculation")
    eval("""log(toBigInt(1), 16, parseBigIntValue("10"), 0, 0, CEILING)""") shouldBe Right(CONST_BIGINT(BigInt(-16)))
  }

  property("BigInt plus") {
    eval(s"$minValue + toBigInt(1)") shouldBe Right(CONST_BIGINT((BigInt(-2) pow 511) + 1))
    eval(s"$maxValue + toBigInt(1)") should produce("is out of range")
    eval(s"$minValue + toBigInt(-1)") should produce("is out of range")
    eval(s"$maxValue + toBigInt(-1)") shouldBe Right(CONST_BIGINT((BigInt(2) pow 511) - 2))
    eval(s"$maxValue + toBigInt(0)") shouldBe eval(maxValue)
    eval(s"$minValue + toBigInt(0)") shouldBe eval(minValue)
    eval(s"toBigInt(0) + $maxValue") shouldBe eval(maxValue)
    eval(s"toBigInt(0) + $minValue") shouldBe eval(minValue)
    eval(s"$maxValue + $minValue") shouldBe Right(CONST_BIGINT(-1))
  }

  property("BigInt minus") {
    eval(s"$minValue - toBigInt(1)") should produce("is out of range")
    eval(s"$maxValue - toBigInt(1)") shouldBe Right(CONST_BIGINT((BigInt(2) pow 511) - 2))
    eval(s"$minValue - toBigInt(-1)") shouldBe Right(CONST_BIGINT((BigInt(-2) pow 511) + 1))
    eval(s"$maxValue - toBigInt(-1)") should produce("is out of range")
    eval(s"$minValue - toBigInt(0)") shouldBe eval(minValue)
    eval(s"$maxValue - toBigInt(0)") shouldBe eval(maxValue)
    eval(s"$minValue - $minValue") shouldBe Right(CONST_BIGINT(0))
    eval(s"$maxValue - $maxValue") shouldBe Right(CONST_BIGINT(0))
  }

  property("BigInt multiplication") {
    val maxValueHalf = s"""parseBigIntValue("${BigInt(2) pow 510}")"""
    val minValueHalf = s"""parseBigIntValue("${-(BigInt(2) pow 510)}")"""
    eval(s"($maxValueHalf - toBigInt(1)) * toBigInt(2) + toBigInt(1)") shouldBe eval(maxValue)
    eval(s"$maxValueHalf * toBigInt(2)") should produce("is out of range")
    eval(s"$maxValueHalf * toBigInt(-2)") shouldBe eval(minValue)
    eval(s"$minValueHalf * toBigInt(2)") shouldBe eval(minValue)
    eval(s"$minValueHalf * toBigInt(-2)") should produce("is out of range")
    eval(s"$maxValue * toBigInt(0)") shouldBe Right(CONST_BIGINT(0))
    eval(s"$minValue * toBigInt(0)") shouldBe Right(CONST_BIGINT(0))
    eval(s"toBigInt(1) * $maxValue") shouldBe eval(maxValue)
    eval(s"toBigInt(1) * $minValue") shouldBe eval(minValue)
  }

  property("BigInt division") {
    eval(s"$maxValue / toBigInt(2)") shouldBe Right(CONST_BIGINT((BigInt(2) pow 510) - 1))
    eval(s"$minValue / toBigInt(2)") shouldBe Right(CONST_BIGINT(-(BigInt(2) pow 510)))
    eval(s"$minValue / toBigInt(0)") should produce("BigInteger divide by zero")
    eval(s"$maxValue / toBigInt(0)") should produce("BigInteger divide by zero")
    eval(s"$maxValue / $maxValue") shouldBe Right(CONST_BIGINT(1))
    eval(s"$maxValue / $minValue") shouldBe Right(CONST_BIGINT(0))
    eval(s"$minValue / $maxValue") shouldBe Right(CONST_BIGINT(-1))
  }

  property("BigInt modulo") {
    eval(s"$maxValue % $maxValue") shouldBe Right(CONST_BIGINT(0))
    eval(s"$minValue % $minValue") shouldBe Right(CONST_BIGINT(0))
    eval(s"$maxValue % $minValue") shouldBe eval(maxValue)
    eval(s"$minValue % $maxValue") shouldBe Right(CONST_BIGINT(-1))
    eval(s"$maxValue % ($maxValue - toBigInt(1))") shouldBe Right(CONST_BIGINT(1))
    eval(s"$maxValue % toBigInt(2)") shouldBe Right(CONST_BIGINT(1))
    eval(s"$maxValue % toBigInt(1)") shouldBe Right(CONST_BIGINT(0))
    eval(s"$maxValue % toBigInt(0)") should produce("BigInteger divide by zero")
  }

  property("BigInt match") {
    eval(
      """
        | let v = 12345
        | match if true then toBigInt(v) else v {
        |   case a: Int    => throw("")
        |   case b: BigInt => b
        | }
      """.stripMargin
    ) shouldBe Right(CONST_BIGINT(12345))
  }

  property("List[BigInt] median - 100 elements") {
    val arr       = (1 to 100).map(_ => Random.nextLong())
    val arrSorted = arr.sorted
    eval(s"[toBigInt(${arr.mkString("),toBigInt(")})].median()") shouldBe Right(CONST_BIGINT(BigInt(Math.floorDiv(arrSorted(49) + arrSorted(50), 2))))
  }

  property("List[BigInt] median - 99 elements") {
    val arr       = (1 to 99).map(_ => Random.nextLong())
    val arrSorted = arr.sorted
    eval(s"[toBigInt(${arr.mkString("),toBigInt(")})].median()") shouldBe Right(CONST_BIGINT(BigInt(arrSorted(49))))
  }

  property("List[BigInt] median - 1 elements") {
    eval(s"[toBigInt(0)].median()") shouldBe Right(CONST_BIGINT(BigInt(0)))
    eval(s"""[$maxValue].median()""") shouldBe Right(CONST_BIGINT(PureContext.BigIntMax))
    eval(s"""[$minValue].median()""") shouldBe Right(CONST_BIGINT(PureContext.BigIntMin))
  }

  property("List[BigInt] median - 1000 elements - success") {
    eval(s"""[${(1 to 1000).map(_ => maxValue).mkString(",")}].median()""") shouldBe Right(CONST_BIGINT(PureContext.BigIntMax))
    eval(s"""[${(1 to 1000).map(_ => minValue).mkString(",")}].median()""") shouldBe Right(CONST_BIGINT(PureContext.BigIntMin))
  }

  property("List[BigInt] median - negative rounding down") {
    eval(s"[toBigInt(${Seq(3, -8).mkString("),toBigInt(")})].median()") shouldBe Right(CONST_BIGINT(BigInt(-3)))
  }

  property("List[BigInt] median - empty list - error") {
    eval(s"[toBigInt(1)].removeByIndex(0).median()") should produce("Can't find median for empty list of BigInt")
  }
}
