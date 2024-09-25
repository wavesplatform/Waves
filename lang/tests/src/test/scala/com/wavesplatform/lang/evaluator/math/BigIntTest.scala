package com.wavesplatform.lang.evaluator.math

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.{StdLibVersion, V5, V6}
import com.wavesplatform.lang.evaluator.EvaluatorSpec
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BIGINT, CONST_BOOLEAN, CONST_LONG, CONST_STRING}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{PureContext, unit}
import com.wavesplatform.test.produce

import scala.math.BigDecimal.RoundingMode.*
import scala.util.Random

class BigIntTest extends EvaluatorSpec {
  implicit val startVersion: StdLibVersion = V5

  private val max = s"""parseBigIntValue("${PureContext.BigIntMax}")"""
  private val min = s"""parseBigIntValue("${PureContext.BigIntMin}")"""

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
    eval(
      "let bin = base58'2Ana1pUpv2ZbMVkwF5FXapYeBEjdxDatLn7nvJkhgTSXbs59SyZSx866bXirPgj8QQVB57uxHJBG1YFvkRbFj4T' ; toBytes(toBigInt(bin)) == bin"
    ) shouldBe Right(
      CONST_BOOLEAN(true)
    )
    eval(
      "toBigInt(base58'a' + base58'2Ana1pUpv2ZbMVkwF5FXapYeBEjdxDatLn7nvJkhgTSXbs59SyZSx866bXirPgj8QQVB57uxHJBG1YFvkRbFj4T' + base58'a', base58'a'.size(), 64).toString()"
    ) shouldBe Right(
      CONST_STRING(
        "52785833603464895924505196455835395749861094195642486808108138863402869537852026544579466671752822414281401856143643660416162921950916138504990605852480"
      ).explicitGet()
    )
    eval(max) shouldBe Right(CONST_BIGINT(PureContext.BigIntMax))
    eval(min) shouldBe Right(CONST_BIGINT(PureContext.BigIntMin))
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
    eval(s"""fraction($max, $max, $max)""") shouldBe Right(CONST_BIGINT(PureContext.BigIntMax))
    eval(s"""fraction($min, $min, $min)""") shouldBe Right(CONST_BIGINT(PureContext.BigIntMin))
    eval(s"""fraction($max, $max, $max, CEILING)""") shouldBe Right(CONST_BIGINT(PureContext.BigIntMax))
    eval(s"""fraction($min, $min, $min, CEILING)""") shouldBe Right(CONST_BIGINT(PureContext.BigIntMin))
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
    eval(s"$min + toBigInt(1)") shouldBe Right(CONST_BIGINT((BigInt(-2) pow 511) + 1))
    eval(s"$max + toBigInt(1)") should produce("is out of range")
    eval(s"$min + toBigInt(-1)") should produce("is out of range")
    eval(s"$max + toBigInt(-1)") shouldBe Right(CONST_BIGINT((BigInt(2) pow 511) - 2))
    eval(s"$max + toBigInt(0)") shouldBe eval(max)
    eval(s"$min + toBigInt(0)") shouldBe eval(min)
    eval(s"toBigInt(0) + $max") shouldBe eval(max)
    eval(s"toBigInt(0) + $min") shouldBe eval(min)
    eval(s"$max + $min") shouldBe Right(CONST_BIGINT(-1))
  }

  property("BigInt minus") {
    eval(s"$min - toBigInt(1)") should produce("is out of range")
    eval(s"$max - toBigInt(1)") shouldBe Right(CONST_BIGINT((BigInt(2) pow 511) - 2))
    eval(s"$min - toBigInt(-1)") shouldBe Right(CONST_BIGINT((BigInt(-2) pow 511) + 1))
    eval(s"$max - toBigInt(-1)") should produce("is out of range")
    eval(s"$min - toBigInt(0)") shouldBe eval(min)
    eval(s"$max - toBigInt(0)") shouldBe eval(max)
    eval(s"$min - $min") shouldBe Right(CONST_BIGINT(0))
    eval(s"$max - $max") shouldBe Right(CONST_BIGINT(0))
  }

  property("BigInt multiplication") {
    val maxValueHalf = s"""parseBigIntValue("${BigInt(2) pow 510}")"""
    val minValueHalf = s"""parseBigIntValue("${-(BigInt(2) pow 510)}")"""
    eval(s"($maxValueHalf - toBigInt(1)) * toBigInt(2) + toBigInt(1)") shouldBe eval(max)
    eval(s"$maxValueHalf * toBigInt(2)") should produce("is out of range")
    eval(s"$maxValueHalf * toBigInt(-2)") shouldBe eval(min)
    eval(s"$minValueHalf * toBigInt(2)") shouldBe eval(min)
    eval(s"$minValueHalf * toBigInt(-2)") should produce("is out of range")
    eval(s"$max * toBigInt(0)") shouldBe Right(CONST_BIGINT(0))
    eval(s"$min * toBigInt(0)") shouldBe Right(CONST_BIGINT(0))
    eval(s"toBigInt(1) * $max") shouldBe eval(max)
    eval(s"toBigInt(1) * $min") shouldBe eval(min)
  }

  property("BigInt division") {
    eval(s"$max / toBigInt(2)") shouldBe Right(CONST_BIGINT((BigInt(2) pow 510) - 1))
    eval(s"$min / toBigInt(2)") shouldBe Right(CONST_BIGINT(-(BigInt(2) pow 510)))
    eval(s"$min / toBigInt(0)") should produce("BigInteger divide by zero")
    eval(s"$max / toBigInt(0)") should produce("BigInteger divide by zero")
    eval(s"$max / $max") shouldBe Right(CONST_BIGINT(1))
    eval(s"$max / $min") shouldBe Right(CONST_BIGINT(0))
    eval(s"$min / $max") shouldBe Right(CONST_BIGINT(-1))
  }

  property("BigInt modulo") {
    eval(s"$max % $max") shouldBe Right(CONST_BIGINT(0))
    eval(s"$min % $min") shouldBe Right(CONST_BIGINT(0))
    eval(s"$max % $min") shouldBe eval(max)
    eval(s"$min % $max") shouldBe Right(CONST_BIGINT(-1))
    eval(s"$max % ($max - toBigInt(1))") shouldBe Right(CONST_BIGINT(1))
    eval(s"$max % toBigInt(2)") shouldBe Right(CONST_BIGINT(1))
    eval(s"$max % toBigInt(1)") shouldBe Right(CONST_BIGINT(0))
    eval(s"$max % toBigInt(0)") should produce("BigInteger divide by zero")
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
    eval(s"""[$max].median()""") shouldBe Right(CONST_BIGINT(PureContext.BigIntMax))
    eval(s"""[$min].median()""") shouldBe Right(CONST_BIGINT(PureContext.BigIntMin))
  }

  property("List[BigInt] median - 1000 elements - success") {
    eval(s"""[${(1 to 1000).map(_ => max).mkString(",")}].median()""") shouldBe Right(CONST_BIGINT(PureContext.BigIntMax))
    eval(s"""[${(1 to 1000).map(_ => min).mkString(",")}].median()""") shouldBe Right(CONST_BIGINT(PureContext.BigIntMin))
  }

  property("List[BigInt] median - negative rounding down") {
    eval(s"[toBigInt(${Seq(3, -8).mkString("),toBigInt(")})].median()") shouldBe Right(CONST_BIGINT(BigInt(-3)))
  }

  property("List[BigInt] median - empty list - error") {
    eval(s"[toBigInt(1)].removeByIndex(0).median()") should produce("Can't find median for empty list of BigInt")
  }

  property("on the limit") {
    val one = "toBigInt(1)"
    val d18 = """parseBigIntValue("987654321012345678")"""
    val d19 = """parseBigIntValue("1987654321012345678")"""

    val e1 = """parseBigIntValue("3259987654320123456789")"""
    val e2 = """parseBigIntValue("515598765432101234567")"""
    val e3 = s"""$max / (${List.fill(7)(s"""toBigInt(${Long.MaxValue})""").mkString(" * ")} / toBigInt(4))"""

    val r = BigInt(
      "6670795527762621906375444802568692078004471712158714717165576501880318489264376534028344582079701518666593922923767238664173166263805614917588045354008642"
    )

    eval(s"pow($max, 0, $max, 18, 18, DOWN)") shouldBe Left("Overflow on BigInt pow calculation")
    eval(s"pow($max, 0, $max, 0, 0, DOWN)") shouldBe Left("Overflow on BigInt pow calculation")
    eval(s"pow($min, 0, $max, 0, 0, DOWN)") shouldBe Left("Overflow on BigInt pow calculation")
    eval(s"pow($max, 0, $min, 0, 18, DOWN)") shouldBe Left("Overflow on BigInt pow calculation")
    eval(s"pow($one, 18, $min, 0, 18, DOWN)") shouldBe Left("Underflow on BigInt pow calculation")
    eval(s"pow($d18, 18, $min, 0, 18, DOWN)") shouldBe Left("Underflow on BigInt pow calculation")
    eval(s"pow($d18, 18, $max, 0, 18, DOWN)") shouldBe Left("Underflow on BigInt pow calculation")
    eval(s"pow($d18, 18, $e3, 18, 18, DOWN)") shouldBe Right(CONST_BIGINT(BigInt(0)))
    eval(s"pow($d18, 18, $e1, 18, 18, HALFUP)") shouldBe Right(CONST_BIGINT(BigInt(3)))
    eval(s"pow($d19, 18, $e2, 18, 0, DOWN)") shouldBe Right(CONST_BIGINT(r))
  }

  property("sqrt") {
    eval(s"pow($max, 0, toBigInt(5), 1, 18, DOWN)") shouldBe Right(
      CONST_BIGINT(BigInt("81877371507464127617551201542979628307507432471243237061821853600756754782485292915524036944801"))
    )
    eval(s"pow($max, 18, toBigInt(5), 1, 18, DOWN)") shouldBe Right(
      CONST_BIGINT(BigInt("81877371507464127617551201542979628307507432471243237061821853600756754782485292915524"))
    )
    eval(s"sqrt($max, 0, 18, DOWN)")(V6) shouldBe Right(
      CONST_BIGINT(BigInt("81877371507464127617551201542979628307507432471243237061821853600756754782485292915524036944801"))
    )
    eval(s"sqrt($max, 18, 18, DOWN)")(V6) shouldBe Right(
      CONST_BIGINT(BigInt("81877371507464127617551201542979628307507432471243237061821853600756754782485292915524"))
    )
  }
}
