package com.wavesplatform.lang

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.Terms.Untyped._
import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang.testing.ScriptGen
import fastparse.core.Parsed.{Failure, Success}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector
import scorex.crypto.encode.{Base58 => ScorexBase58}

class ParserTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  def parse(x: String): EXPR = Parser(x).get.value
  def isParsed(x: String): Boolean = Parser(x) match {
    case Success(_, _)    => true
    case Failure(_, _, _) => false
  }

  def toString(expr: EXPR): String = expr match {
    case CONST_LONG(x)   => whitespaces.sample.get + s"$x" + whitespaces.sample.get
    case CONST_STRING(x) => whitespaces.sample.get + s"""\"$x\"""" + whitespaces.sample.get
    case TRUE            => whitespaces.sample.get + "true" + whitespaces.sample.get
    case FALSE           => whitespaces.sample.get + "false" + whitespaces.sample.get
    case BINARY_OP(x: EXPR, op: BINARY_OP_KIND, y: EXPR) =>
      op match {
        case SUM_OP => s"(${toString(x)}+${toString(y)})"
        case GT_OP  => s"(${toString(x)}>${toString(y)})"
        case AND_OP => s"(${toString(x)}&&${toString(y)})"
        case OR_OP  => s"(${toString(x)}||${toString(y)})"
        case EQ_OP  => s"(${toString(x)}==${toString(y)})"
        case GE_OP  => s"(${toString(x)}>=${toString(y)})"
      }
    case IF(cond: EXPR, x: EXPR, y: EXPR) => s"(if(${toString(cond)}) then ${toString(x)} else ${toString(y)})"
    case BLOCK(let: Option[LET], body: EXPR) =>
      let match {
        case Some(let) => s"let ${let.name} = ${toString(let.value)}; ${toString(body)}"
        case None      => s"${toString(body)}"
      }
    case _ => ???
  }

  def genElementCheck(gen: Gen[EXPR]): Unit = {
    forAll(gen) { exp =>
      val code = toString(exp)
      parse(code) shouldBe exp
    }
  }

  property("all types of multiline expressions") {
    val gas = 50
    genElementCheck(CONST_LONGgen)
    genElementCheck(STRgen)
    genElementCheck(BOOLgen(gas))
    genElementCheck(SUMgen(gas))
    genElementCheck(EQ_INTgen(gas))
    genElementCheck(INTGen(gas))
    genElementCheck(GEgen(gas))
    genElementCheck(GTgen(gas))
    genElementCheck(ANDgen(gas))
    genElementCheck(ORgen(gas))
    genElementCheck(BLOCKgen(gas))
  }

  property("priority in binary expressions") {
    parse("1 == 0 || 3 == 2") shouldBe BINARY_OP(BINARY_OP(CONST_LONG(1), EQ_OP, CONST_LONG(0)),
                                                 OR_OP,
                                                 BINARY_OP(CONST_LONG(3), EQ_OP, CONST_LONG(2)))
    parse("3 + 2 > 2 + 1") shouldBe BINARY_OP(BINARY_OP(CONST_LONG(3), SUM_OP, CONST_LONG(2)), GT_OP, BINARY_OP(CONST_LONG(2), SUM_OP, CONST_LONG(1)))
    parse("1 >= 0 || 3 > 2") shouldBe BINARY_OP(BINARY_OP(CONST_LONG(1), GE_OP, CONST_LONG(0)), OR_OP, BINARY_OP(CONST_LONG(3), GT_OP, CONST_LONG(2)))
  }

  property("bytestr expressions") {
    parse("false || sigVerify(base58'333', base58'222', base58'111')") shouldBe BINARY_OP(
      FALSE,
      OR_OP,
      FUNCTION_CALL(
        "sigVerify",
        List(
          CONST_BYTEVECTOR(ByteVector(ScorexBase58.decode("333").get)),
          CONST_BYTEVECTOR(ByteVector(ScorexBase58.decode("222").get)),
          CONST_BYTEVECTOR(ByteVector(ScorexBase58.decode("111").get))
        )
      )
    )
  }

  property("let/ref constructs") {
    val expr = parse("""let X = 10;
let Y = 11;
X > Y
      """.stripMargin)

    expr shouldBe BLOCK(Some(LET("X", CONST_LONG(10))), BLOCK(Some(LET("Y", CONST_LONG(11))), BINARY_OP(REF("X"), GT_OP, REF("Y"))))
  }

  property("if") {
    parse("if(true) then 1 else if(X==Y) then 2 else 3") shouldBe IF(TRUE,
                                                                     CONST_LONG(1),
                                                                     IF(BINARY_OP(REF("X"), EQ_OP, REF("Y")), CONST_LONG(2), CONST_LONG(3)))
    parse("""if ( true )
        |then 1
        |else if(X== Y)
        |     then 2
        |       else 3""".stripMargin) shouldBe IF(TRUE, CONST_LONG(1), IF(BINARY_OP(REF("X"), EQ_OP, REF("Y")), CONST_LONG(2), CONST_LONG(3)))

    parse("""if
        |
        |     (true)
        |then let A = 10;
        |  1
        |else if ( X == Y) then 2 else 3""".stripMargin) shouldBe IF(
      TRUE,
      BLOCK(Some(LET("A", CONST_LONG(10))), CONST_LONG(1)),
      IF(BINARY_OP(REF("X"), EQ_OP, REF("Y")), CONST_LONG(2), CONST_LONG(3))
    )

  }

  property("string literal with unicode chars") {
    val stringWithUnicodeChars = "❤✓☀★☂♞☯☭☢€☎∞❄♫\u20BD"

    parse(
      s"""
         |
         | "$stringWithUnicodeChars"
         |
       """.stripMargin
    ) shouldBe CONST_STRING(stringWithUnicodeChars)
  }

  property("reserved keywords are invalid variable names") {
    def script(keyword: String): String =
      s"""
        |
        |let $keyword = 1
        |$keyword + 1
        |
      """.stripMargin

    List("if", "then", "else", "true", "false").foreach(kv => isParsed(script(kv)) shouldBe false)
  }

  property("multisig sample") {
    val script =
      """
        |
        |let A = base58'PK1PK1PK1PK1PK1'
        |let B = base58'PK2PK2PK2PK2PK2'
        |let C = base58'PK3PK3PK3PK3PK3'
        |
        |let W = tx.bodyBytes
        |let P = tx.PROOF
        |let V = sigVerify(W,P,A)
        |
        |let AC = if(V) then 1 else 0
        |let BC = if(sigVerify(tx.bodyBytes,tx.PROOF,B)) then 1 else 0
        |let CC = if(sigVerify(tx.bodyBytes,tx.PROOF,C)) then 1 else 0
        |
        | AC + BC+ CC >= 2
        |
      """.stripMargin
    parse(script) // gets parsed, but later will fail on type check!
  }

  property("function call") {
    parse("FOO(1,2)".stripMargin) shouldBe FUNCTION_CALL("FOO", List(CONST_LONG(1), CONST_LONG(2)))
    parse("FOO(X)".stripMargin) shouldBe FUNCTION_CALL("FOO", List(REF("X")))
  }

  property("isDefined/extract") {
    parse("isDefined(X)") shouldBe FUNCTION_CALL("isDefined", List(REF("X")))
    parse("if(isDefined(X)) then extract(X) else Y") shouldBe IF(FUNCTION_CALL("isDefined", List(REF("X"))),
                                                                 FUNCTION_CALL("extract", List(REF("X"))),
                                                                 REF("Y"))
  }

  property("getter") {
    parse("xxx.yyy") shouldBe GETTER(REF("xxx"), "yyy")
    parse(
      """
        |
        | xxx.yyy
        |
      """.stripMargin
    ) shouldBe GETTER(REF("xxx"), "yyy")

    parse("xxx(yyy).zzz") shouldBe GETTER(FUNCTION_CALL("xxx", List(REF("yyy"))), "zzz")
    parse(
      """
        |
        | xxx(yyy).zzz
        |
      """.stripMargin
    ) shouldBe GETTER(FUNCTION_CALL("xxx", List(REF("yyy"))), "zzz")

    parse("(xxx(yyy)).zzz") shouldBe GETTER(FUNCTION_CALL("xxx", List(REF("yyy"))), "zzz")
    parse(
      """
        |
        | (xxx(yyy)).zzz
        |
      """.stripMargin
    ) shouldBe GETTER(FUNCTION_CALL("xxx", List(REF("yyy"))), "zzz")

    parse("{xxx(yyy)}.zzz") shouldBe GETTER(FUNCTION_CALL("xxx", List(REF("yyy"))), "zzz")
    parse(
      """
        |
        | {
        |   xxx(yyy)
        | }.zzz
        |
      """.stripMargin
    ) shouldBe GETTER(FUNCTION_CALL("xxx", List(REF("yyy"))), "zzz")

    parse(
      """
        |
        | {
        |   let yyy = aaa(bbb)
        |   xxx(yyy)
        | }.zzz
        |
      """.stripMargin
    ) shouldBe GETTER(BLOCK(Some(LET("yyy", FUNCTION_CALL("aaa", List(REF("bbb"))))), FUNCTION_CALL("xxx", List(REF("yyy")))), "zzz")
  }
}
