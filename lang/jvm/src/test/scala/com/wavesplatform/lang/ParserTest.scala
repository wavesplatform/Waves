package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang.Terms.Untyped._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector
import scorex.crypto.encode.{Base58 => ScorexBase58}

class ParserTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  def parse(x: String): EXPR = Parser(x).get.value

  property("simple expressions") {
    parse("10") shouldBe CONST_INT(10)
    parse("10+11") shouldBe BINARY_OP(CONST_INT(10), SUM_OP, CONST_INT(11))
    parse("(10+11)") shouldBe BINARY_OP(CONST_INT(10), SUM_OP, CONST_INT(11))
    parse("(10+11) + 12") shouldBe BINARY_OP(BINARY_OP(CONST_INT(10), SUM_OP, CONST_INT(11)), SUM_OP, CONST_INT(12))
    parse("10   + 11 + 12") shouldBe BINARY_OP(BINARY_OP(CONST_INT(10), SUM_OP, CONST_INT(11)), SUM_OP, CONST_INT(12))
    parse("1+2+3+4+5") shouldBe BINARY_OP(BINARY_OP(BINARY_OP(BINARY_OP(CONST_INT(1), SUM_OP, CONST_INT(2)), SUM_OP, CONST_INT(3)), SUM_OP, CONST_INT(4)), SUM_OP, CONST_INT(5))
    parse("1==1") shouldBe BINARY_OP(CONST_INT(1), EQ_OP, CONST_INT(1))
    parse("true && true") shouldBe BINARY_OP(TRUE, AND_OP, TRUE)
    parse("true || false") shouldBe BINARY_OP(TRUE, OR_OP, FALSE)
    parse("true || (true && false)") shouldBe BINARY_OP(TRUE, OR_OP, BINARY_OP(TRUE, AND_OP, FALSE))
    parse("false || false || false") shouldBe BINARY_OP(BINARY_OP(FALSE, OR_OP, FALSE), OR_OP, FALSE)
    parse("(1>= 0)||(3 >2)") shouldBe BINARY_OP(BINARY_OP(CONST_INT(1), GE_OP, CONST_INT(0)), OR_OP, BINARY_OP(CONST_INT(3), GT_OP, CONST_INT(2)))
  }

  property("priority in binary expressions") {
    parse("1 == 0 || 3 == 2") shouldBe BINARY_OP(BINARY_OP(CONST_INT(1), EQ_OP, CONST_INT(0)), OR_OP, BINARY_OP(CONST_INT(3), EQ_OP, CONST_INT(2)))
    parse("3 + 2 > 2 + 1") shouldBe BINARY_OP(BINARY_OP(CONST_INT(3), SUM_OP, CONST_INT(2)), GT_OP, BINARY_OP(CONST_INT(2), SUM_OP, CONST_INT(1)))
    parse("1 >= 0 || 3 > 2") shouldBe BINARY_OP(BINARY_OP(CONST_INT(1), GE_OP, CONST_INT(0)), OR_OP, BINARY_OP(CONST_INT(3), GT_OP, CONST_INT(2)))
  }

  property("bytestr expressions") {
    parse("checkSig(base58'333', base58'222', base58'111')") shouldBe SIG_VERIFY(
      CONST_BYTEVECTOR(ByteVector(ScorexBase58.decode("333").get)),
      CONST_BYTEVECTOR(ByteVector(ScorexBase58.decode("222").get)),
      CONST_BYTEVECTOR(ByteVector(ScorexBase58.decode("111").get))
    )

    parse("false || checkSig(base58'333', base58'222', base58'111')") shouldBe BINARY_OP(
      FALSE,
      OR_OP,
      SIG_VERIFY(
        CONST_BYTEVECTOR(ByteVector(ScorexBase58.decode("333").get)),
        CONST_BYTEVECTOR(ByteVector(ScorexBase58.decode("222").get)),
        CONST_BYTEVECTOR(ByteVector(ScorexBase58.decode("111").get))
      )
    )
  }

  property("let/ref constructs") {
    parse("""let X = 10;
        |3 > 2
      """.stripMargin) shouldBe BLOCK(Some(LET("X", CONST_INT(10))), BINARY_OP(CONST_INT(3), GT_OP, CONST_INT(2)))

    parse("(let X = 10; 3 > 2)") shouldBe BLOCK(Some(LET("X", CONST_INT(10))), BINARY_OP(CONST_INT(3), GT_OP, CONST_INT(2)))
    parse("(let X = 3 + 2; 3 > 2)") shouldBe BLOCK(Some(LET("X", BINARY_OP(CONST_INT(3), SUM_OP, CONST_INT(2)))), BINARY_OP(CONST_INT(3), GT_OP, CONST_INT(2)))
    parse("(let X = if(true) then true else false; false)") shouldBe BLOCK(Some(LET("X", IF(TRUE, TRUE, FALSE))), FALSE)

    val expr = parse("""let X = 10;
let Y = 11;
X > Y
      """.stripMargin)

    expr shouldBe BLOCK(Some(LET("X", CONST_INT(10))), BLOCK(Some(LET("Y", CONST_INT(11))), BINARY_OP(REF("X"), GT_OP, REF("Y"))))
  }

  property("multiline") {
    parse("""
        |
        |false
        |
        |
      """.stripMargin) shouldBe FALSE

    parse("""let X = 10;
        |
        |true
      """.stripMargin) shouldBe BLOCK(Some(LET("X", CONST_INT(10))), TRUE)
    parse("""let X = 11;
        |true
      """.stripMargin) shouldBe BLOCK(Some(LET("X", CONST_INT(11))), TRUE)

    parse("""
        |
        |let X = 12;
        |
        |3
        | +
        |  2
        |
      """.stripMargin) shouldBe BLOCK(Some(LET("X", CONST_INT(12))), BINARY_OP(CONST_INT(3), SUM_OP, CONST_INT(2)))
  }

  property("if") {
    parse("if(true) then 1 else 2") shouldBe IF(TRUE, CONST_INT(1), CONST_INT(2))
    parse("if(true) then 1 else if(X==Y) then 2 else 3") shouldBe IF(TRUE, CONST_INT(1), IF(BINARY_OP(REF("X"), EQ_OP, REF("Y")), CONST_INT(2), CONST_INT(3)))
    parse("""if ( true )
        |then 1
        |else if(X== Y)
        |     then 2
        |       else 3""".stripMargin) shouldBe IF(TRUE, CONST_INT(1), IF(BINARY_OP(REF("X"), EQ_OP, REF("Y")), CONST_INT(2), CONST_INT(3)))

    parse("if (true) then false else false==false") shouldBe IF(TRUE, FALSE, BINARY_OP(FALSE, EQ_OP, FALSE))

    parse("""if
        |
        |     (true)
        |then let A = 10;
        |  1
        |else if ( X == Y) then 2 else 3""".stripMargin) shouldBe IF(
      TRUE,
      BLOCK(Some(LET("A", CONST_INT(10))), CONST_INT(1)),
      IF(BINARY_OP(REF("X"), EQ_OP, REF("Y")), CONST_INT(2), CONST_INT(3))
    )

  }
  property("get field of ref") {
    parse("XXX.YYY") shouldBe GETTER(REF("XXX"), "YYY")
    parse("""
        |
        | X.Y
        |
      """.stripMargin) shouldBe GETTER(REF("X"), "Y")
  }

  property("multisig sample") {
    val script =
      """
        |
        |let A = base58'PK1PK1PK1PK1PK1'
        |let B = base58'PK2PK2PK2PK2PK2'
        |let C = base58'PK3PK3PK3PK3PK3'
        |
        |let W = TX.BODYBYTES
        |let P = TX.PROOF
        |let V = checkSig(W,P,A)
        |
        |let AC = if(V) then 1 else 0
        |let BC = if(checkSig(TX.BODYBYTES,TX.PROOF,B)) then 1 else 0
        |let CC = if(checkSig(TX.BODYBYTES,TX.PROOF,C)) then 1 else 0
        |
        | AC + BC+ CC >= 2
        |
      """.stripMargin
    parse(script)
  }

  property("isDefined/get") {
    parse("isDefined(X)") shouldBe IS_DEFINED(REF("X"))
    parse("if(isDefined(X)) then get(X) else Y") shouldBe IF(IS_DEFINED(REF("X")), GET(REF("X")), REF("Y"))
  }

  property("EVALUATE patmat") {
    eval("""
          |let MULTICHARVARNAME = Some(500)
          |
          |let Z = match(MULTICHARVARNAME) {
          | case None => 8
          | case Some(B) => B + B
          | }
          |
          | get(Some(Z)) + 1
          |
      """.stripMargin) shouldBe Right(1001)

    eval("""
          |
          | let X = Some(10)
          |
          |match(X) {
          |  case None => 0
          |  case Some(V) => V + V + V + V
          |}
        """.stripMargin) shouldBe Right(40)

    eval("""
          |
          |let X = Some(10)
          |
          |match(X) {
          |  case Some(V) => V + V + V + V
          |  case None => 0
          |}
        """.stripMargin) shouldBe Right(40)
  }

  private def eval(code: String) = {
    val untyped = parse(code)
    val typed   = TypeChecker(TypeChecker.TypeCheckerContext.empty, untyped)
    typed.flatMap(Evaluator(Context.empty, _))
  }
}
