package com.wavesplatform.lang

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.Parser
import com.wavesplatform.lang.v1.Terms.Untyped._
import com.wavesplatform.lang.v1.Terms._
import com.wavesplatform.lang.v1.testing.ScriptGenParser
import fastparse.core.Parsed.{Failure, Success}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector
import scorex.crypto.encode.{Base58 => ScorexBase58}

class ParserTest extends PropSpec with PropertyChecks with Matchers with ScriptGenParser with NoShrink {

  def parse(x: String): EXPR = Parser(x).get.value
  def isParsed(x: String): Boolean = Parser(x) match {
    case Success(_, _)    => true
    case Failure(_, _, _) => false
  }

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
    case BINARY_OP(x: EXPR, op: BINARY_OP_KIND, y: EXPR) =>
      for {
        arg1 <- toString(x)
        arg2 <- toString(y)
      } yield s"($arg1${opsToFunctions(op)}$arg2)"
    case IF(cond: EXPR, x: EXPR, y: EXPR) =>
      for {
        c <- toString(cond)
        t <- toString(x)
        f <- toString(y)
      } yield s"(if($c) then $t else $f)"
    case BLOCK(let: Option[LET], body: EXPR) =>
      let match {
        case Some(l) =>
          for {
            letstr  <- toString(l.value)
            bodyStr <- toString(body)
          } yield s"let ${l.name} = $letstr; $bodyStr"
        case None => toString(body)
      }
    case _ => ???
  }

  def genElementCheck(gen: Gen[EXPR]): Unit = {

    forAll(for {
      expr <- gen
      str  <- toString(expr)
    } yield (expr, str)) {
      case ((expr, str)) =>
        parse(str) shouldBe expr
    }
  }

  property("all types of multiline expressions") {
    val gas = 50
    genElementCheck(CONST_LONGgen)
    genElementCheck(STRgen)
    genElementCheck(REFgen)
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

    List("if", "then", "else", "true", "false", "let").foreach(kv => isParsed(script(kv)) shouldBe false)
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

  property("crypto functions") {
    val hashFunctions = Vector("sha256", "blake2b256", "keccak256")
    val text          = "❤✓☀★☂♞☯☭☢€☎∞❄♫\u20BD=test message"
    val encodedText   = ScorexBase58.encode(text.getBytes)

    for (f <- hashFunctions) {
      parse(
        s"""
           |
           |$f(base58'$encodedText')
           |
       """.stripMargin
      ) shouldBe
        FUNCTION_CALL(
          f,
          List(CONST_BYTEVECTOR(ByteVector(text.getBytes)))
        )
    }
  }
}
