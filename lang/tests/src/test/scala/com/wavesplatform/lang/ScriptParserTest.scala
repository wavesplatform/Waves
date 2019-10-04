package com.wavesplatform.lang

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import com.wavesplatform.lang.v1.parser.Expressions._
import com.wavesplatform.lang.v1.parser.{BinaryOperation, Expressions, Parser}
import com.wavesplatform.lang.v1.testing.ScriptGenParser
import fastparse.core.Parsed.{Failure, Success}
import org.scalacheck.Gen
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import scorex.crypto.encode.{Base58 => ScorexBase58}

class ScriptParserTest extends PropSpec with PropertyChecks with Matchers with ScriptGenParser with NoShrink {

  private def parse(x: String): EXPR = Parser.parseExpr(x) match {
    case Success(r, _)            => r
    case e: Failure[Char, String] => catchParseError(x, e)
  }

  private def catchParseError(x: String, e: Failure[Char, String]): Nothing = {
    import e.{index => i}
    println(s"val code1 = new String(Array[Byte](${x.getBytes("UTF-8").mkString(",")}))")
    println(s"""val code2 = "${escapedCode(x)}"""")
    println(s"Can't parse (len=${x.length}): <START>\n$x\n<END>\nError: $e\nPosition ($i): '${x.slice(i, i + 1)}'\nTraced:\n${e.extra.traced.fullStack
      .mkString("\n")}")
    throw new TestFailedException("Test failed", 0)
  }

  private def escapedCode(s: String): String =
    s.flatMap {
      case '"'  => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case x    => x.toChar.toString
    }.mkString

  private def cleanOffsets(l: LET): LET =
    l.copy(Pos(0, 0), name = cleanOffsets(l.name), value = cleanOffsets(l.value), types = l.types.map(cleanOffsets(_)))

  private def cleanOffsets[T](p: PART[T]): PART[T] = p match {
    case PART.VALID(_, x)   => PART.VALID(AnyPos, x)
    case PART.INVALID(_, x) => PART.INVALID(AnyPos, x)
  }

  private def cleanOffsets(expr: EXPR): EXPR = expr match {
    case x: CONST_LONG                       => x.copy(position = Pos(0, 0))
    case x: REF                              => x.copy(position = Pos(0, 0), key = cleanOffsets(x.key))
    case x: CONST_STRING                     => x.copy(position = Pos(0, 0), value = cleanOffsets(x.value))
    case x: CONST_BYTESTR                    => x.copy(position = Pos(0, 0), value = cleanOffsets(x.value))
    case x: TRUE                             => x.copy(position = Pos(0, 0))
    case x: FALSE                            => x.copy(position = Pos(0, 0))
    case x: BINARY_OP                        => x.copy(position = Pos(0, 0), a = cleanOffsets(x.a), b = cleanOffsets(x.b))
    case x: IF                               => x.copy(position = Pos(0, 0), cond = cleanOffsets(x.cond), ifTrue = cleanOffsets(x.ifTrue), ifFalse = cleanOffsets(x.ifFalse))
    case x @ BLOCK(_, l: Expressions.LET, _) => x.copy(position = Pos(0, 0), let = cleanOffsets(l), body = cleanOffsets(x.body))
    case x: FUNCTION_CALL                    => x.copy(position = Pos(0, 0), name = cleanOffsets(x.name), args = x.args.map(cleanOffsets(_)))
    case _                                   => throw new NotImplementedError(s"toString for ${expr.getClass.getSimpleName}")
  }

  private def genElementCheck(gen: Gen[EXPR]): Unit = {
    val testGen: Gen[(EXPR, String)] = for {
      expr <- gen
      str  <- toString(expr)
    } yield (expr, str)

    forAll(testGen) {
      case (expr, str) =>
        withClue(str) {
          cleanOffsets(parse(str)) shouldBe expr
        }
    }
  }

  private def multiLineExprTests(tests: (String, Gen[EXPR])*): Unit = tests.foreach {
    case (label, gen) =>
      property(s"multiline expressions: $label") {
        genElementCheck(gen)
      }
  }

  private val gas = 50
  multiLineExprTests(
    "CONST_LONG" -> CONST_LONGgen.map(_._1),
    "STR"        -> STRgen,
    "REF"        -> REFgen,
    "BOOL"       -> BOOLgen(gas).map(_._1),
    "SUM"        -> SUMgen(gas).map(_._1),
    "EQ"         -> EQ_INTgen(gas).map(_._1),
    "INT"        -> INTGen(gas).map(_._1),
    "GE"         -> GEgen(gas).map(_._1),
    "GT"         -> GTgen(gas).map(_._1),
    "AND"        -> ANDgen(gas).map(_._1),
    "OR"         -> ORgen(gas).map(_._1),
    "BLOCK"      -> BLOCKgen(gas)
  )

  property("priority in binary expressions") {
    parse("1 == 0 || 3 == 2") shouldBe BINARY_OP(
      AnyPos,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 1), EQ_OP, CONST_LONG(AnyPos, 0)),
      OR_OP,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 3), EQ_OP, CONST_LONG(AnyPos, 2))
    )
    parse("3 + 2 > 2 + 1") shouldBe BINARY_OP(
      AnyPos,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 3), SUM_OP, CONST_LONG(AnyPos, 2)),
      GT_OP,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 2), SUM_OP, CONST_LONG(AnyPos, 1))
    )
    parse("1 >= 0 || 3 > 2") shouldBe BINARY_OP(
      AnyPos,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 1), GE_OP, CONST_LONG(AnyPos, 0)),
      OR_OP,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 3), GT_OP, CONST_LONG(AnyPos, 2))
    )
  }

  property("bytestr expressions") {
    parse("false || sigVerify(base58'333', base58'222', base58'111')") shouldBe BINARY_OP(
      AnyPos,
      FALSE(AnyPos),
      OR_OP,
      FUNCTION_CALL(
        AnyPos,
        PART.VALID(AnyPos, "sigVerify"),
        List(
          CONST_BYTESTR(AnyPos, PART.VALID(AnyPos, ByteStr(ScorexBase58.decode("333").get))),
          CONST_BYTESTR(AnyPos, PART.VALID(AnyPos, ByteStr(ScorexBase58.decode("222").get))),
          CONST_BYTESTR(AnyPos, PART.VALID(AnyPos, ByteStr(ScorexBase58.decode("111").get)))
        )
      )
    )
  }

  property("valid non-empty base58 definition") {
    parse("base58'bQbp'") shouldBe CONST_BYTESTR(AnyPos, PART.VALID(AnyPos, ByteStr("foo".getBytes("UTF-8"))))
  }

  property("valid empty base58 definition") {
    parse("base58''") shouldBe CONST_BYTESTR(AnyPos, PART.VALID(AnyPos, ByteStr.empty))
  }

  property("invalid base58 definition") {
    parse("base58' bQbp'") shouldBe CONST_BYTESTR(AnyPos, PART.INVALID(AnyPos, "can't parse Base58 string"))
  }

  property("valid non-empty base64 definition") {
    parse("base64'TElLRQ=='") shouldBe CONST_BYTESTR(AnyPos, PART.VALID(AnyPos, ByteStr("LIKE".getBytes("UTF-8"))))
  }

  property("valid empty base64 definition") {
    parse("base64''") shouldBe CONST_BYTESTR(AnyPos, PART.VALID(AnyPos, ByteStr.empty))
  }

  property("invalid base64 definition") {
    parse("base64'mid-size'") shouldBe CONST_BYTESTR(AnyPos, PART.INVALID(AnyPos, "can't parse Base64 string"))
  }

  property("valid empty base16 definition") {
    parse("base16''") shouldBe CONST_BYTESTR(AnyPos, PART.VALID(AnyPos, ByteStr.empty))
  }

  property("valid non-empty base16 definition") {
    parse("base16'0123456789abcdef123456789ABCDEF0ABCDEFfabcde'") shouldBe
        CONST_BYTESTR(AnyPos, PART.VALID(AnyPos, ByteStr(Array[Short](
          0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0x12, 0x34, 0x56, 0x78, 0x9A, 0xBC, 0xDE, 0xF0, 0xAB, 0xCD, 0xEF, 0xfa, 0xbc, 0xde).map(_.toByte))))
  }

  property("invalid base16 definition") {
    parse("base16'mid-size'") shouldBe CONST_BYTESTR(AnyPos, PART.INVALID(AnyPos, "m isn't base16/hex digit"))
    parse("base16'123'") shouldBe CONST_BYTESTR(AnyPos, PART.INVALID(AnyPos, "Need internal bytes number"))
  }

  property("literal too long") {
    import Global.MaxLiteralLength
    val longLiteral = "A" * (MaxLiteralLength + 1)
    val to          = 8 + MaxLiteralLength
    parse(s"base58'$longLiteral'") shouldBe
      CONST_BYTESTR(Pos(0, to + 1), PART.INVALID(Pos(8, to), s"base58Decode input exceeds $MaxLiteralLength"))
  }

  property("string is consumed fully") {
    parse(""" "   fooo    bar" """) shouldBe CONST_STRING(Pos(1, 17), PART.VALID(Pos(2, 16), "   fooo    bar"))
  }

  property("string literal with unicode chars") {
    val stringWithUnicodeChars = "❤✓☀★☂♞☯☭☢€☎∞❄♫\u20BD"

    parse(
      s"""
         |
         | "$stringWithUnicodeChars"
         |
       """.stripMargin
    ) shouldBe CONST_STRING(Pos(3, 20), PART.VALID(Pos(4, 19), stringWithUnicodeChars))
  }

  property("string literal with unicode chars in language") {
    parse("\"\\u1234\"") shouldBe CONST_STRING(Pos(0, 8), PART.VALID(Pos(1, 7), "ሴ"))
  }

  property("should parse invalid unicode symbols") {
    parse("\"\\uqwer\"") shouldBe CONST_STRING(
      AnyPos,
      PART.INVALID(AnyPos, "can't parse 'qwer' as HEX string in '\\uqwer'")
    )
  }

  property("should parse incomplete unicode symbol definition") {
    parse("\"\\u12 test\"") shouldBe CONST_STRING(AnyPos, PART.INVALID(AnyPos, "incomplete UTF-8 symbol definition: '\\u12'"))
    parse("\"\\u\"") shouldBe CONST_STRING(AnyPos, PART.INVALID(AnyPos, "incomplete UTF-8 symbol definition: '\\u'"))
  }

  property("string literal with special symbols") {
    parse("\"\\t\\n\\r\\\\\\\"\"") shouldBe CONST_STRING(AnyPos, PART.VALID(AnyPos, "\t\n\r\\\""))
  }

  property("should parse invalid special symbols") {
    parse("\"\\ test\"") shouldBe CONST_STRING(AnyPos, PART.INVALID(AnyPos, "unknown escaped symbol: '\\ '. The valid are \b, \f, \n, \r, \t"))
  }

  property("block: multiline without ;") {
    val s =
      """let q = 1
        |c""".stripMargin
    parse(s) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "q"), CONST_LONG(AnyPos, 1), List.empty),
      REF(AnyPos, PART.VALID(AnyPos, "c"))
    )
  }

  property("block: func") {
    val s =
      """func q(x: Int, y: Boolean) = { 42 }
        |c""".stripMargin
    parse(s) shouldBe BLOCK(
      AnyPos,
      FUNC(
        AnyPos,
        PART.VALID(AnyPos, "q"),
        Seq((PART.VALID(AnyPos, "x"), Seq((PART.VALID(AnyPos, "Int"), None))), (PART.VALID(AnyPos, "y"), Seq((PART.VALID(AnyPos, "Boolean"), None)))),
        CONST_LONG(AnyPos, 42)
      ),
      REF(AnyPos, PART.VALID(AnyPos, "c"))
    )
  }

  property("block: func with union") {
    val s =
      """func q(x: Int | String) = { 42 }
        |c""".stripMargin
    parse(s) shouldBe BLOCK(
      AnyPos,
      FUNC(AnyPos,
           PART.VALID(AnyPos, "q"),
           Seq((PART.VALID(AnyPos, "x"), Seq((PART.VALID(AnyPos, "Int"), None), (PART.VALID(AnyPos, "String"), None)))),
           CONST_LONG(AnyPos, 42)),
      REF(AnyPos, PART.VALID(AnyPos, "c"))
    )
  }

  property("block: multiline with ; at end of let") {
    val s =
      """let q = 1;
        |c""".stripMargin
    parse(s) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "q"), CONST_LONG(AnyPos, 1), List.empty),
      REF(AnyPos, PART.VALID(AnyPos, "c"))
    )
  }

  property("block: multiline with ; at start of body") {
    val s =
      """let q = 1
        |; c""".stripMargin
    parse(s) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "q"), CONST_LONG(AnyPos, 1), List.empty),
      REF(AnyPos, PART.VALID(AnyPos, "c"))
    )
  }

  property("block: oneline") {
    val s = "let q = 1; c"
    parse(s) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "q"), CONST_LONG(AnyPos, 1), List.empty),
      REF(AnyPos, PART.VALID(AnyPos, "c"))
    )
  }

  property("block: invalid") {
    val s = "let q = 1 c"
    parse(s) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "q"), CONST_LONG(AnyPos, 1), List.empty),
      INVALID(AnyPos, "expected ';'")
    )
  }

  property("should parse a binary operation with block operand") {
    val script =
      """let x = a &&
        |let y = 1
        |true
        |true""".stripMargin

    parse(script) shouldBe BLOCK(
      AnyPos,
      LET(
        AnyPos,
        PART.VALID(AnyPos, "x"),
        BINARY_OP(
          AnyPos,
          REF(AnyPos, PART.VALID(AnyPos, "a")),
          AND_OP,
          BLOCK(AnyPos, LET(AnyPos, PART.VALID(AnyPos, "y"), CONST_LONG(AnyPos, 1), List()), TRUE(AnyPos))
        ),
        List()
      ),
      TRUE(AnyPos)
    )
  }

  property("reserved keywords are invalid variable names in block: if") {
    val script =
      s"""let if = 1
         |true""".stripMargin
    parse(script) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.INVALID(AnyPos, "keywords are restricted: if"), CONST_LONG(AnyPos, 1), Seq.empty),
      TRUE(AnyPos)
    )
  }

  property("reserved keywords are invalid variable names in block: let") {
    val script =
      s"""let let = 1
         |true""".stripMargin
    parse(script) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.INVALID(AnyPos, "keywords are restricted: let"), CONST_LONG(AnyPos, 1), Seq.empty),
      TRUE(AnyPos)
    )
  }

  List("then", "else", "true").foreach { keyword =>
    property(s"reserved keywords are invalid variable names in block: $keyword") {
      val script =
        s"""let ${keyword.padTo(4, " ").mkString} = 1
           |true""".stripMargin
      parse(script) shouldBe BLOCK(
        AnyPos,
        LET(AnyPos, PART.INVALID(AnyPos, s"keywords are restricted: $keyword"), CONST_LONG(AnyPos, 1), Seq.empty),
        TRUE(AnyPos)
      )
    }
  }

  property("reserved keywords are invalid variable names in block: false") {
    val script =
      s"""let false = 1
         |true""".stripMargin
    parse(script) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.INVALID(AnyPos, "keywords are restricted: false"), CONST_LONG(AnyPos, 1), Seq.empty),
      TRUE(AnyPos)
    )
  }

  property("reserved keywords are invalid variable names in expr: let") {
    val script = "let + 1"
    parse(script) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.INVALID(AnyPos, "expected a variable's name"), INVALID(AnyPos, "expected a value"), List.empty),
      INVALID(AnyPos, "expected ';'")
    )
  }

  property("reserved keywords are invalid variable names in expr: if") {
    val script = "if + 1"
    parse(script) shouldBe BINARY_OP(
      AnyPos,
      IF(AnyPos, INVALID(AnyPos, "expected a condition"), INVALID(AnyPos, "expected a true branch"), INVALID(AnyPos, "expected a false branch")),
      BinaryOperation.SUM_OP,
      CONST_LONG(AnyPos, 1)
    )
  }

  property("reserved keywords are invalid variable names in expr: then") {
    val script = "then + 1"
    parse(script) shouldBe BINARY_OP(
      AnyPos,
      IF(AnyPos,
         INVALID(AnyPos, "expected a condition"),
         INVALID(AnyPos, "expected a true branch's expression"),
         INVALID(AnyPos, "expected a false branch")),
      BinaryOperation.SUM_OP,
      CONST_LONG(AnyPos, 1)
    )
  }

  property("reserved keywords are invalid variable names in expr: else") {
    val script = "else + 1"
    parse(script) shouldBe BINARY_OP(
      AnyPos,
      IF(AnyPos,
         INVALID(AnyPos, "expected a condition"),
         INVALID(AnyPos, "expected a true branch"),
         INVALID(AnyPos, "expected a false branch's expression")),
      BinaryOperation.SUM_OP,
      CONST_LONG(AnyPos, 1)
    )
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
    parse("FOO(1,2)".stripMargin) shouldBe FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, "FOO"), List(CONST_LONG(AnyPos, 1), CONST_LONG(AnyPos, 2)))
    parse("FOO(X)".stripMargin) shouldBe FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, "FOO"), List(REF(AnyPos, PART.VALID(AnyPos, "X"))))
  }

  property("isDefined") {
    parse("isDefined(X)") shouldBe FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, "isDefined"), List(REF(AnyPos, PART.VALID(AnyPos, "X"))))
  }

  property("extract") {
    parse("if(isDefined(X)) then extract(X) else Y") shouldBe IF(
      AnyPos,
      FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, "isDefined"), List(REF(AnyPos, PART.VALID(AnyPos, "X")))),
      FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, "extract"), List(REF(AnyPos, PART.VALID(AnyPos, "X")))),
      REF(AnyPos, PART.VALID(AnyPos, "Y"))
    )
  }

  property("getter: spaces from left") {
    parse("xxx  .yyy") shouldBe GETTER(AnyPos, REF(AnyPos, PART.VALID(AnyPos, "xxx")), PART.VALID(AnyPos, "yyy"))
  }

  property("getter: spaces from right") {
    parse("xxx.  yyy") shouldBe GETTER(AnyPos, REF(AnyPos, PART.VALID(AnyPos, "xxx")), PART.VALID(AnyPos, "yyy"))
  }

  property("getter: no spaces") {
    parse("xxx.yyy") shouldBe GETTER(AnyPos, REF(AnyPos, PART.VALID(AnyPos, "xxx")), PART.VALID(AnyPos, "yyy"))
  }

  property("getter on function result") {
    parse("xxx(yyy).zzz") shouldBe GETTER(
      AnyPos,
      FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, "xxx"), List(REF(AnyPos, PART.VALID(AnyPos, "yyy")))),
      PART.VALID(AnyPos, "zzz")
    )
  }

  property("getter on round braces") {
    parse("(xxx(yyy)).zzz") shouldBe GETTER(
      AnyPos,
      FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, "xxx"), List(REF(AnyPos, PART.VALID(AnyPos, "yyy")))),
      PART.VALID(AnyPos, "zzz")
    )
  }

  property("getter on curly braces") {
    parse("{xxx(yyy)}.zzz") shouldBe GETTER(
      AnyPos,
      FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, "xxx"), List(REF(AnyPos, PART.VALID(AnyPos, "yyy")))),
      PART.VALID(AnyPos, "zzz")
    )
  }

  property("getter on block") {
    parse(
      """{
        |  let yyy = aaa(bbb)
        |  xxx(yyy)
        |}.zzz""".stripMargin
    ) shouldBe GETTER(
      AnyPos,
      BLOCK(
        AnyPos,
        LET(
          AnyPos,
          PART.VALID(AnyPos, "yyy"),
          FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, "aaa"), List(REF(AnyPos, PART.VALID(AnyPos, "bbb")))),
          Seq.empty
        ),
        FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, "xxx"), List(REF(AnyPos, PART.VALID(AnyPos, "yyy"))))
      ),
      PART.VALID(AnyPos, "zzz")
    )
  }

  property("multiple getters") {
    parse("x.y.z") shouldBe GETTER(AnyPos, GETTER(AnyPos, REF(AnyPos, PART.VALID(AnyPos, "x")), PART.VALID(AnyPos, "y")), PART.VALID(AnyPos, "z"))
  }

  property("array accessor") {
    parse("x[0]") shouldBe FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, "getElement"), List(REF(AnyPos, PART.VALID(AnyPos, "x")), CONST_LONG(AnyPos, 0)))
  }

  property("multiple array accessors") {
    parse("x[0][1]") shouldBe FUNCTION_CALL(
      AnyPos,
      PART.VALID(AnyPos, "getElement"),
      List(
        FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, "getElement"), List(REF(AnyPos, PART.VALID(AnyPos, "x")), CONST_LONG(AnyPos, 0))),
        CONST_LONG(AnyPos, 1)
      )
    )
  }

  property("accessor and getter") {
    parse("x[0].y") shouldBe GETTER(
      AnyPos,
      FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, "getElement"), List(REF(AnyPos, PART.VALID(AnyPos, "x")), CONST_LONG(AnyPos, 0))),
      PART.VALID(AnyPos, "y")
    )
  }

  property("getter and accessor") {
    parse("x.y[0]") shouldBe FUNCTION_CALL(
      AnyPos,
      PART.VALID(AnyPos, "getElement"),
      List(
        GETTER(AnyPos, REF(AnyPos, PART.VALID(AnyPos, "x")), PART.VALID(AnyPos, "y")),
        CONST_LONG(AnyPos, 0)
      )
    )
  }

  property("function call and accessor") {
    parse("x(y)[0]") shouldBe FUNCTION_CALL(
      AnyPos,
      PART.VALID(AnyPos, "getElement"),
      List(
        FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, "x"), List(REF(AnyPos, PART.VALID(AnyPos, "y")))),
        CONST_LONG(AnyPos, 0)
      )
    )
  }

  property("braces in block's let and body") {
    val text =
      """let a = (foo)
        |(bar)""".stripMargin
    parse(text) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "a"), REF(AnyPos, PART.VALID(AnyPos, "foo")), List.empty),
      REF(AnyPos, PART.VALID(AnyPos, "bar"))
    )
  }

  property("crypto functions: sha256") {
    val text        = "❤✓☀★☂♞☯☭☢€☎∞❄♫\u20BD=test message"
    val encodedText = ScorexBase58.encode(text.getBytes("UTF-8"))

    parse(s"sha256(base58'$encodedText')".stripMargin) shouldBe
      FUNCTION_CALL(Pos(0, 96), PART.VALID(Pos(0, 6), "sha256"), List(CONST_BYTESTR(Pos(7, 95), PART.VALID(Pos(15, 94), ByteStr(text.getBytes("UTF-8"))))))
  }

  property("crypto functions: blake2b256") {
    val text        = "❤✓☀★☂♞☯☭☢€☎∞❄♫\u20BD=test message"
    val encodedText = ScorexBase58.encode(text.getBytes("UTF-8"))

    parse(s"blake2b256(base58'$encodedText')".stripMargin) shouldBe
      FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, "blake2b256"), List(CONST_BYTESTR(AnyPos, PART.VALID(AnyPos, ByteStr(text.getBytes("UTF-8"))))))
  }

  property("crypto functions: keccak256") {
    val text        = "❤✓☀★☂♞☯☭☢€☎∞❄♫\u20BD=test message"
    val encodedText = ScorexBase58.encode(text.getBytes("UTF-8"))

    parse(s"keccak256(base58'$encodedText')".stripMargin) shouldBe
      FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, "keccak256"), List(CONST_BYTESTR(AnyPos, PART.VALID(AnyPos, ByteStr(text.getBytes("UTF-8"))))))
  }

  property("should parse a binary operation without a second operand") {
    val script = "a &&"
    parse(script) shouldBe BINARY_OP(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "a")),
      AND_OP,
      INVALID(AnyPos, "expected a second operator")
    )
  }

  property("simple matching") {
    val code =
      """match tx {
        |  case a: TypeA => 0
        |  case b: TypeB => 1
        |}""".stripMargin
    parse(code) shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "tx")),
      List(
        MATCH_CASE(AnyPos, Some(PART.VALID(AnyPos, "a")), List(PART.VALID(AnyPos, "TypeA")), CONST_LONG(AnyPos, 0)),
        MATCH_CASE(AnyPos, Some(PART.VALID(AnyPos, "b")), List(PART.VALID(AnyPos, "TypeB")), CONST_LONG(AnyPos, 1))
      )
    )
  }

  property("multiple union type matching") {
    val code =
      """match tx {
        |  case txa: TypeA => 0
        |  case underscore : TypeB | TypeC => 1
        |}""".stripMargin
    parse(code) shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "tx")),
      List(
        MATCH_CASE(AnyPos, Some(PART.VALID(AnyPos, "txa")), List(PART.VALID(AnyPos, "TypeA")), CONST_LONG(AnyPos, 0)),
        MATCH_CASE(
          AnyPos,
          Some(PART.VALID(AnyPos, "underscore")),
          List(PART.VALID(AnyPos, "TypeB"), PART.VALID(AnyPos, "TypeC")),
          CONST_LONG(AnyPos, 1)
        )
      )
    )
  }

  property("matching expression") {
    val code =
      """match foo(x) + bar {
        |  case x:TypeA => 0
        |  case y:TypeB | TypeC => 1
        |}""".stripMargin
    parse(code) shouldBe MATCH(
      AnyPos,
      BINARY_OP(
        AnyPos,
        FUNCTION_CALL(AnyPos, PART.VALID(AnyPos, "foo"), List(REF(AnyPos, PART.VALID(AnyPos, "x")))),
        BinaryOperation.SUM_OP,
        REF(AnyPos, PART.VALID(AnyPos, "bar"))
      ),
      List(
        MATCH_CASE(AnyPos, Some(PART.VALID(AnyPos, "x")), List(PART.VALID(AnyPos, "TypeA")), CONST_LONG(AnyPos, 0)),
        MATCH_CASE(AnyPos, Some(PART.VALID(AnyPos, "y")), List(PART.VALID(AnyPos, "TypeB"), PART.VALID(AnyPos, "TypeC")), CONST_LONG(AnyPos, 1))
      )
    )
  }

  property("pattern matching - allow shadowing") {
    val code =
      """match p { 
        |  case p: PointA | PointB => true
        |  case _ => false
        |}""".stripMargin
    parse(code) shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "p")),
      List(
        MATCH_CASE(
          AnyPos,
          Some(PART.VALID(AnyPos, "p")),
          List(PART.VALID(AnyPos, "PointA"), PART.VALID(AnyPos, "PointB")),
          TRUE(AnyPos)
        ),
        MATCH_CASE(
          AnyPos,
          None,
          List.empty,
          FALSE(AnyPos)
        )
      )
    )
  }

  property("pattern matching with valid case, but no type is defined") {
    parse("match tx { case x => 1 } ") shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "tx")),
      List(
        MATCH_CASE(
          AnyPos,
          Some(PART.VALID(AnyPos, "x")),
          List.empty,
          CONST_LONG(AnyPos, 1)
        )
      )
    )
  }

  property("pattern matching with valid case, placeholder instead of variable name") {
    parse("match tx { case  _:TypeA => 1 } ") shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "tx")),
      List(
        MATCH_CASE(
          AnyPos,
          None,
          List(PART.VALID(AnyPos, "TypeA")),
          CONST_LONG(AnyPos, 1)
        )
      )
    )
  }

  property("pattern matching with no cases") {
    parse("match tx { } ") shouldBe INVALID(AnyPos, "pattern matching requires case branches")
  }

  property("pattern matching with invalid case - no variable, type and expr are defined") {
    parse("match tx { case => } ") shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "tx")),
      List(
        MATCH_CASE(
          AnyPos,
          Some(PART.INVALID(AnyPos, "invalid syntax, should be: `case varName: Type => expr` or `case _ => expr`")),
          List.empty,
          INVALID(AnyPos, "expected expression")
        )
      )
    )
  }

  property("pattern matching with invalid case - no variable and type are defined") {
    parse("match tx { case => 1 } ") shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "tx")),
      List(
        MATCH_CASE(
          AnyPos,
          Some(PART.INVALID(AnyPos, "invalid syntax, should be: `case varName: Type => expr` or `case _ => expr`")),
          List.empty,
          CONST_LONG(AnyPos, 1)
        )
      )
    )
  }

  property("pattern matching with invalid case - no expr is defined") {
    parse("match tx { case TypeA => } ") shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "tx")),
      List(
        MATCH_CASE(AnyPos, Some(PART.VALID(AnyPos, "TypeA")), Seq.empty, INVALID(AnyPos, "expected expression"))
      )
    )
  }

  property("pattern matching with invalid case - no var is defined") {
    parse("match tx { case :TypeA => 1 } ") shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "tx")),
      List(
        MATCH_CASE(
          AnyPos,
          Some(PART.INVALID(AnyPos, "invalid syntax, should be: `case varName: Type => expr` or `case _ => expr`")),
          Seq.empty,
          CONST_LONG(AnyPos, 1)
        )
      )
    )
  }

  property("pattern matching with invalid case - expression in variable definition") {
    parse("match tx { case 1 + 1 => 1 } ") shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "tx")),
      List(
        MATCH_CASE(
          AnyPos,
          Some(PART.INVALID(AnyPos, "invalid syntax, should be: `case varName: Type => expr` or `case _ => expr`")),
          List.empty,
          CONST_LONG(AnyPos, 1)
        )
      )
    )
  }

  property("pattern matching with default case - no type is defined, one separator") {
    parse("match tx { case _: | => 1 } ") shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "tx")),
      List(
        MATCH_CASE(
          AnyPos,
          None,
          Seq(PART.INVALID(AnyPos, "the type for variable should be specified: `case varName: Type => expr`")),
          CONST_LONG(AnyPos, 1)
        )
      )
    )
  }

  property("pattern matching with default case - no type is defined, multiple separators") {
    parse("match tx { case  _: |||| => 1 } ") shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "tx")),
      List(
        MATCH_CASE(
          AnyPos,
          None,
          Seq(PART.INVALID(AnyPos, "the type for variable should be specified: `case varName: Type => expr`")),
          CONST_LONG(AnyPos, 1)
        )
      )
    )
  }

  property("pattern matching - incomplete binary operation") {
    val script =
      """match tx {
        |  case a => true &&
        |  case b => 1
        |}""".stripMargin

    parse(script) shouldBe
      MATCH(
        AnyPos,
        REF(AnyPos, PART.VALID(AnyPos, "tx")),
        List(
          MATCH_CASE(
            AnyPos,
            Some(PART.VALID(AnyPos, "a")),
            List(),
            BINARY_OP(AnyPos, TRUE(AnyPos), AND_OP, INVALID(AnyPos, "expected a second operator"))
          ),
          MATCH_CASE(AnyPos, Some(PART.VALID(AnyPos, "b")), List(), CONST_LONG(AnyPos, 1))
        )
      )
  }

  property("pattern matching - incomplete binary operation with block") {
    val script =
      """match tx {
        |  case a =>
        |    let x = true
        |    x &&
        |  case b => 1
        |}""".stripMargin

    parse(script) shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "tx")),
      List(
        MATCH_CASE(
          AnyPos,
          Some(PART.VALID(AnyPos, "a")),
          List(),
          BLOCK(
            AnyPos,
            LET(AnyPos, PART.VALID(AnyPos, "x"), TRUE(AnyPos), List.empty),
            BINARY_OP(AnyPos, REF(AnyPos, PART.VALID(AnyPos, "x")), AND_OP, INVALID(AnyPos, "expected a second operator"))
          )
        ),
        MATCH_CASE(AnyPos, Some(PART.VALID(AnyPos, "b")), List.empty, CONST_LONG(AnyPos, 1))
      )
    )
  }

  property("if expressions") {
    parse("if (10 < 15) then true else false") shouldBe IF(
      AnyPos,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 15), LT_OP, CONST_LONG(AnyPos, 10)),
      TRUE(AnyPos),
      FALSE(AnyPos)
    )
    parse("if 10 < 15 then true else false") shouldBe IF(
      AnyPos,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 15), LT_OP, CONST_LONG(AnyPos, 10)),
      TRUE(AnyPos),
      FALSE(AnyPos)
    )
    parse(s"""if (10 < 15)
                |then true
                |else false""".stripMargin) shouldBe IF(
      AnyPos,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 15), LT_OP, CONST_LONG(AnyPos, 10)),
      TRUE(AnyPos),
      FALSE(AnyPos)
    )

    parse(s"""if 10 < 15
                |then true
                |else false""".stripMargin) shouldBe IF(
      AnyPos,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 15), LT_OP, CONST_LONG(AnyPos, 10)),
      TRUE(AnyPos),
      FALSE(AnyPos)
    )
  }

  property("underscore in numbers") {
    parse("100_000_000") shouldBe CONST_LONG(AnyPos, 100000000)
  }

  property("comments - the whole line at start") {
    val code =
      """# foo
        |true""".stripMargin

    parse(code) shouldBe TRUE(AnyPos)
  }

  property("comments - the whole line at end") {
    val code =
      """true
        |# foo""".stripMargin

    parse(code) shouldBe TRUE(AnyPos)
  }

  property("comments - block - after let") {
    val s =
      """let # foo
        |  x = true
        |x""".stripMargin
    parse(s) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "x"), TRUE(AnyPos), List.empty),
      REF(AnyPos, PART.VALID(AnyPos, "x"))
    )
  }

  property("comments - block - before assignment") {
    val s =
      """let x # foo
        |  = true
        |x""".stripMargin
    parse(s) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "x"), TRUE(AnyPos), List.empty),
      REF(AnyPos, PART.VALID(AnyPos, "x"))
    )
  }

  property("comments - block - between LET and BODY (full line)") {
    val code =
      """let x = true
        |# foo
        |x""".stripMargin

    parse(code) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "x"), TRUE(AnyPos), List.empty),
      REF(AnyPos, PART.VALID(AnyPos, "x"))
    )
  }

  property("comments - block - between LET and BODY (at end of a line)") {
    val code =
      """let x = true # foo
        |x""".stripMargin

    parse(code) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "x"), TRUE(AnyPos), List.empty),
      REF(AnyPos, PART.VALID(AnyPos, "x"))
    )
  }

  property("comments - if - after condition") {
    val code =
      """if 10 < 15 # test
        |then true else false""".stripMargin

    parse(code) shouldBe IF(
      AnyPos,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 15), LT_OP, CONST_LONG(AnyPos, 10)),
      TRUE(AnyPos),
      FALSE(AnyPos)
    )
  }

  property("comments - pattern matching - after case") {
    val code =
      """match p {
        |  case # test
        |       p: PointA | PointB => true
        |  case _ => false
        |}""".stripMargin
    parse(code) shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "p")),
      List(
        MATCH_CASE(
          AnyPos,
          Some(PART.VALID(AnyPos, "p")),
          List(PART.VALID(AnyPos, "PointA"), PART.VALID(AnyPos, "PointB")),
          TRUE(AnyPos)
        ),
        MATCH_CASE(
          AnyPos,
          None,
          List.empty,
          FALSE(AnyPos)
        )
      )
    )
  }

  property("comments - pattern matching - after variable") {
    val code =
      """match p {
        |  case p # test
        |       : PointA
        |       | PointB => true
        |  case _ => false
        |}""".stripMargin
    parse(code) shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "p")),
      List(
        MATCH_CASE(
          AnyPos,
          Some(PART.VALID(AnyPos, "p")),
          List(PART.VALID(AnyPos, "PointA"), PART.VALID(AnyPos, "PointB")),
          TRUE(AnyPos)
        ),
        MATCH_CASE(
          AnyPos,
          None,
          List.empty,
          FALSE(AnyPos)
        )
      )
    )
  }

  property("comments - pattern matching - before types") {
    val code =
      """match p {
        |  case p: # test
        |         PointA | PointB => true
        |  case _ => false
        |}""".stripMargin
    parse(code) shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "p")),
      List(
        MATCH_CASE(
          AnyPos,
          Some(PART.VALID(AnyPos, "p")),
          List(PART.VALID(AnyPos, "PointA"), PART.VALID(AnyPos, "PointB")),
          TRUE(AnyPos)
        ),
        MATCH_CASE(
          AnyPos,
          None,
          List.empty,
          FALSE(AnyPos)
        )
      )
    )
  }

  property("comments - pattern matching - before a value's block") {
    val code =
      """match p {
        |  case p: PointA | PointB # test
        |         => true
        |  case _ => false
        |}""".stripMargin
    parse(code) shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "p")),
      List(
        MATCH_CASE(
          AnyPos,
          Some(PART.VALID(AnyPos, "p")),
          List(PART.VALID(AnyPos, "PointA"), PART.VALID(AnyPos, "PointB")),
          TRUE(AnyPos)
        ),
        MATCH_CASE(
          AnyPos,
          None,
          List.empty,
          FALSE(AnyPos)
        )
      )
    )
  }

  property("comments - pattern matching - in a type definition - 1") {
    val code =
      """match p {
        |  case p : PointA # foo
        |         | PointB # bar
        |         => true
        |  case _ => false
        |}""".stripMargin
    parse(code) shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "p")),
      List(
        MATCH_CASE(
          AnyPos,
          Some(PART.VALID(AnyPos, "p")),
          List(PART.VALID(AnyPos, "PointA"), PART.VALID(AnyPos, "PointB")),
          TRUE(AnyPos)
        ),
        MATCH_CASE(
          AnyPos,
          None,
          List.empty,
          FALSE(AnyPos)
        )
      )
    )
  }

  property("comments - pattern matching - in a type definition - 2") {
    val code =
      """match p {
        |  case p: PointA | # foo
        |          PointB   # bar
        |         => true
        |  case _ => false
        |}""".stripMargin
    parse(code) shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "p")),
      List(
        MATCH_CASE(
          AnyPos,
          Some(PART.VALID(AnyPos, "p")),
          List(PART.VALID(AnyPos, "PointA"), PART.VALID(AnyPos, "PointB")),
          TRUE(AnyPos)
        ),
        MATCH_CASE(
          AnyPos,
          None,
          List.empty,
          FALSE(AnyPos)
        )
      )
    )
  }

  property("comments - pattern matching - between cases") {
    val code =
      """match p {
        |  # foo
        |  case p: PointA | PointB => true
        |  # bar
        |  case _ => false
        |  # baz
        |}""".stripMargin
    parse(code) shouldBe MATCH(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "p")),
      List(
        MATCH_CASE(
          AnyPos,
          Some(PART.VALID(AnyPos, "p")),
          List(PART.VALID(AnyPos, "PointA"), PART.VALID(AnyPos, "PointB")),
          TRUE(AnyPos)
        ),
        MATCH_CASE(
          AnyPos,
          None,
          List.empty,
          FALSE(AnyPos)
        )
      )
    )
  }

  property("comments - getter - before dot") {
    val code =
      """x # foo
        |.y""".stripMargin

    parse(code) shouldBe GETTER(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "x")),
      PART.VALID(AnyPos, "y")
    )
  }

  property("comments - getter - after dot") {
    val code =
      """x. # foo
        |y""".stripMargin

    parse(code) shouldBe GETTER(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "x")),
      PART.VALID(AnyPos, "y")
    )
  }

  property("comments - function call") {
    val code =
      """f(
        | # foo
        | 1 # bar
        | # baz
        | , 2
        | # quux
        |)""".stripMargin

    parse(code) shouldBe FUNCTION_CALL(
      AnyPos,
      PART.VALID(AnyPos, "f"),
      List(CONST_LONG(AnyPos, 1), CONST_LONG(AnyPos, 2))
    )
  }

  property("comments - array") {
    val code =
      """xs[
        | # foo
        | 1
        | # bar
        |]""".stripMargin

    parse(code) shouldBe FUNCTION_CALL(
      AnyPos,
      PART.VALID(AnyPos, "getElement"),
      List(REF(AnyPos, PART.VALID(AnyPos, "xs")), CONST_LONG(AnyPos, 1))
    )
  }

  property("comments - in func and around") {
    val code =
      """
        |
        | # comment 1
        | func foo() = # comment 2
        | { # more comments
        |   throw()
        | } # comment 3
        |
        | foo()
        |
        """.stripMargin

    parse(code)
  }

  property("operations priority") {
    parse("a-b+c") shouldBe BINARY_OP(AnyPos,
                                      BINARY_OP(AnyPos, REF(AnyPos, PART.VALID(AnyPos, "a")), SUB_OP, REF(AnyPos, PART.VALID(AnyPos, "b"))),
                                      SUM_OP,
                                      REF(AnyPos, PART.VALID(AnyPos, "c")))
    parse("a+b-c") shouldBe BINARY_OP(AnyPos,
                                      BINARY_OP(AnyPos, REF(AnyPos, PART.VALID(AnyPos, "a")), SUM_OP, REF(AnyPos, PART.VALID(AnyPos, "b"))),
                                      SUB_OP,
                                      REF(AnyPos, PART.VALID(AnyPos, "c")))
    parse("a+b*c") shouldBe BINARY_OP(AnyPos,
                                      REF(AnyPos, PART.VALID(AnyPos, "a")),
                                      SUM_OP,
                                      BINARY_OP(AnyPos, REF(AnyPos, PART.VALID(AnyPos, "b")), MUL_OP, REF(AnyPos, PART.VALID(AnyPos, "c"))))
    parse("a*b-c") shouldBe BINARY_OP(AnyPos,
                                      BINARY_OP(AnyPos, REF(AnyPos, PART.VALID(AnyPos, "a")), MUL_OP, REF(AnyPos, PART.VALID(AnyPos, "b"))),
                                      SUB_OP,
                                      REF(AnyPos, PART.VALID(AnyPos, "c")))
    parse("a/b*c") shouldBe BINARY_OP(AnyPos,
                                      BINARY_OP(AnyPos, REF(AnyPos, PART.VALID(AnyPos, "a")), DIV_OP, REF(AnyPos, PART.VALID(AnyPos, "b"))),
                                      MUL_OP,
                                      REF(AnyPos, PART.VALID(AnyPos, "c")))

    parse("a<b==c>=d") shouldBe BINARY_OP(
      AnyPos,
      BINARY_OP(AnyPos,
                BINARY_OP(AnyPos, REF(AnyPos, PART.VALID(AnyPos, "b")), EQ_OP, REF(AnyPos, PART.VALID(AnyPos, "c"))),
                LT_OP,
                REF(AnyPos, PART.VALID(AnyPos, "a"))),
      GE_OP,
      REF(AnyPos, PART.VALID(AnyPos, "d"))
    )
  }

  property("allow name starts with kerword") {
    parse("ifx") shouldBe REF(AnyPos, PART.VALID(AnyPos, "ifx"))
    parse("thenx") shouldBe REF(AnyPos, PART.VALID(AnyPos, "thenx"))
    parse("elsex") shouldBe REF(AnyPos, PART.VALID(AnyPos, "elsex"))
    parse("matchx") shouldBe REF(AnyPos, PART.VALID(AnyPos, "matchx"))
    parse("truex") shouldBe REF(AnyPos, PART.VALID(AnyPos, "truex"))
    parse("falsex") shouldBe REF(AnyPos, PART.VALID(AnyPos, "falsex"))
  }

  property("parser StackOverflow check") {
    val depth = 10000
    val lastStmt = (1 to depth).foldLeft("i0") { (acc, i) =>
      s"$acc + i$i"
    }
    val manyLets = (1 to depth).foldLeft("let i0 = 1") { (acc, i) =>
      s"$acc\nlet i$i = 1"
    }
    val script = s"$manyLets\n$lastStmt"

    Parser.parseExpr(script) shouldBe an[Success[_, _, _]]
  }
}
