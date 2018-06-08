package com.wavesplatform.lang

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import com.wavesplatform.lang.v1.parser.Expressions._
import com.wavesplatform.lang.v1.parser.{BinaryOperation, Parser}
import com.wavesplatform.lang.v1.testing.ScriptGenParser
import fastparse.core.Parsed.{Failure, Success}
import org.scalacheck.Gen
import org.scalatest.exceptions.TestFailedException
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector
import scorex.crypto.encode.{Base58 => ScorexBase58}

class ParserTest extends PropSpec with PropertyChecks with Matchers with ScriptGenParser with NoShrink {

  private def parseOne(x: String): EXPR = Parser(x) match {
    case Success(r, _) =>
      if (r.size > 1) {
        println(s"Can't parse (len=${x.length}): <START>\n$x\n<END>")
        throw new TestFailedException(s"Expected 1 expression, but got ${r.size}: $r", 0)
      } else r.head
    case e: Failure[Char, String] => catchParseError(x, e)
  }

  private def parseAll(x: String): Seq[EXPR] = Parser(x) match {
    case Success(r, _)            => r
    case e: Failure[Char, String] => catchParseError(x, e)
  }

  private def catchParseError(x: String, e: Failure[Char, String]): Nothing = {
    import e.{index => i}
    println(s"val code1 = new String(Array[Byte](${x.getBytes.mkString(",")}))")
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
    case PART.VALID(_, x)   => PART.VALID(0, 0, x)
    case PART.INVALID(_, x) => PART.INVALID(0, 0, x)
  }

  private def cleanOffsets(expr: EXPR): EXPR = expr match {
    case x: CONST_LONG       => x.copy(position = Pos(0,0))
    case x: REF              => x.copy(position = Pos(0,0), key = cleanOffsets(x.key))
    case x: CONST_STRING     => x.copy(position = Pos(0,0), value = cleanOffsets(x.value))
    case x: CONST_BYTEVECTOR => x.copy(position = Pos(0,0), value = cleanOffsets(x.value))
    case x: TRUE             => x.copy(position = Pos(0,0))
    case x: FALSE            => x.copy(position = Pos(0,0))
    case x: BINARY_OP        => x.copy(position = Pos(0,0), a = cleanOffsets(x.a), b = cleanOffsets(x.b))
    case x: IF               => x.copy(position = Pos(0,0), cond = cleanOffsets(x.cond), ifTrue = cleanOffsets(x.ifTrue), ifFalse = cleanOffsets(x.ifFalse))
    case x: BLOCK            => x.copy(position = Pos(0,0), let = cleanOffsets(x.let), body = cleanOffsets(x.body))
    case x: FUNCTION_CALL    => x.copy(position = Pos(0,0), name = cleanOffsets(x.name), args = x.args.map(cleanOffsets(_)))
    case _                   => throw new NotImplementedError(s"toString for ${expr.getClass.getSimpleName}")
  }

  private def genElementCheck(gen: Gen[EXPR]): Unit = {
    val testGen: Gen[(EXPR, String)] = for {
      expr <- gen
      str  <- toString(expr)
    } yield (expr, str)

    forAll(testGen) {
      case (expr, str) =>
        withClue(str) {
          cleanOffsets(parseOne(str)) shouldBe expr
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
    parseOne("1 == 0 || 3 == 2") shouldBe BINARY_OP(
      AnyPos,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 1), EQ_OP, CONST_LONG(AnyPos, 0)),
      OR_OP,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 3), EQ_OP, CONST_LONG(AnyPos, 2))
    )
    parseOne("3 + 2 > 2 + 1") shouldBe BINARY_OP(
      AnyPos,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 3), SUM_OP, CONST_LONG(AnyPos, 2)),
      GT_OP,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 2), SUM_OP, CONST_LONG(AnyPos, 1))
    )
    parseOne("1 >= 0 || 3 > 2") shouldBe BINARY_OP(
      AnyPos,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 1), GE_OP, CONST_LONG(AnyPos, 0)),
      OR_OP,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 3), GT_OP, CONST_LONG(AnyPos, 2))
    )
  }

  property("bytestr expressions") {
    parseOne("false || sigVerify(base58'333', base58'222', base58'111')") shouldBe BINARY_OP(
      AnyPos,
      FALSE(AnyPos),
      OR_OP,
      FUNCTION_CALL(
        AnyPos,
        PART.VALID(AnyPos, "sigVerify"),
        List(
          CONST_BYTEVECTOR(AnyPos, PART.VALID(AnyPos, ByteVector(ScorexBase58.decode("333").get))),
          CONST_BYTEVECTOR(AnyPos, PART.VALID(AnyPos, ByteVector(ScorexBase58.decode("222").get))),
          CONST_BYTEVECTOR(AnyPos, PART.VALID(AnyPos, ByteVector(ScorexBase58.decode("111").get)))
        )
      )
    )
  }

  property("valid non-empty base58 definition") {
    parseOne("base58'bQbp'") shouldBe CONST_BYTEVECTOR(AnyPos, PART.VALID(AnyPos, ByteVector("foo".getBytes)))
  }

  property("valid empty base58 definition") {
    parseOne("base58''") shouldBe CONST_BYTEVECTOR(AnyPos, PART.VALID(AnyPos, ByteVector.empty))
  }

  property("invalid base58 definition") {
    parseOne("base58' bQbp'") shouldBe CONST_BYTEVECTOR(AnyPos, PART.INVALID(AnyPos, "can't parse Base58 string"))
  }

  property("valid non-empty base64 definition") {
    parseOne("base64'TElLRQ=='") shouldBe CONST_BYTEVECTOR(AnyPos, PART.VALID(AnyPos, ByteVector("LIKE".getBytes)))
  }

  property("valid empty base64 definition") {
    parseOne("base64''") shouldBe CONST_BYTEVECTOR(AnyPos, PART.VALID(AnyPos, ByteVector.empty))
  }

  property("invalid base64 definition") {
    parseOne("base64'mid-size'") shouldBe CONST_BYTEVECTOR(AnyPos, PART.INVALID(AnyPos, "can't parse Base64 string"))
  }

  property("literal too long") {
    import Global.MaxLiteralLength
    val longLiteral = "A" * (MaxLiteralLength + 1)
    val to          = 8 + MaxLiteralLength
    parseOne(s"base58'$longLiteral'") shouldBe
      CONST_BYTEVECTOR(Pos(0, to + 1), PART.INVALID(Pos(8, to), s"base58Decode input exceeds $MaxLiteralLength"))
    parseOne(s"base64'base64:$longLiteral'") shouldBe
      CONST_BYTEVECTOR(Pos(0, to + 8), PART.INVALID(Pos(8, to + 7), s"base58Decode input exceeds $MaxLiteralLength"))
  }

  property("string is consumed fully") {
    parseOne(""" "   fooo    bar" """) shouldBe CONST_STRING(Pos(1, 17), PART.VALID(Pos(2, 16), "   fooo    bar"))
  }

  property("string literal with unicode chars") {
    val stringWithUnicodeChars = "❤✓☀★☂♞☯☭☢€☎∞❄♫\u20BD"

    parseOne(
      s"""
         |
         | "$stringWithUnicodeChars"
         |
       """.stripMargin
    ) shouldBe CONST_STRING(Pos(3, 20), PART.VALID(Pos(4, 19), stringWithUnicodeChars))
  }

  property("string literal with unicode chars in language") {
    parseOne("\"\\u1234\"") shouldBe CONST_STRING(Pos(0, 8), PART.VALID(Pos(1, 7), "ሴ"))
  }

  property("should parse invalid unicode symbols") {
    parseOne("\"\\uqwer\"") shouldBe CONST_STRING(
      AnyPos,
      PART.INVALID(AnyPos, "can't parse 'qwer' as HEX string in '\\uqwer'")
    )
  }

  property("should parse incomplete unicode symbol definition") {
    parseOne("\"\\u12 test\"") shouldBe CONST_STRING(AnyPos, PART.INVALID(AnyPos, "incomplete UTF-8 symbol definition: '\\u12'"))
    parseOne("\"\\u\"") shouldBe CONST_STRING(AnyPos, PART.INVALID(AnyPos, "incomplete UTF-8 symbol definition: '\\u'"))
  }

  property("string literal with special symbols") {
    parseOne("\"\\t\"") shouldBe CONST_STRING(AnyPos, PART.VALID(AnyPos, "\t"))
  }

  property("should parse invalid special symbols") {
    parseOne("\"\\ test\"") shouldBe CONST_STRING(AnyPos, PART.INVALID(AnyPos, "unknown escaped symbol: '\\ '. The valid are \b, \f, \n, \r, \t"))
  }

  property("should parse incomplete special symbols") {
    parseOne("\"foo \\\"") shouldBe CONST_STRING(AnyPos, PART.INVALID(AnyPos, "invalid escaped symbol: '\\'. The valid are \b, \f, \n, \r, \t"))
  }

  property("block: multiline without ;") {
    val s =
      """let q = 1
        |c""".stripMargin
    parseOne(s) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "q"), CONST_LONG(AnyPos, 1), List.empty),
      REF(AnyPos, PART.VALID(AnyPos, "c"))
    )
  }

  property("block: multiline with ; at end of let") {
    val s =
      """let q = 1;
        |c""".stripMargin
    parseOne(s) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "q"), CONST_LONG(AnyPos, 1), List.empty),
      REF(AnyPos, PART.VALID(AnyPos, "c"))
    )
  }

  property("block: multiline with ; at start of body") {
    val s =
      """let q = 1
        |; c""".stripMargin
    parseOne(s) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "q"), CONST_LONG(AnyPos, 1), List.empty),
      REF(AnyPos, PART.VALID(AnyPos, "c"))
    )
  }

  property("block: oneline") {
    val s = "let q = 1; c"
    parseOne(s) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "q"), CONST_LONG(AnyPos, 1), List.empty),
      REF(AnyPos, PART.VALID(AnyPos, "c"))
    )
  }

  property("block: invalid") {
    val s = "let q = 1 c"
    parseOne(s) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "q"), CONST_LONG(AnyPos, 1), List.empty),
      INVALID(AnyPos, "can't find a separator. Did you mean ';' or '\\n' ?", Some(REF(AnyPos, PART.VALID(AnyPos, "c"))))
    )
  }

  property("should parse a binary operation with block operand") {
    val script =
      """let x = a &&
        |let y = 1
        |true
        |true""".stripMargin

    parseOne(script) shouldBe BLOCK(
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
    parseOne(script) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.INVALID(AnyPos, "keywords are restricted: if"), CONST_LONG(AnyPos, 1), Seq.empty),
      TRUE(AnyPos)
    )
  }

  property("reserved keywords are invalid variable names in block: let") {
    val script =
      s"""let let = 1
         |true""".stripMargin
    parseOne(script) shouldBe BLOCK(
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
      parseOne(script) shouldBe BLOCK(
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
    parseOne(script) shouldBe BLOCK(
      0,
      18,
      LET(0, 13, PART.INVALID(4, 9, "keywords are restricted: false"), CONST_LONG(12, 13, 1), Seq.empty),
      TRUE(14, 18)
    )
  }

  property("reserved keywords are invalid variable names in expr: let") {
    val script = "let + 1"
    parseOne(script) shouldBe BLOCK(
      0,
      7,
      LET(0, 3, PART.INVALID(4, 4, "expected a variable's name"), INVALID(4, 4, "expected a value"), List.empty),
      INVALID(3, 3, "expected ';'")
    )
  }

  property("reserved keywords are invalid variable names in expr: if") {
    val script = "if + 1"
    parseOne(script) shouldBe BINARY_OP(
      0,
      6,
      IF(0, 2, INVALID(3, 3, "expected a condition"), INVALID(3, 3, "expected a true branch"), INVALID(3, 3, "expected a false branch")),
      BinaryOperation.SUM_OP,
      CONST_LONG(5, 6, 1)
    )
  }

  property("reserved keywords are invalid variable names in expr: then") {
    val script = "then + 1"
    parseOne(script) shouldBe BINARY_OP(
      0,
      8,
      IF(0, 4, INVALID(0, 0, "expected a condition"), INVALID(5, 5, "expected a true branch's expression"), INVALID(5, 5, "expected a false branch")),
      BinaryOperation.SUM_OP,
      CONST_LONG(7, 8, 1)
    )
  }

  property("reserved keywords are invalid variable names in expr: else") {
    val script = "else + 1"
    parseOne(script) shouldBe BINARY_OP(
      0,
      8,
      IF(0, 4, INVALID(0, 0, "expected a condition"), INVALID(0, 0, "expected a true branch"), INVALID(5, 5, "expected a false branch's expression")),
      BinaryOperation.SUM_OP,
      CONST_LONG(7, 8, 1)
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
    parseOne(script) // gets parsed, but later will fail on type check!
  }

  property("function call") {
    parseOne("FOO(1,2)".stripMargin) shouldBe FUNCTION_CALL(0, 8, PART.VALID(0, 3, "FOO"), List(CONST_LONG(4, 5, 1), CONST_LONG(6, 7, 2)))
    parseOne("FOO(X)".stripMargin) shouldBe FUNCTION_CALL(0, 6, PART.VALID(0, 3, "FOO"), List(REF(4, 5, PART.VALID(4, 5, "X"))))
  }

  property("function call on curly braces") {
    parseOne("{ 1 }(2, 3, 4)") shouldBe FUNCTION_CALL(
      0,
      14,
      PART.INVALID(0, 5, "'CONST_LONG(2,3,1)' is not a function name"),
      List(CONST_LONG(6, 7, 2), CONST_LONG(9, 10, 3), CONST_LONG(12, 13, 4))
    )
  }

  property("function call on round braces") {
    parseOne("( 1 )(2, 3, 4)") shouldBe FUNCTION_CALL(
      0,
      14,
      PART.INVALID(0, 5, "'CONST_LONG(2,3,1)' is not a function name"),
      List(CONST_LONG(6, 7, 2), CONST_LONG(9, 10, 3), CONST_LONG(12, 13, 4))
    )
  }

  property("isDefined") {
    parseOne("isDefined(X)") shouldBe FUNCTION_CALL(0, 12, PART.VALID(0, 9, "isDefined"), List(REF(10, 11, PART.VALID(10, 11, "X"))))
  }

  property("extract") {
    parseOne("if(isDefined(X)) then extract(X) else Y") shouldBe IF(
      0,
      39,
      FUNCTION_CALL(3, 15, PART.VALID(3, 12, "isDefined"), List(REF(13, 14, PART.VALID(13, 14, "X")))),
      FUNCTION_CALL(22, 32, PART.VALID(22, 29, "extract"), List(REF(30, 31, PART.VALID(30, 31, "X")))),
      REF(38, 39, PART.VALID(38, 39, "Y"))
    )
  }

  property("getter: spaces from left") {
    parseOne("xxx  .yyy") shouldBe GETTER(0, 9, REF(0, 3, PART.VALID(0, 3, "xxx")), PART.VALID(6, 9, "yyy"))
  }

  property("getter: spaces from right") {
    parseOne("xxx.  yyy") shouldBe GETTER(0, 9, REF(0, 3, PART.VALID(0, 3, "xxx")), PART.VALID(6, 9, "yyy"))
  }

  property("getter: no spaces") {
    parseOne("xxx.yyy") shouldBe GETTER(0, 7, REF(0, 3, PART.VALID(0, 3, "xxx")), PART.VALID(4, 7, "yyy"))
  }

  property("getter on function result") {
    parseOne("xxx(yyy).zzz") shouldBe GETTER(
      0,
      12,
      FUNCTION_CALL(0, 8, PART.VALID(0, 3, "xxx"), List(REF(4, 7, PART.VALID(4, 7, "yyy")))),
      PART.VALID(9, 12, "zzz")
    )
  }

  property("getter on round braces") {
    parseOne("(xxx(yyy)).zzz") shouldBe GETTER(
      0,
      14,
      FUNCTION_CALL(1, 9, PART.VALID(1, 4, "xxx"), List(REF(5, 8, PART.VALID(5, 8, "yyy")))),
      PART.VALID(11, 14, "zzz")
    )
  }

  property("getter on curly braces") {
    parseOne("{xxx(yyy)}.zzz") shouldBe GETTER(
      0,
      14,
      FUNCTION_CALL(1, 9, PART.VALID(1, 4, "xxx"), List(REF(5, 8, PART.VALID(5, 8, "yyy")))),
      PART.VALID(11, 14, "zzz")
    )
  }

  property("getter on block") {
    parseOne(
      """{
        |  let yyy = aaa(bbb)
        |  xxx(yyy)
        |}.zzz""".stripMargin
    ) shouldBe GETTER(
      0,
      39,
      BLOCK(
        4,
        33,
        LET(
          4,
          22,
          PART.VALID(8, 11, "yyy"),
          FUNCTION_CALL(14, 22, PART.VALID(14, 17, "aaa"), List(REF(18, 21, PART.VALID(18, 21, "bbb")))),
          Seq.empty
        ),
        FUNCTION_CALL(25, 33, PART.VALID(25, 28, "xxx"), List(REF(29, 32, PART.VALID(29, 32, "yyy"))))
      ),
      PART.VALID(36, 39, "zzz")
    )
  }

  property("multiple getters") {
    parseOne("x.y.z") shouldBe GETTER(0, 5, GETTER(0, 3, REF(0, 1, PART.VALID(0, 1, "x")), PART.VALID(2, 3, "y")), PART.VALID(4, 5, "z"))
  }

  property("array accessor") {
    parseOne("x[0]") shouldBe FUNCTION_CALL(0, 4, PART.VALID(1, 4, "getElement"), List(REF(0, 1, PART.VALID(0, 1, "x")), CONST_LONG(2, 3, 0)))
  }

  property("multiple array accessors") {
    parseOne("x[0][1]") shouldBe FUNCTION_CALL(
      0,
      7,
      PART.VALID(4, 7, "getElement"),
      List(
        FUNCTION_CALL(0, 4, PART.VALID(1, 4, "getElement"), List(REF(0, 1, PART.VALID(0, 1, "x")), CONST_LONG(2, 3, 0))),
        CONST_LONG(5, 6, 1)
      )
    )
  }

  property("accessor and getter") {
    parseOne("x[0].y") shouldBe GETTER(
      0,
      6,
      FUNCTION_CALL(0, 4, PART.VALID(1, 4, "getElement"), List(REF(0, 1, PART.VALID(0, 1, "x")), CONST_LONG(2, 3, 0))),
      PART.VALID(5, 6, "y")
    )
  }

  property("getter and accessor") {
    parseOne("x.y[0]") shouldBe FUNCTION_CALL(
      0,
      6,
      PART.VALID(3, 6, "getElement"),
      List(
        GETTER(0, 3, REF(0, 1, PART.VALID(0, 1, "x")), PART.VALID(2, 3, "y")),
        CONST_LONG(4, 5, 0)
      )
    )
  }

  property("function call and accessor") {
    parseOne("x(y)[0]") shouldBe FUNCTION_CALL(
      0,
      7,
      PART.VALID(4, 7, "getElement"),
      List(
        FUNCTION_CALL(0, 4, PART.VALID(0, 1, "x"), List(REF(2, 3, PART.VALID(2, 3, "y")))),
        CONST_LONG(5, 6, 0)
      )
    )
  }

  property("braces in block's let and body") {
    val text =
      """let a = (foo)
        |(bar)""".stripMargin
    parseOne(text) shouldBe BLOCK(
      0,
      19,
      LET(0, 13, PART.VALID(4, 5, "a"), REF(9, 12, PART.VALID(9, 12, "foo")), List.empty),
      REF(15, 18, PART.VALID(15, 18, "bar"))
    )
  }

  property("crypto functions: sha256") {
    val text        = "❤✓☀★☂♞☯☭☢€☎∞❄♫\u20BD=test message"
    val encodedText = ScorexBase58.encode(text.getBytes)

    parseOne(s"sha256(base58'$encodedText')".stripMargin) shouldBe
      FUNCTION_CALL(0, 96, PART.VALID(0, 6, "sha256"), List(CONST_BYTEVECTOR(7, 95, PART.VALID(15, 94, ByteVector(text.getBytes)))))
  }

  property("crypto functions: blake2b256") {
    val text        = "❤✓☀★☂♞☯☭☢€☎∞❄♫\u20BD=test message"
    val encodedText = ScorexBase58.encode(text.getBytes)

    parseOne(s"blake2b256(base58'$encodedText')".stripMargin) shouldBe
      FUNCTION_CALL(0, 100, PART.VALID(0, 10, "blake2b256"), List(CONST_BYTEVECTOR(11, 99, PART.VALID(19, 98, ByteVector(text.getBytes)))))
  }

  property("crypto functions: keccak256") {
    val text        = "❤✓☀★☂♞☯☭☢€☎∞❄♫\u20BD=test message"
    val encodedText = ScorexBase58.encode(text.getBytes)

    parseOne(s"keccak256(base58'$encodedText')".stripMargin) shouldBe
      FUNCTION_CALL(0, 99, PART.VALID(0, 9, "keccak256"), List(CONST_BYTEVECTOR(10, 98, PART.VALID(18, 97, ByteVector(text.getBytes)))))
  }

  property("show parse all input including INVALID") {
    val script =
      """let C = 1
        |foo
        |@~2
        |true""".stripMargin

    parseAll(script) shouldBe Seq(
      BLOCK(0, 13, LET(0, 9, PART.VALID(4, 5, "C"), CONST_LONG(8, 9, 1), Seq.empty), REF(10, 13, PART.VALID(10, 13, "foo"))),
      INVALID(14, 17, "can't parse the expression"),
      TRUE(18, 22)
    )
  }

  property("should parse INVALID expressions at start") {
    val script =
      """@ /
        |let C = 1
        |true""".stripMargin
    parseAll(script) shouldBe Seq(
      INVALID(0, 3, "can't parse the expression"),
      BLOCK(
        4,
        18,
        LET(4, 13, PART.VALID(8, 9, "C"), CONST_LONG(12, 13, 1), List.empty),
        TRUE(14, 18)
      )
    )
  }

  property("should parse INVALID expressions in the middle") {
    val script =
      """let C = 1
        |@ /
        |true""".stripMargin
    parseAll(script) shouldBe Seq(
      BLOCK(
        0,
        13,
        LET(0, 9, PART.VALID(4, 5, "C"), CONST_LONG(8, 9, 1), Seq.empty),
        INVALID(10, 13, "can't parse the expression")
      ),
      TRUE(14, 18)
    )
  }

  property("should parse INVALID expressions at end") {
    val script =
      """let C = 1
        |true
        |~ /""".stripMargin
    parseAll(script) shouldBe Seq(
      BLOCK(0, 14, LET(0, 9, PART.VALID(4, 5, "C"), CONST_LONG(8, 9, 1), Seq.empty), TRUE(10, 14)),
      INVALID(15, 18, "can't parse the expression")
    )
  }

  property("should parse a binary operation without a second operand") {
    val script = "a &&"
    parseOne(script) shouldBe BINARY_OP(
      0,
      4,
      REF(0, 1, PART.VALID(0, 1, "a")),
      AND_OP,
      INVALID(4, 4, "expected a second operator")
    )
  }

  property("simple matching") {
    val code =
      """match tx {
        |  case a: TypeA => 0
        |  case b: TypeB => 1
        |}""".stripMargin
    parseOne(code) shouldBe MATCH(
      0,
      54,
      REF(6, 8, PART.VALID(6, 8, "tx")),
      List(
        MATCH_CASE(13, 31, Some(PART.VALID(18, 19, "a")), List(PART.VALID(21, 26, "TypeA")), CONST_LONG(30, 31, 0)),
        MATCH_CASE(34, 52, Some(PART.VALID(39, 40, "b")), List(PART.VALID(42, 47, "TypeB")), CONST_LONG(51, 52, 1))
      )
    )
  }

  property("multiple union type matching") {
    val code =
      """match tx {
        |  case txa: TypeA => 0
        |  case underscore : TypeB | TypeC => 1
        |}""".stripMargin
    parseOne(code) shouldBe MATCH(
      0,
      74,
      REF(6, 8, PART.VALID(6, 8, "tx")),
      List(
        MATCH_CASE(13, 33, Some(PART.VALID(18, 21, "txa")), List(PART.VALID(23, 28, "TypeA")), CONST_LONG(32, 33, 0)),
        MATCH_CASE(
          36,
          72,
          Some(PART.VALID(41, 51, "underscore")),
          List(PART.VALID(54, 59, "TypeB"), PART.VALID(62, 67, "TypeC")),
          CONST_LONG(71, 72, 1)
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
    parseOne(code) shouldBe MATCH(
      0,
      70,
      BINARY_OP(
        6,
        18,
        FUNCTION_CALL(6, 12, PART.VALID(6, 9, "foo"), List(REF(10, 11, PART.VALID(10, 11, "x")))),
        BinaryOperation.SUM_OP,
        REF(15, 18, PART.VALID(15, 18, "bar"))
      ),
      List(
        MATCH_CASE(23, 40, Some(PART.VALID(28, 29, "x")), List(PART.VALID(30, 35, "TypeA")), CONST_LONG(39, 40, 0)),
        MATCH_CASE(43, 68, Some(PART.VALID(48, 49, "y")), List(PART.VALID(50, 55, "TypeB"), PART.VALID(58, 63, "TypeC")), CONST_LONG(67, 68, 1))
      )
    )
  }

  property("pattern matching - allow shadowing") {
    val code =
      """match p { 
        |  case p: PointA | PointB => true
        |  case _ => false
        |}""".stripMargin
    parseOne(code) shouldBe MATCH(
      0,
      64,
      REF(6, 7, PART.VALID(6, 7, "p")),
      List(
        MATCH_CASE(
          13,
          44,
          Some(PART.VALID(18, 19, "p")),
          List(PART.VALID(21, 27, "PointA"), PART.VALID(30, 36, "PointB")),
          TRUE(40, 44)
        ),
        MATCH_CASE(
          47,
          62,
          None,
          List.empty,
          FALSE(57, 62)
        )
      )
    )
  }

  property("pattern matching with valid case, but no type is defined") {
    parseOne("match tx { case x => 1 } ") shouldBe MATCH(
      0,
      24,
      REF(6, 8, PART.VALID(6, 8, "tx")),
      List(
        MATCH_CASE(
          11,
          22,
          Some(PART.VALID(16, 17, "x")),
          List.empty,
          CONST_LONG(21, 22, 1)
        )
      )
    )
  }

  property("pattern matching with valid case, placeholder instead of variable name") {
    parseOne("match tx { case  _:TypeA => 1 } ") shouldBe MATCH(
      0,
      31,
      REF(6, 8, PART.VALID(6, 8, "tx")),
      List(
        MATCH_CASE(
          11,
          29,
          None,
          List(PART.VALID(19, 24, "TypeA")),
          CONST_LONG(28, 29, 1)
        )
      )
    )
  }

  property("pattern matching with no cases") {
    parseOne("match tx { } ") shouldBe INVALID(0, 12, "pattern matching requires case branches")
  }

  property("pattern matching with invalid case - no variable, type and expr are defined") {
    parseOne("match tx { case => } ") shouldBe MATCH(
      0,
      20,
      REF(6, 8, PART.VALID(6, 8, "tx")),
      List(
        MATCH_CASE(
          11,
          18,
          Some(PART.INVALID(16, 16, "invalid syntax, should be: `case varName: Type => expr` or `case _ => expr`")),
          List.empty,
          INVALID(16, 18, "expected expression")
        )
      )
    )
  }

  property("pattern matching with invalid case - no variable and type are defined") {
    parseOne("match tx { case => 1 } ") shouldBe MATCH(
      0,
      22,
      REF(6, 8, PART.VALID(6, 8, "tx")),
      List(
        MATCH_CASE(
          11,
          20,
          Some(PART.INVALID(16, 16, "invalid syntax, should be: `case varName: Type => expr` or `case _ => expr`")),
          List.empty,
          CONST_LONG(19, 20, 1)
        )
      )
    )
  }

  property("pattern matching with invalid case - no expr is defined") {
    parseOne("match tx { case TypeA => } ") shouldBe MATCH(
      0,
      26,
      REF(6, 8, PART.VALID(6, 8, "tx")),
      List(
        MATCH_CASE(11, 24, Some(PART.VALID(16, 21, "TypeA")), Seq.empty, INVALID(21, 24, "expected expression"))
      )
    )
  }

  property("pattern matching with invalid case - no var is defined") {
    parseOne("match tx { case :TypeA => 1 } ") shouldBe MATCH(
      0,
      29,
      REF(6, 8, PART.VALID(6, 8, "tx")),
      List(
        MATCH_CASE(
          11,
          27,
          Some(PART.INVALID(16, 23, "invalid syntax, should be: `case varName: Type => expr` or `case _ => expr`")),
          Seq.empty,
          CONST_LONG(26, 27, 1)
        )
      )
    )
  }

  property("pattern matching with invalid case - expression in variable definition") {
    parseOne("match tx { case 1 + 1 => 1 } ") shouldBe MATCH(
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
    parseOne("match tx { case _: | => 1 } ") shouldBe MATCH(
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
    parseOne("match tx { case  _: |||| => 1 } ") shouldBe MATCH(
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

    parseOne(script) shouldBe
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

    parseOne(script) shouldBe MATCH(
      0,
      64,
      REF(6, 8, PART.VALID(6, 8, "tx")),
      List(
        MATCH_CASE(
          13,
          48,
          Some(PART.VALID(18, 19, "a")),
          List(),
          BLOCK(
            27,
            48,
            LET(27, 39, PART.VALID(31, 32, "x"), TRUE(35, 39), List.empty),
            BINARY_OP(44, 51, REF(44, 45, PART.VALID(44, 45, "x")), AND_OP, INVALID(51, 51, "expected a second operator"))
          )
        ),
        MATCH_CASE(51, 62, Some(PART.VALID(56, 57, "b")), List.empty, CONST_LONG(61, 62, 1))
      )
    )
  }

  property("if expressions") {
    parseOne("if (10 < 15) then true else false") shouldBe IF(
      AnyPos,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 15), LT_OP, CONST_LONG(AnyPos, 10)),
      TRUE(AnyPos),
      FALSE(AnyPos)
    )
    parseOne("if 10 < 15 then true else false") shouldBe IF(
      AnyPos,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 15), LT_OP, CONST_LONG(AnyPos, 10)),
      TRUE(AnyPos),
      FALSE(AnyPos)
    )
    parseOne(s"""if (10 < 15)
                |then true
                |else false""".stripMargin) shouldBe IF(
      AnyPos,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 15), LT_OP, CONST_LONG(AnyPos, 10)),
      TRUE(AnyPos),
      FALSE(AnyPos)
    )

    parseOne(s"""if 10 < 15
                |then true
                |else false""".stripMargin) shouldBe IF(
      AnyPos,
      BINARY_OP(AnyPos, CONST_LONG(AnyPos, 15), LT_OP, CONST_LONG(AnyPos, 10)),
      TRUE(AnyPos),
      FALSE(AnyPos)
    )
  }

  property("underscore in numbers") {
    parseOne("100_000_000") shouldBe CONST_LONG(AnyPos, 100000000)
  }

  property("comments - the whole line at start") {
    val code =
      """# foo
        |true""".stripMargin

    parseOne(code) shouldBe TRUE(AnyPos)
  }

  property("comments - the whole line at end") {
    val code =
      """true
        |# foo""".stripMargin

    parseOne(code) shouldBe TRUE(0, 4)
  }

  property("comments - block - after let") {
    val s =
      """let # foo
        |  x = true
        |x""".stripMargin
    parseOne(s) shouldBe BLOCK(
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
    parseOne(s) shouldBe BLOCK(
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

    parseOne(code) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "x"), TRUE(AnyPos), List.empty),
      REF(AnyPos, PART.VALID(AnyPos, "x"))
    )
  }

  property("comments - block - between LET and BODY (at end of a line)") {
    val code =
      """let x = true # foo
        |x""".stripMargin

    parseOne(code) shouldBe BLOCK(
      AnyPos,
      LET(AnyPos, PART.VALID(AnyPos, "x"), TRUE(AnyPos), List.empty),
      REF(AnyPos, PART.VALID(AnyPos, "x"))
    )
  }

  property("comments - if - after condition") {
    val code =
      """if 10 < 15 # test
        |then true else false""".stripMargin

    parseOne(code) shouldBe IF(
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
    parseOne(code) shouldBe MATCH(
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
    parseOne(code) shouldBe MATCH(
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
    parseOne(code) shouldBe MATCH(
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
    parseOne(code) shouldBe MATCH(
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
    parseOne(code) shouldBe MATCH(
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
    parseOne(code) shouldBe MATCH(
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
    parseOne(code) shouldBe MATCH(
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

    parseOne(code) shouldBe GETTER(
      AnyPos,
      REF(AnyPos, PART.VALID(AnyPos, "x")),
      PART.VALID(AnyPos, "y")
    )
  }

  property("comments - getter - after dot") {
    val code =
      """x. # foo
        |y""".stripMargin

    parseOne(code) shouldBe GETTER(
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

    parseOne(code) shouldBe FUNCTION_CALL(
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

    parseOne(code) shouldBe FUNCTION_CALL(
      AnyPos,
      PART.VALID(AnyPos, "getElement"),
      List(REF(AnyPos, PART.VALID(AnyPos, "xs")), CONST_LONG(AnyPos, 1))
    )
  }

  property("operations priority") {
    parseOne("a-b+c") shouldBe BINARY_OP(AnyPos,
                                         BINARY_OP(AnyPos, REF(AnyPos, PART.VALID(AnyPos, "a")), SUB_OP, REF(AnyPos, PART.VALID(AnyPos, "b"))),
                                         SUM_OP,
                                         REF(AnyPos, PART.VALID(AnyPos, "c")))
    parseOne("a+b-c") shouldBe BINARY_OP(AnyPos,
                                         BINARY_OP(AnyPos, REF(AnyPos, PART.VALID(AnyPos, "a")), SUM_OP, REF(AnyPos, PART.VALID(AnyPos, "b"))),
                                         SUB_OP,
                                         REF(AnyPos, PART.VALID(AnyPos, "c")))
    parseOne("a+b*c") shouldBe BINARY_OP(AnyPos,
                                         REF(AnyPos, PART.VALID(AnyPos, "a")),
                                         SUM_OP,
                                         BINARY_OP(AnyPos, REF(AnyPos, PART.VALID(AnyPos, "b")), MUL_OP, REF(AnyPos, PART.VALID(AnyPos, "c"))))
    parseOne("a*b-c") shouldBe BINARY_OP(AnyPos,
                                         BINARY_OP(AnyPos, REF(AnyPos, PART.VALID(AnyPos, "a")), MUL_OP, REF(AnyPos, PART.VALID(AnyPos, "b"))),
                                         SUB_OP,
                                         REF(AnyPos, PART.VALID(AnyPos, "c")))
    parseOne("a/b*c") shouldBe BINARY_OP(AnyPos,
                                         BINARY_OP(AnyPos, REF(AnyPos, PART.VALID(AnyPos, "a")), DIV_OP, REF(AnyPos, PART.VALID(AnyPos, "b"))),
                                         MUL_OP,
                                         REF(AnyPos, PART.VALID(AnyPos, "c")))
    parseOne("a<b==c>=d") shouldBe BINARY_OP(
      0,
      9,
      BINARY_OP(0, 3, REF(2, 3, PART.VALID(2, 3, "b")), LT_OP, REF(0, 1, PART.VALID(0, 1, "a"))),
      EQ_OP,
      BINARY_OP(5, 9, REF(5, 6, PART.VALID(5, 6, "c")), GE_OP, REF(8, 9, PART.VALID(8, 9, "d")))
    )
  }

  property("allow name starts with kerword") {
    parseOne("ifx") shouldBe REF(0, 3, PART.VALID(0, 3, "ifx"))
    parseOne("thenx") shouldBe REF(0, 5, PART.VALID(0, 5, "thenx"))
    parseOne("elsex") shouldBe REF(0, 5, PART.VALID(0, 5, "elsex"))
    parseOne("matchx") shouldBe REF(0, 6, PART.VALID(0, 6, "matchx"))
    parseOne("truex") shouldBe REF(0, 5, PART.VALID(0, 5, "truex"))
    parseOne("falsex") shouldBe REF(0, 6, PART.VALID(0, 6, "falsex"))
  }
}
