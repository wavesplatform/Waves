package com.wavesplatform.lang

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.parser.BinaryOperation._
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
    println(s"val codeInBytes = new String(Array[Byte](${x.getBytes.mkString(",")}))")
    println(s"""val codeInStr = "${escapedCode(x)}"""")
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
    l.copy(start = 0, end = 0, name = cleanOffsets(l.name), value = cleanOffsets(l.value), types = l.types.map(cleanOffsets(_)))

  private def cleanOffsets[T](p: PART[T]): PART[T] = p match {
    case PART.VALID(_, _, x)   => PART.VALID(0, 0, x)
    case PART.INVALID(_, _, x) => PART.INVALID(0, 0, x)
  }

  private def cleanOffsets(expr: EXPR): EXPR = expr match {
    case x: CONST_LONG       => x.copy(start = 0, end = 0)
    case x: REF              => x.copy(start = 0, end = 0, key = cleanOffsets(x.key))
    case x: CONST_STRING     => x.copy(start = 0, end = 0, value = cleanOffsets(x.value))
    case x: CONST_BYTEVECTOR => x.copy(start = 0, end = 0, value = cleanOffsets(x.value))
    case x: TRUE             => x.copy(start = 0, end = 0)
    case x: FALSE            => x.copy(start = 0, end = 0)
    case x: BINARY_OP        => x.copy(start = 0, end = 0, a = cleanOffsets(x.a), b = cleanOffsets(x.b))
    case x: IF               => x.copy(start = 0, end = 0, cond = cleanOffsets(x.cond), ifTrue = cleanOffsets(x.ifTrue), ifFalse = cleanOffsets(x.ifFalse))
    case x: BLOCK            => x.copy(start = 0, end = 0, let = cleanOffsets(x.let), body = cleanOffsets(x.body))
    case x: FUNCTION_CALL    => x.copy(start = 0, end = 0, name = cleanOffsets(x.name), args = x.args.map(cleanOffsets(_)))
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
      0,
      16,
      BINARY_OP(0, 6, CONST_LONG(0, 1, 1), EQ_OP, CONST_LONG(5, 6, 0)),
      OR_OP,
      BINARY_OP(10, 16, CONST_LONG(10, 11, 3), EQ_OP, CONST_LONG(15, 16, 2))
    )
    parseOne("3 + 2 > 2 + 1") shouldBe BINARY_OP(
      0,
      13,
      BINARY_OP(0, 5, CONST_LONG(0, 1, 3), SUM_OP, CONST_LONG(4, 5, 2)),
      GT_OP,
      BINARY_OP(8, 13, CONST_LONG(8, 9, 2), SUM_OP, CONST_LONG(12, 13, 1))
    )
    parseOne("1 >= 0 || 3 > 2") shouldBe BINARY_OP(
      0,
      15,
      BINARY_OP(0, 6, CONST_LONG(0, 1, 1), GE_OP, CONST_LONG(5, 6, 0)),
      OR_OP,
      BINARY_OP(10, 15, CONST_LONG(10, 11, 3), GT_OP, CONST_LONG(14, 15, 2))
    )
  }

  property("bytestr expressions") {
    parseOne("false || sigVerify(base58'333', base58'222', base58'111')") shouldBe BINARY_OP(
      0,
      57,
      FALSE(0, 5),
      OR_OP,
      FUNCTION_CALL(
        9,
        57,
        PART.VALID(9, 18, "sigVerify"),
        List(
          CONST_BYTEVECTOR(19, 30, PART.VALID(27, 29, ByteVector(ScorexBase58.decode("333").get))),
          CONST_BYTEVECTOR(32, 43, PART.VALID(40, 42, ByteVector(ScorexBase58.decode("222").get))),
          CONST_BYTEVECTOR(45, 56, PART.VALID(53, 55, ByteVector(ScorexBase58.decode("111").get)))
        )
      )
    )
  }

  property("valid non-empty base58 definition") {
    parseOne("base58'bQbp'") shouldBe CONST_BYTEVECTOR(0, 12, PART.VALID(8, 11, ByteVector("foo".getBytes)))
  }

  property("valid empty base58 definition") {
    parseOne("base58''") shouldBe CONST_BYTEVECTOR(0, 8, PART.VALID(8, 7, ByteVector.empty))
  }

  property("invalid base58 definition") {
    parseOne("base58' bQbp'") shouldBe CONST_BYTEVECTOR(0, 13, PART.INVALID(8, 12, "can't parse Base58 string"))
  }

  property("valid non-empty base64 definition") {
    parseOne("base64'TElLRQ=='") shouldBe CONST_BYTEVECTOR(0, 16, PART.VALID(8, 15, ByteVector("LIKE".getBytes)))
  }

  property("valid empty base64 definition") {
    parseOne("base64''") shouldBe CONST_BYTEVECTOR(0, 8, PART.VALID(8, 7, ByteVector.empty))
  }

  property("invalid base64 definition") {
    parseOne("base64'mid-size'") shouldBe CONST_BYTEVECTOR(0, 16, PART.INVALID(8, 15, "can't parse Base64 string"))
  }

  property("literal too long") {
    import Global.MaxLiteralLength
    val longLiteral = "A" * (MaxLiteralLength + 1)
    val to          = 8 + MaxLiteralLength
    parseOne(s"base58'$longLiteral'") shouldBe
      CONST_BYTEVECTOR(0, to + 1, PART.INVALID(8, to, s"base58Decode input exceeds $MaxLiteralLength"))
    parseOne(s"base64'base64:$longLiteral'") shouldBe
      CONST_BYTEVECTOR(0, to + 8, PART.INVALID(8, to + 7, s"base58Decode input exceeds $MaxLiteralLength"))
  }

  property("string is consumed fully") {
    parseOne(""" "   fooo    bar" """) shouldBe CONST_STRING(1, 17, PART.VALID(2, 16, "   fooo    bar"))
  }

  property("string literal with unicode chars") {
    val stringWithUnicodeChars = "❤✓☀★☂♞☯☭☢€☎∞❄♫\u20BD"

    parseOne(
      s"""
         |
         | "$stringWithUnicodeChars"
         |
       """.stripMargin
    ) shouldBe CONST_STRING(3, 20, PART.VALID(4, 19, stringWithUnicodeChars))
  }

  property("string literal with unicode chars in language") {
    parseOne("\"\\u1234\"") shouldBe CONST_STRING(0, 8, PART.VALID(1, 7, "ሴ"))
  }

  property("should parse invalid unicode symbols") {
    parseOne("\"\\uqwer\"") shouldBe CONST_STRING(
      0,
      8,
      PART.INVALID(1, 7, "can't parse 'qwer' as HEX string in '\\uqwer'")
    )
  }

  property("should parse incomplete unicode symbol definition") {
    parseOne("\"\\u12 test\"") shouldBe CONST_STRING(0, 11, PART.INVALID(1, 10, "incomplete UTF-8 symbol definition: '\\u12'"))
    parseOne("\"\\u\"") shouldBe CONST_STRING(0, 4, PART.INVALID(1, 3, "incomplete UTF-8 symbol definition: '\\u'"))
  }

  property("string literal with special symbols") {
    parseOne("\"\\t\"") shouldBe CONST_STRING(0, 4, PART.VALID(1, 3, "\t"))
  }

  property("should parse invalid special symbols") {
    parseOne("\"\\ test\"") shouldBe CONST_STRING(0, 8, PART.INVALID(1, 7, "unknown escaped symbol: '\\ '. The valid are \b, \f, \n, \r, \t"))
  }

  property("should parse incomplete special symbols") {
    parseOne("\"foo \\\"") shouldBe CONST_STRING(0, 7, PART.INVALID(1, 6, "invalid escaped symbol: '\\'. The valid are \b, \f, \n, \r, \t"))
  }

  property("block: multiline without ;") {
    val s =
      """let q = 1
        |c""".stripMargin
    parseOne(s) shouldBe BLOCK(
      0,
      11,
      LET(0, 9, PART.VALID(4, 5, "q"), CONST_LONG(8, 9, 1), List.empty),
      REF(10, 11, PART.VALID(10, 11, "c"))
    )
  }

  property("block: multiline with ; at end of let") {
    val s =
      """let q = 1;
        |c""".stripMargin
    parseOne(s) shouldBe BLOCK(
      0,
      12,
      LET(0, 9, PART.VALID(4, 5, "q"), CONST_LONG(8, 9, 1), List.empty),
      REF(11, 12, PART.VALID(11, 12, "c"))
    )
  }

  property("block: multiline with ; at start of body") {
    val s =
      """let q = 1
        |; c""".stripMargin
    parseOne(s) shouldBe BLOCK(
      0,
      13,
      LET(0, 9, PART.VALID(4, 5, "q"), CONST_LONG(8, 9, 1), List.empty),
      REF(12, 13, PART.VALID(12, 13, "c"))
    )
  }

  property("block: oneline") {
    val s = "let q = 1; c"
    parseOne(s) shouldBe BLOCK(
      0,
      12,
      LET(0, 9, PART.VALID(4, 5, "q"), CONST_LONG(8, 9, 1), List.empty),
      REF(11, 12, PART.VALID(11, 12, "c"))
    )
  }

  property("block: invalid") {
    val s = "let q = 1 c"
    parseOne(s) shouldBe BLOCK(
      0,
      11,
      LET(0, 9, PART.VALID(4, 5, "q"), CONST_LONG(8, 9, 1), List.empty),
      INVALID(9, 9, "can't find a separator. Did you mean ';' or '\\n' ?", Some(REF(10, 11, PART.VALID(10, 11, "c"))))
    )
  }

  property("reserved keywords are invalid variable names in block: if") {
    val script =
      s"""let if = 1
         |true""".stripMargin
    parseOne(script) shouldBe BLOCK(
      0,
      15,
      LET(0, 10, PART.INVALID(4, 6, "keywords are restricted: if"), CONST_LONG(9, 10, 1), Seq.empty),
      TRUE(11, 15)
    )
  }

  property("reserved keywords are invalid variable names in block: let") {
    val script =
      s"""let let = 1
         |true""".stripMargin
    parseOne(script) shouldBe BLOCK(
      0,
      16,
      LET(0, 11, PART.INVALID(4, 7, "keywords are restricted: let"), CONST_LONG(10, 11, 1), Seq.empty),
      TRUE(12, 16)
    )
  }

  List("then", "else", "true").foreach { keyword =>
    property(s"reserved keywords are invalid variable names in block: $keyword") {
      val script =
        s"""let ${keyword.padTo(4, " ").mkString} = 1
           |true""".stripMargin
      parseOne(script) shouldBe BLOCK(
        0,
        17,
        LET(0, 12, PART.INVALID(4, 8, s"keywords are restricted: $keyword"), CONST_LONG(11, 12, 1), Seq.empty),
        TRUE(13, 17)
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

  property("reserved keywords are invalid variable names in expr: if") {
    val script = "if + 1"
    parseOne(script) shouldBe BINARY_OP(
      0,
      6,
      REF(0, 2, PART.INVALID(0, 2, "keywords are restricted: if")),
      BinaryOperation.SUM_OP,
      CONST_LONG(5, 6, 1)
    )
  }

  property("reserved keywords are invalid variable names in expr: let") {
    val script = "let + 1"
    parseOne(script) shouldBe BINARY_OP(
      0,
      7,
      REF(0, 3, PART.INVALID(0, 3, "keywords are restricted: let")),
      BinaryOperation.SUM_OP,
      CONST_LONG(6, 7, 1)
    )
  }

  List("then", "else").foreach { keyword =>
    property(s"reserved keywords are invalid variable names in expr: $keyword") {
      val script = s"$keyword + 1"
      parseOne(script) shouldBe BINARY_OP(
        0,
        8,
        REF(0, 4, PART.INVALID(0, 4, s"keywords are restricted: $keyword")),
        BinaryOperation.SUM_OP,
        CONST_LONG(7, 8, 1)
      )
    }
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

  ignore("crypto functions: sha256") {
    val text        = "❤✓☀★☂♞☯☭☢€☎∞❄♫\u20BD=test message"
    val encodedText = ScorexBase58.encode(text.getBytes)

    parseOne(s"sha256(base58'$encodedText')".stripMargin) shouldBe
      FUNCTION_CALL(0, 54, PART.VALID(0, 6, "sha256"), List(CONST_BYTEVECTOR(7, 53, PART.VALID(15, 52, ByteVector(text.getBytes)))))
  }

  ignore("crypto functions: blake2b256") {
    val text        = "❤✓☀★☂♞☯☭☢€☎∞❄♫\u20BD=test message"
    val encodedText = ScorexBase58.encode(text.getBytes)

    parseOne(s"blake2b256(base58'$encodedText')".stripMargin) shouldBe
      FUNCTION_CALL(0, 58, PART.VALID(0, 10, "blake2b256"), List(CONST_BYTEVECTOR(11, 57, PART.VALID(19, 56, ByteVector(text.getBytes)))))
  }

  ignore("crypto functions: keccak256") {
    val text        = "❤✓☀★☂♞☯☭☢€☎∞❄♫\u20BD=test message"
    val encodedText = ScorexBase58.encode(text.getBytes)

    parseOne(s"keccak256(base58'$encodedText')".stripMargin) shouldBe
      FUNCTION_CALL(0, 57, PART.VALID(0, 9, "keccak256"), List(CONST_BYTEVECTOR(10, 56, PART.VALID(18, 55, ByteVector(text.getBytes)))))
  }

  property("show parse all input including INVALID") {
    val script =
      """let C = 1
        |foo
        |@~2
        |true""".stripMargin

    parseAll(script) shouldBe Seq(
      BLOCK(0, 13, LET(0, 9, PART.VALID(4, 5, "C"), CONST_LONG(8, 9, 1), Seq.empty), REF(10, 13, PART.VALID(10, 13, "foo"))),
      INVALID(14, 16, "@~", Some(CONST_LONG(16, 17, 2))),
      TRUE(18, 22)
    )
  }

  property("should parse INVALID expressions in the middle") {
    val script =
      """let C = 1
        |@ /
        |true""".stripMargin
    parseOne(script) shouldBe BLOCK(
      0,
      18,
      LET(0, 9, PART.VALID(4, 5, "C"), CONST_LONG(8, 9, 1), Seq.empty),
      INVALID(10, 13, "@ /", Some(TRUE(14, 18)))
    )
  }

  property("should parse INVALID expressions at start") {
    val script =
      """@ /
        |let C = 1
        |true""".stripMargin
    parseOne(script) shouldBe INVALID(
      0,
      3,
      "@ /",
      Some(
        BLOCK(
          4,
          18,
          LET(4, 13, PART.VALID(8, 9, "C"), CONST_LONG(12, 13, 1), Seq.empty),
          TRUE(14, 18)
        )
      )
    )
  }

  property("should parse INVALID expressions at end") {
    val script =
      """let C = 1
        |true
        |~ /""".stripMargin
    parseAll(script) shouldBe Seq(
      BLOCK(0, 14, LET(0, 9, PART.VALID(4, 5, "C"), CONST_LONG(8, 9, 1), Seq.empty), TRUE(10, 14)),
      INVALID(15, 18, "~ /")
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
      0,
      28,
      REF(6, 8, PART.VALID(6, 8, "tx")),
      List(
        MATCH_CASE(
          11,
          26,
          Some(PART.INVALID(16, 22, "invalid syntax, should be: `case varName: Type => expr` or `case _ => expr`")),
          List.empty,
          CONST_LONG(25, 26, 1)
        )
      )
    )
  }

  property("pattern matching with default case - no type is defined, one separator") {
    parseOne("match tx { case _: | => 1 } ") shouldBe MATCH(
      0,
      27,
      REF(6, 8, PART.VALID(6, 8, "tx")),
      List(
        MATCH_CASE(
          11,
          25,
          None,
          Seq(PART.INVALID(19, 21, "the type for variable should be specified: `case varName: Type => expr`")),
          CONST_LONG(24, 25, 1)
        )
      )
    )
  }

  property("pattern matching with default case - no type is defined, multiple separators") {
    parseOne("match tx { case  _: |||| => 1 } ") shouldBe MATCH(
      0,
      31,
      REF(6, 8, PART.VALID(6, 8, "tx")),
      List(
        MATCH_CASE(
          11,
          29,
          None,
          Seq(PART.INVALID(20, 25, "the type for variable should be specified: `case varName: Type => expr`")),
          CONST_LONG(28, 29, 1)
        )
      )
    )
  }

  property("if expressions") {
    parseOne("if (10 < 15) then true else false") shouldBe IF(
      0,
      33,
      BINARY_OP(4, 11, CONST_LONG(9, 11, 15), LT_OP, CONST_LONG(4, 6, 10)),
      TRUE(18, 22),
      FALSE(28, 33)
    )
    parseOne("if 10 < 15 then true else false") shouldBe IF(
      0,
      31,
      BINARY_OP(3, 10, CONST_LONG(8, 10, 15), LT_OP, CONST_LONG(3, 5, 10)),
      TRUE(16, 20),
      FALSE(26, 31)
    )
    parseOne(s"""if (10 < 15)
                |then true
                |else false""".stripMargin) shouldBe IF(
      0,
      33,
      BINARY_OP(4, 11, CONST_LONG(9, 11, 15), LT_OP, CONST_LONG(4, 6, 10)),
      TRUE(18, 22),
      FALSE(28, 33)
    )

    parseOne(s"""if 10 < 15
                |then true
                |else false""".stripMargin) shouldBe IF(
      0,
      31,
      BINARY_OP(3, 10, CONST_LONG(8, 10, 15), LT_OP, CONST_LONG(3, 5, 10)),
      TRUE(16, 20),
      FALSE(26, 31)
    )
  }

  property("underscore in numbers") {
    parseOne("100_000_000") shouldBe CONST_LONG(0, 11, 100000000)
  }

  property("comments - the whole line at start") {
    val code =
      """# foo
        |true""".stripMargin

    parseOne(code) shouldBe TRUE(6, 10)
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
      0,
      22,
      LET(0, 20, PART.VALID(12, 13, "x"), TRUE(16, 20), List.empty),
      REF(21, 22, PART.VALID(21, 22, "x"))
    )
  }

  property("comments - block - before assignment") {
    val s =
      """let x # foo
        |  = true
        |x""".stripMargin
    parseOne(s) shouldBe BLOCK(
      0,
      22,
      LET(0, 20, PART.VALID(4, 5, "x"), TRUE(16, 20), List.empty),
      REF(21, 22, PART.VALID(21, 22, "x"))
    )
  }

  property("comments - block - between LET and BODY (full line)") {
    val code =
      """let x = true
        |# foo
        |x""".stripMargin

    parseOne(code) shouldBe BLOCK(
      0,
      20,
      LET(0, 18, PART.VALID(4, 5, "x"), TRUE(8, 12), List.empty),
      REF(19, 20, PART.VALID(19, 20, "x"))
    )
  }

  property("comments - block - between LET and BODY (at end of a line)") {
    val code =
      """let x = true # foo
        |x""".stripMargin

    parseOne(code) shouldBe BLOCK(
      0,
      20,
      LET(0, 18, PART.VALID(4, 5, "x"), TRUE(8, 12), List.empty),
      REF(19, 20, PART.VALID(19, 20, "x"))
    )
  }

  property("comments - if - after condition") {
    val code =
      """if 10 < 15 # test
        |then true else false""".stripMargin

    parseOne(code) shouldBe IF(
      0,
      38,
      BINARY_OP(3, 17, CONST_LONG(8, 10, 15), LT_OP, CONST_LONG(3, 5, 10)),
      TRUE(23, 27),
      FALSE(33, 38)
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
      0,
      77,
      REF(6, 7, PART.VALID(6, 7, "p")),
      List(
        MATCH_CASE(
          12,
          57,
          Some(PART.VALID(31, 32, "p")),
          List(PART.VALID(34, 40, "PointA"), PART.VALID(43, 49, "PointB")),
          TRUE(53, 57)
        ),
        MATCH_CASE(
          60,
          75,
          None,
          List.empty,
          FALSE(70, 75)
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
      0,
      85,
      REF(6, 7, PART.VALID(6, 7, "p")),
      List(
        MATCH_CASE(
          12,
          65,
          Some(PART.VALID(17, 18, "p")),
          List(PART.VALID(35, 41, "PointA"), PART.VALID(51, 57, "PointB")),
          TRUE(61, 65)
        ),
        MATCH_CASE(
          68,
          83,
          None,
          List.empty,
          FALSE(78, 83)
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
      0,
      79,
      REF(6, 7, PART.VALID(6, 7, "p")),
      List(
        MATCH_CASE(
          12,
          59,
          Some(PART.VALID(17, 18, "p")),
          List(PART.VALID(36, 42, "PointA"), PART.VALID(45, 51, "PointB")),
          TRUE(55, 59)
        ),
        MATCH_CASE(
          62,
          77,
          None,
          List.empty,
          FALSE(72, 77)
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
      0,
      79,
      REF(6, 7, PART.VALID(6, 7, "p")),
      List(
        MATCH_CASE(
          12,
          59,
          Some(PART.VALID(17, 18, "p")),
          List(PART.VALID(20, 26, "PointA"), PART.VALID(29, 35, "PointB")),
          TRUE(55, 59)
        ),
        MATCH_CASE(
          62,
          77,
          None,
          List.empty,
          FALSE(72, 77)
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
      0,
      94,
      REF(6, 7, PART.VALID(6, 7, "p")),
      List(
        MATCH_CASE(
          12,
          74,
          Some(PART.VALID(17, 18, "p")),
          List(PART.VALID(21, 27, "PointA"), PART.VALID(45, 51, "PointB")),
          TRUE(70, 74)
        ),
        MATCH_CASE(
          77,
          92,
          None,
          List.empty,
          FALSE(87, 92)
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
      0,
      96,
      REF(6, 7, PART.VALID(6, 7, "p")),
      List(
        MATCH_CASE(
          12,
          76,
          Some(PART.VALID(17, 18, "p")),
          List(PART.VALID(20, 26, "PointA"), PART.VALID(45, 51, "PointB")),
          TRUE(72, 76)
        ),
        MATCH_CASE(
          79,
          94,
          None,
          List.empty,
          FALSE(89, 94)
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
      0,
      87,
      REF(6, 7, PART.VALID(6, 7, "p")),
      List(
        MATCH_CASE(
          20,
          59,
          Some(PART.VALID(25, 26, "p")),
          List(PART.VALID(28, 34, "PointA"), PART.VALID(37, 43, "PointB")),
          TRUE(47, 51)
        ),
        MATCH_CASE(
          62,
          85,
          None,
          List.empty,
          FALSE(72, 77)
        )
      )
    )
  }

  property("comments - getter - before dot") {
    val code =
      """x # foo
        |.y""".stripMargin

    parseOne(code) shouldBe GETTER(
      0,
      10,
      REF(0, 1, PART.VALID(0, 1, "x")),
      PART.VALID(9, 10, "y")
    )
  }

  property("comments - getter - after dot") {
    val code =
      """x. # foo
        |y""".stripMargin

    parseOne(code) shouldBe GETTER(
      0,
      10,
      REF(0, 1, PART.VALID(0, 1, "x")),
      PART.VALID(9, 10, "y")
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
      0,
      40,
      PART.VALID(0, 1, "f"),
      List(CONST_LONG(11, 12, 1), CONST_LONG(29, 30, 2))
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
      0,
      22,
      PART.VALID(2, 22, "getElement"),
      List(REF(0, 2, PART.VALID(0, 2, "xs")), CONST_LONG(12, 13, 1))
    )
  }
}
