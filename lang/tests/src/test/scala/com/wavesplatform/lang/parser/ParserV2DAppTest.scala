package com.wavesplatform.lang.parser

import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import com.wavesplatform.lang.v1.parser.Expressions._
import com.wavesplatform.lang.v1.parser.{Expressions, Parser, ParserV2}
import com.wavesplatform.lang.v1.testing.ScriptGenParser
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ParserV2DAppTest extends PropSpec with PropertyChecks with Matchers with ScriptGenParser with NoShrink {

  private def parse(x: String): DAPP = ParserV2.parseDAPP(x) match {
    case Right((parsedScript, _)) => parsedScript
    case _                        => throw new TestFailedException("Test failed", 0)
  }

  private def cleanOffsets(l: LET): LET =
    l.copy(Pos(0, 0), name = cleanOffsets(l.name), value = cleanOffsets(l.value))  // , types = l.types.map(cleanOffsets(_))

  private def cleanOffsets[T](p: PART[T]): PART[T] = p match {
    case PART.VALID(_, x)   => PART.VALID(AnyPos, x)
    case PART.INVALID(_, x) => PART.INVALID(AnyPos, x)
  }

  private def cleanOffsets(expr: EXPR): EXPR = expr match {
    case x: CONST_LONG                             => x.copy(position = Pos(0, 0))
    case x: REF                                    => x.copy(position = Pos(0, 0), key = cleanOffsets(x.key))
    case x: CONST_STRING                           => x.copy(position = Pos(0, 0), value = cleanOffsets(x.value))
    case x: CONST_BYTESTR                          => x.copy(position = Pos(0, 0), value = cleanOffsets(x.value))
    case x: TRUE                                   => x.copy(position = Pos(0, 0))
    case x: FALSE                                  => x.copy(position = Pos(0, 0))
    case x: BINARY_OP                              => x.copy(position = Pos(0, 0), a = cleanOffsets(x.a), b = cleanOffsets(x.b))
    case x: IF                                     => x.copy(position = Pos(0, 0), cond = cleanOffsets(x.cond), ifTrue = cleanOffsets(x.ifTrue), ifFalse = cleanOffsets(x.ifFalse))
    case x @ BLOCK(_, l: Expressions.LET, _, _, _) => x.copy(position = Pos(0, 0), let = cleanOffsets(l), body = cleanOffsets(x.body))
    case x: FUNCTION_CALL                          => x.copy(position = Pos(0, 0), name = cleanOffsets(x.name), args = x.args.map(cleanOffsets(_)))
    case _                                         => throw new NotImplementedError(s"toString for ${expr.getClass.getSimpleName}")
  }

  property("simple 1-annotated function") {
    val code =
      """
        |
        | @Ann(foo)
        | func bar(arg:Baz) = {
        |    3
        | }
        |
        |
        |""".stripMargin
    parse(code) shouldBe DAPP(
      AnyPos,
      List.empty,
      List(
        ANNOTATEDFUNC(
          AnyPos,
          List(Expressions.ANNOTATION(AnyPos, PART.VALID(AnyPos, "Ann"), List(PART.VALID(AnyPos, "foo")))),
          Expressions.FUNC(
            AnyPos,
            CONST_LONG(AnyPos, 3),
            PART.VALID(AnyPos, "bar"),
            List((PART.VALID(AnyPos, "arg"), Single(PART.VALID(AnyPos, "Baz"), None)))
          )
        )
      )
    )
  }

  property("simple 2-annotated function") {
    val code =
      """
        | func foo() = {
        |   true
        | }
        |
        | @Ann(foo)
        | @Ioann(zoo, shazoo)
        | func bar(arg:Baz) = {
        |    3
        | }
        |
        |
        |""".stripMargin
    parse(code) shouldBe DAPP(
      AnyPos,
      List(
        FUNC(
          AnyPos,
          TRUE(AnyPos),
          PART.VALID(AnyPos, "foo"),
          List.empty
        )
      ),
      List(
        ANNOTATEDFUNC(
          AnyPos,
          List(
            Expressions.ANNOTATION(AnyPos, PART.VALID(AnyPos, "Ann"), List(PART.VALID(AnyPos, "foo"))),
            Expressions.ANNOTATION(AnyPos, PART.VALID(AnyPos, "Ioann"), List(PART.VALID(AnyPos, "zoo"), PART.VALID(AnyPos, "shazoo")))
          ),
          Expressions.FUNC(
            AnyPos,
            CONST_LONG(AnyPos, 3),
            PART.VALID(AnyPos, "bar"),
            List((PART.VALID(AnyPos, "arg"), Single(PART.VALID(AnyPos, "Baz"), None)))
          )
        )
      )
    )
  }

  property("contract script with comment in the beginning") {
    val code =
      """
        | # comment1
        | # comment2
        | func foo() = {
        |    true # comment3
        | }
        | # comment4
        | # comment5
        | @Ann(foo)
        | func bar(arg:Baz) = {
        |    3  # comment6
        | }
        |
        |""".stripMargin
    parse(code) shouldBe DAPP(
      AnyPos,
      List(
        FUNC(
          AnyPos,
          TRUE(AnyPos),
          PART.VALID(AnyPos, "foo"),
          List.empty
        )
      ),
      List(
        ANNOTATEDFUNC(
          AnyPos,
          List(Expressions.ANNOTATION(AnyPos, PART.VALID(AnyPos, "Ann"), List(PART.VALID(AnyPos, "foo")))),
          Expressions.FUNC(
            AnyPos,
            CONST_LONG(AnyPos, 3),
            PART.VALID(AnyPos, "bar"),
            List((PART.VALID(AnyPos, "arg"), Single(PART.VALID(AnyPos, "Baz"), None)))
          )
        )
      )
    )
  }

  property("functions without body brackets with comments") {
    val code =
      """
        | # comment
        | func foo() = 42 + 42 - 1
        |
        | @Ann(x)
        | func bar(arg:ArgType) = foo() # comment
        | # comment
        | @Ann(y)
        | func baz(arg:ArgType) = if (10 < 15) then true else false
        |
        |""".stripMargin
    parse(code)
  }

  property("parse directives as comments (ignore)") {
    val code =
      """
        | # comment
        | {-# STDLIB_VERSION 3 #-}
        | {-# TEST_TEST 123 #-}
        | # comment
        |
        | @Ann(foo)
        | func bar(arg:Baz) = {
        |    3
        | }
        |
        |
        |""".stripMargin
    parse(code) shouldBe DAPP(
      AnyPos,
      List.empty,
      List(
        ANNOTATEDFUNC(
          AnyPos,
          List(Expressions.ANNOTATION(AnyPos, PART.VALID(AnyPos, "Ann"), List(PART.VALID(AnyPos, "foo")))),
          Expressions.FUNC(
            AnyPos,
            CONST_LONG(AnyPos, 3),
            PART.VALID(AnyPos, "bar"),
            List((PART.VALID(AnyPos, "arg"), Single(PART.VALID(AnyPos, "Baz"), None)))
          )
        )
      )
    )
  }

  property("functions with comment after first body bracket") {
    val code =
      """
        |
        | #@Callable(i)
        | func foo() = 42 + 42 - 1
        |
        | @Ann(x)
        | func bar(arg:ArgType) = { # more comments
        |   foo() # comment
        | }
        |
        |""".stripMargin
    parse(code)
  }

  property("contract with comments in different places") {
    val code =
      """
        | # comment
        | {-# STDLIB_VERSION 3 #-} # comment
        | {-# TEST_TEST 123 #-} # comment
        | # comment
        | # comment # comment
        | func foo1 # comment
        | ( # comment
        | ) # comment
        | = 42 + 42 - 1 # comment
        | # comment
        | @Callable(i) # comment
        | func foo # comment
        | ( # comment
        | ) # comment
        | = 42 + 42 - 1 # comment
        | # comment
        | @Ann # comment
        | (  # comment
        | x  # comment
        | )  # comment
        | func bar # comment
        | ( # comment
        | arg:ArgType, # comment
        | arg2:ArgType # comment
        | ) # comment
        | = { # more comments
        |   foo() # comment
        | } # comment
        | # comment
        |""".stripMargin
    parse(code)
  }

  property("disallow function declarations after annotated funcions.") {
    val code =
      """
        | # comment
        | {-# STDLIB_VERSION 3 #-}
        | {-# TEST_TEST 123 #-}
        | # comment
        |
        | @Ann(foo)
        | func bar(arg:Baz) = {
        |    3
        | }
        |
        | func baz(arg:Int) = {
        |    4
        | }
        |""".stripMargin
    Parser.parseContract(code).toString.contains("Local functions should be defined before @Callable one") shouldBe true
  }

  property("disallow value declarations after annotated funcions.") {
    val code =
      """
        | # comment
        | {-# STDLIB_VERSION 3 #-}
        | {-# TEST_TEST 123 #-}
        | # comment
        |
        | @Ann(foo)
        | func bar(arg:Baz) = {
        |    3
        | }
        |
        | let baz = 4
        |
        |""".stripMargin
    Parser.parseContract(code).toString.contains("Local functions should be defined before @Callable one") shouldBe true
  }

  property("Unary expr") {
    val code =
      """{-# STDLIB_VERSION 4 #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |{-# CONTENT_TYPE DAPP #-}
        |
        |let a10 = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
        |
        |func deleteEntry(acc: List[DeleteEntry], e: String) = DeleteEntry(e) :: acc
        |
        |func t() = delateEntry("q") :: FOLD<10>(a10, [], deleteEntry)
        |
        |@Callable(i) func f() = []
        |""".stripMargin
    ParserV2.parseDAPP(code) shouldBe Symbol("right")
  }

  property("FOLD expr") {
    val code =
      """{-# STDLIB_VERSION 4 #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |{-# CONTENT_TYPE DAPP #-}
        |
        |let a10 = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
        |
        |func deleteEntry(acc: List[DeleteEntry], e: String) = DeleteEntry(e) :: acc
        |
        |@Callable(i) func delete100Entries() = FOLD<10>(a10, [], deleteEntry)
        |
        |@Callable(i) func delete(k: String) = [DeleteEntry(k)]
        |""".stripMargin
    ParserV2.parseDAPP(code) shouldBe Symbol("right")
  }

}
