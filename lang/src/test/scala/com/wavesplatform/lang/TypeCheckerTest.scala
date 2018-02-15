package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang.TypeChecker.{Context, Defs, TypeCheckResult}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector
import scorex.crypto.encode.Base58

class TypeCheckerTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  // ERASURE!

  private def rootTypeTest(propertyName: String)
                          (expr: Expr,
                           expectedResult: TypeCheckResult[Type],
                           varDefs: Defs = Map.empty,
                           predefTypes: Map[String, CUSTOMTYPE] = Map.empty): Unit = property(propertyName) {
    TypeChecker(Context(predefTypes, varDefs), expr).map(_.exprType) match {
      case Right(None) => fail("Type didn't resolved")
      case Right(Some(x)) => Right(x) shouldBe expectedResult
      case e@Left(_) => e shouldBe expectedResult
    }
  }

  private def treeTypeTest(propertyName: String)
                          (expr: Expr,
                           expectedResult: TypeCheckResult[Expr],
                           predefTypes: Map[String, CUSTOMTYPE] = Map.empty,
                           varDefs: Defs = Map.empty): Unit = property(propertyName) {
    TypeChecker(Context(predefTypes, varDefs), expr) shouldBe expectedResult
  }

  private def treeDidNotChangedTest(properties: (String, Expr)*): Unit = properties.foreach {
    case (propertyName, expr) => treeTypeTest(propertyName)(expr = expr, expectedResult = Right(expr))
  }

  private val pointType = CUSTOMTYPE("Point", List("x" -> INT, "y" -> INT))

  rootTypeTest("successful on very deep expressions (stack overflow check)")(
    expr = (1 to 100000).foldLeft[Terms.Expr](CONST_INT(0))((acc, _) => SUM(Block(None, acc), Block(None, CONST_INT(1)))),
    expectedResult = Right(INT)
  )

  treeDidNotChangedTest(
    "CONST_INT" -> CONST_INT(0),
    "CONST_BYTEVECTOR" -> CONST_BYTEVECTOR(ByteVector(1, 2, 3)),
    "TRUE" -> TRUE,
    "FALSE" -> FALSE,
    "NONE" -> NONE
  )

  treeTypeTest("GETTER")(
    predefTypes = Map(pointType.name -> pointType),
    varDefs = Map("p" -> TYPEREF("Point")),
    expr = GETTER(
      ref = Block(None, REF("p")),
      field = "x"
    ),
    expectedResult = Right(GETTER(
      ref = Block(None, REF("p", Some(TYPEREF("Point"))), Some(TYPEREF("Point"))),
      field = "x",
      exprType = Some(INT)
    ))
  )

  treeTypeTest("SUM")(
    expr = SUM(Block(None, CONST_INT(0)), Block(None, CONST_INT(1))),
    expectedResult = Right(SUM(Block(None, CONST_INT(0), Some(INT)), Block(None, CONST_INT(1), Some(INT))))
  )

  treeTypeTest("AND")(
    expr = AND(Block(None, TRUE), Block(None, FALSE)),
    expectedResult = Right(AND(Block(None, TRUE, Some(BOOLEAN)), Block(None, FALSE, Some(BOOLEAN))))
  )

  treeTypeTest("OR")(
    expr = OR(Block(None, TRUE), Block(None, FALSE)),
    expectedResult = Right(OR(Block(None, TRUE, Some(BOOLEAN)), Block(None, FALSE, Some(BOOLEAN))))
  )

  treeTypeTest("EQ")(
    expr = EQ(Block(None, CONST_INT(0)), Block(None, CONST_INT(1))),
    expectedResult = Right(EQ(Block(None, CONST_INT(0), Some(INT)), Block(None, CONST_INT(1), Some(INT))))
  )

  treeTypeTest("GT")(
    expr = GT(Block(None, CONST_INT(0)), Block(None, CONST_INT(1))),
    expectedResult = Right(GT(Block(None, CONST_INT(0), Some(INT)), Block(None, CONST_INT(1), Some(INT))))
  )

  treeTypeTest("GE")(
    expr = GE(Block(None, CONST_INT(0)), Block(None, CONST_INT(1))),
    expectedResult = Right(GE(Block(None, CONST_INT(0), Some(INT)), Block(None, CONST_INT(1), Some(INT))))
  )

  // SIG_VERIFY
  treeTypeTest("SIG_VERIFY")(
    expr = SIG_VERIFY(
      message = Block(None, CONST_BYTEVECTOR(ByteVector(Base58.decode("333").get))),
      signature = Block(None, CONST_BYTEVECTOR(ByteVector(Base58.decode("222").get))),
      publicKey = Block(None, CONST_BYTEVECTOR(ByteVector(Base58.decode("111").get)))
    ),
    expectedResult = Right(SIG_VERIFY(
      message = Block(None, CONST_BYTEVECTOR(ByteVector(Base58.decode("333").get)), Some(BYTEVECTOR)),
      signature = Block(None, CONST_BYTEVECTOR(ByteVector(Base58.decode("222").get)), Some(BYTEVECTOR)),
      publicKey = Block(None, CONST_BYTEVECTOR(ByteVector(Base58.decode("111").get)), Some(BYTEVECTOR))
    ))
  )

  treeTypeTest("IS_DEFINED")(
    expr = IS_DEFINED(Block(None, NONE)),
    expectedResult = Right(IS_DEFINED(Block(None, NONE, Some(OPTION(NOTHING)))))
  )

  treeTypeTest("LET")(
    expr = LET("x", Block(None, CONST_INT(0))),
    expectedResult = Right(LET("x", Block(None, CONST_INT(0), Some(INT))))
  )

  treeTypeTest("BLOCK")(
    expr = Block(
      let = None,
      t = CONST_INT(0)
    ),
    expectedResult = Right(Block(
      let = None,
      t = CONST_INT(0),
      exprType = Some(INT)
    ))
  )

  treeTypeTest("IF")(
    expr = IF(
      cond = Block(None, TRUE),
      ifTrue = Block(None, SOME(Block(None, TRUE))),
      ifFalse = Block(None, NONE),
    ),
    expectedResult = Right(IF(
      cond = Block(None, TRUE, Some(BOOLEAN)),
      ifTrue = Block(None, SOME(Block(None, TRUE, Some(BOOLEAN)), Some(OPTION(BOOLEAN))), Some(OPTION(BOOLEAN))),
      ifFalse = Block(None, NONE, Some(OPTION(NOTHING))),
      exprType = Some(OPTION(BOOLEAN))
    ))
  )

  treeTypeTest("REF")(
    predefTypes = Map(pointType.name -> pointType),
    varDefs = Map("p" -> TYPEREF("Point")),
    expr = REF("p"),
    expectedResult = Right(REF("p", Some(TYPEREF("Point"))))
  )

  treeTypeTest("GET(SOME)")(
    expr = GET(Block(None, SOME(Block(None, TRUE)))),
    expectedResult = Right(GET(
      t = Block(
        let = None,
        t = SOME(Block(None, TRUE, Some(BOOLEAN)), Some(OPTION(BOOLEAN))),
        exprType = Some(OPTION(BOOLEAN))
      ),
      exprType = Some(BOOLEAN)
    ))
  )

  treeTypeTest("GET(NONE)")(
    expr = GET(Block(None, Block(None, NONE))),
    expectedResult = Right(GET(
      t = Block(
        let = None,
        t = Block(None, NONE, Some(OPTION(NOTHING))),
        exprType = Some(OPTION(NOTHING))
      ),
      exprType = Some(NOTHING)
    ))
  )

  treeTypeTest("SOME")(
    expr = SOME(Block(None, TRUE)),
    expectedResult = Right(SOME(Block(None, TRUE, Some(BOOLEAN)), Some(OPTION(BOOLEAN)))),
  )
}
