package com.wavesplatform.lang

import com.wavesplatform.lang.TypeChecker.{Context, Defs, TypeCheckResult}
import com.wavesplatform.lang.Terms._
import monix.eval.Coeval
import org.scalacheck.Prop
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Prop.BooleanOperators
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector

class TypeCheckerTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  private def propertyTest(propertyName: String)
                          (expr: Expr,
                           expectedResult: TypeCheckResult[Type],
                           predefTypes: Map[String, CUSTOMTYPE] = Map.empty,
                           defs: Defs = Map.empty): Unit = property(propertyName) {
    val r: Prop = TypeChecker(Context(predefTypes, defs), expr).map(_.exprType) match {
      case Right(None) => false :| "Type didn't resolved"
      case Right(Some(x)) => (Right(x) == expectedResult) :| "Ok"
      case e@Left(_) => (e == expectedResult) :| "Ok err"
    }

    r
  }

  private def simpleDeclarationAndUsage(i: Int) = Block(Some(LET("x", CONST_INT(i))), REF("x"))

  //  property("successful on very deep expressions (stack overflow check)") {
  //    val term = (1 to 100000).foldLeft[Terms.Expr](CONST_INT(0))((acc, _) => SUM(acc, CONST_INT(1)))
  //    test(expr = term) shouldBe Right(100000)
  //  }

  propertyTest("CONST_INT")(
    expr = CONST_INT(0),
    expectedResult = Right(INT)
  )

//  propertyTest("GETTER")(
//
//  )

  propertyTest("LET")(
    expr = LET("x", CONST_INT(0)),
    expectedResult = Right(INT)
  )

  propertyTest("BLOCK")(
    expr = Block(
      let = None,
      t = CONST_INT(0)
    ),
    expectedResult = Right(INT)
  )

  //  property("successful on x = y") {
  //    test(
  //      expr = Block(Some(LET("x", CONST_INT(3))),
  //                   Block(
  //                     Some(LET("y", REF("x"))),
  //                     SUM(REF("x"), REF("y"))
  //                   ))) shouldBe Right(6)
  //  }
  //
  //  property("successful on simple get") {
  //    test(expr = simpleDeclarationAndUsage(3)) shouldBe Right(3)
  //  }
  //
  //  property("successful on get used further in expr") {
  //    test(
  //      expr = Block(
  //        Some(LET("x", CONST_INT(3))),
  //        EQ(REF("x"), CONST_INT(2))
  //      )) shouldBe Right(false)
  //  }
  //
  //  property("successful on multiple lets") {
  //    test(
  //      expr = Block(
  //        Some(LET("x", CONST_INT(3))),
  //        Block(Some(LET("y", CONST_INT(3))), EQ(REF("x"), REF("y")))
  //      )) shouldBe Right(true)
  //  }
  //
  //  property("successful on multiple lets with expression") {
  //    test(
  //      expr = Block(
  //        Some(LET("x", CONST_INT(3))),
  //        Block(Some(LET("y", SUM(CONST_INT(3), CONST_INT(0)))), EQ(REF("x"), REF("y")))
  //      )) shouldBe Right(true)
  //  }
  //
  //  property("successful on deep type resolution") {
  //    test(expr = IF(EQ(CONST_INT(1), CONST_INT(2)), simpleDeclarationAndUsage(3), CONST_INT(4))) shouldBe Right(4)
  //  }
  //
  //  property("successful on same value names in different branches") {
  //    test(expr = IF(EQ(CONST_INT(1), CONST_INT(2)), simpleDeclarationAndUsage(3), simpleDeclarationAndUsage(4))) shouldBe Right(4)
  //  }
  //
  //  property("fails if override") {
  //    test(
  //      expr = Block(
  //        Some(LET("x", CONST_INT(3))),
  //        Block(Some(LET("x", SUM(CONST_INT(3), CONST_INT(0)))), EQ(REF("x"), CONST_INT(1)))
  //      )) should produce("already defined")
  //  }
  //
  //  property("fails if types do not match") {
  //    test(
  //      expr = Block(
  //        Some(LET("x", CONST_INT(3))),
  //        Block(Some(LET("y", EQ(CONST_INT(3), CONST_INT(0)))), EQ(REF("x"), REF("y")))
  //      )) should produce("Typecheck failed")
  //  }
  //
  //  property("fails if definition not found") {
  //    test(expr = SUM(REF("x"), CONST_INT(2))) should produce("Cannot resolve type of x")
  //  }
  //
  //  property("fails if 'IF' branches lead to different types") {
  //    test(expr = IF(EQ(CONST_INT(1), CONST_INT(2)), CONST_INT(0), CONST_BYTEVECTOR(ByteVector.empty))) should produce("Typecheck failed")
  //  }
  //
  //  property("Typechecking") {
  //    test(expr = EQ(CONST_INT(2), CONST_INT(2))) shouldBe Right(true)
  //  }
  //
  //  property("successful Typechecking Some") {
  //    test(expr = EQ(SOME(CONST_INT(2)), SOME(CONST_INT(2)))) shouldBe Right(true)
  //  }
  //
  //  property("successful Typechecking Option") {
  //    test(expr = EQ(SOME(CONST_INT(2)), NONE)) shouldBe Right(false)
  //    test(expr = EQ(NONE, SOME(CONST_INT(2)))) shouldBe Right(false)
  //  }
  //
  //  property("successful nested Typechecking Option") {
  //    test(expr = EQ(SOME(SOME(SOME(CONST_INT(2)))), NONE)) shouldBe Right(false)
  //    test(expr = EQ(SOME(NONE), SOME(SOME(CONST_INT(2))))) shouldBe Right(false)
  //  }
  //
  //  property("fails if nested Typechecking Option finds mismatch") {
  //    test(expr = EQ(SOME(SOME(FALSE)), SOME(SOME(CONST_INT(2))))) should produce("Typecheck failed")
  //  }
  //
  //  property("successful GET/IS_DEFINED") {
  //    test(expr = IS_DEFINED(NONE)) shouldBe Right(false)
  //    test(expr = IS_DEFINED(SOME(CONST_INT(1)))) shouldBe Right(true)
  //    test(expr = GET(SOME(CONST_INT(1)))) shouldBe Right(1)
  //  }
  //
  //  property("resolveType") {
  //    Evaluator.resolveType(Context(Map.empty,Map.empty), SOME(CONST_INT(3))).result shouldBe Right(OPTION(INT))
  //    Evaluator.resolveType(Context(Map.empty,Map.empty), NONE).result shouldBe Right(OPTION(NOTHING))
  //    Evaluator.resolveType(Context(Map.empty,Map.empty), IF(TRUE, SOME(CONST_INT(3)), NONE)).result shouldBe Right(OPTION(INT))
  //    Evaluator.resolveType(Context(Map.empty,Map.empty), IF(TRUE, NONE, SOME(CONST_INT(3)))).result shouldBe Right(OPTION(INT))
  //    Evaluator.resolveType(Context(Map.empty,Map.empty), IF(TRUE, NONE, NONE)).result shouldBe Right(OPTION(NOTHING))
  //    Evaluator.resolveType(Context(Map.empty,Map.empty), IF(TRUE, SOME(FALSE), SOME(CONST_INT(3)))).result should produce("Typecheck")
  //  }
  //
  //  property("successful resolve strongest type") {
  //    test(expr = GET(IF(TRUE, SOME(CONST_INT(3)), SOME(CONST_INT(2))))) shouldBe Right(3)
  //    test(expr = GET(IF(TRUE, SOME(CONST_INT(3)), NONE))) shouldBe Right(3)
  //    test(expr = SUM(CONST_INT(1), GET(IF(TRUE, SOME(CONST_INT(3)), NONE)))) shouldBe Right(4)
  //  }
  //
  //  property("custom type field access") {
  //    val pointType     = CUSTOMTYPE("Point", List("X" -> INT, "Y" -> INT))
  //    val pointInstance = OBJECT(Map("X" -> LazyVal(INT)(Coeval(3)), "Y" -> LazyVal(INT)(Coeval(4))))
  //    test(
  //      predefTypes = Map(pointType.name -> pointType),
  //      defs = Map("p" -> (TYPEREF("Point"), pointInstance)),
  //      expr = SUM(GETTER(REF("p"), "X"), CONST_INT(2))
  //    ) shouldBe Right(5)
  //
  //  }
}
