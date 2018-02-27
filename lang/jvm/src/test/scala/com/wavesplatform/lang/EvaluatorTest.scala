package com.wavesplatform.lang

import cats.data.EitherT
import com.wavesplatform.lang.Context._
import com.wavesplatform.lang.Terms.Typed._
import com.wavesplatform.lang.Terms._
import monix.eval.Coeval
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class EvaluatorTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  private def ev(context: Context = Context.empty, expr: EXPR): Either[_, _] = {
    Evaluator(context, expr)
  }

  private def simpleDeclarationAndUsage(i: Int) = BLOCK(Some(LET("x", CONST_INT(i))), REF("x", INT), INT)

  property("successful on very deep expressions (stack overflow check)") {
    val term = (1 to 100000).foldLeft[EXPR](CONST_INT(0))((acc, _) => BINARY_OP(acc, SUM_OP, CONST_INT(1), INT))
    ev(expr = term) shouldBe Right(100000)
  }

  property("successful on unused let") {
    ev(
      expr = BLOCK(
        Some(LET("x", CONST_INT(3))),
        CONST_INT(3),
        INT
      )) shouldBe Right(3)
  }

  property("successful on some expr") {
    ev(expr = SOME(CONST_INT(4), OPTION(INT))) shouldBe Right(Some(4))
  }

  property("successful on some block") {
    ev(
      expr = BLOCK(
        None,
        SOME(CONST_INT(3), OPTION(INT)),
        OPTION(INT)
      )) shouldBe Right(Some(3))
  }

  property("successful on x = y") {
    ev(
      expr = BLOCK(Some(LET("x", CONST_INT(3))),
                   BLOCK(
                     Some(LET("y", REF("x", INT))),
                     BINARY_OP(REF("x", INT), SUM_OP, REF("y", INT), INT),
                     INT
                   ),
                   INT)) shouldBe Right(6)
  }

  property("successful on simple get") {
    ev(expr = simpleDeclarationAndUsage(3)) shouldBe Right(3)
  }

  property("successful on get used further in expr") {
    ev(
      expr = BLOCK(
        Some(LET("x", CONST_INT(3))),
        BINARY_OP(REF("x", INT), EQ_OP, CONST_INT(2), INT),
        INT
      )) shouldBe Right(false)
  }

  property("successful on multiple lets") {
    ev(
      expr = BLOCK(
        Some(LET("x", CONST_INT(3))),
        BLOCK(Some(LET("y", CONST_INT(3))), BINARY_OP(REF("x", INT), EQ_OP, REF("y", INT), INT), INT),
        INT
      )) shouldBe Right(true)
  }

  property("successful on multiple lets with expression") {
    ev(
      expr = BLOCK(
        Some(LET("x", CONST_INT(3))),
        BLOCK(Some(LET("y", BINARY_OP(CONST_INT(3), SUM_OP, CONST_INT(0), INT))), BINARY_OP(REF("x", INT), EQ_OP, REF("y", INT), INT), INT),
        INT
      )) shouldBe Right(true)
  }

  property("successful on deep type resolution") {
    ev(expr = IF(BINARY_OP(CONST_INT(1), EQ_OP, CONST_INT(2), INT), simpleDeclarationAndUsage(3), CONST_INT(4), INT)) shouldBe Right(4)
  }

  property("successful on same value names in different branches") {
    ev(expr = IF(BINARY_OP(CONST_INT(1), EQ_OP, CONST_INT(2), INT), simpleDeclarationAndUsage(3), simpleDeclarationAndUsage(4), INT)) shouldBe Right(
      4)
  }

  property("fails if override") {
    ev(
      expr = BLOCK(
        Some(LET("x", CONST_INT(3))),
        BLOCK(Some(LET("x", BINARY_OP(CONST_INT(3), SUM_OP, CONST_INT(0), INT))), BINARY_OP(REF("x", INT), EQ_OP, CONST_INT(1), INT), INT),
        INT
      )) should produce("already defined")
  }

  property("fails if definition not found") {
    ev(expr = BINARY_OP(REF("x", INT), SUM_OP, CONST_INT(2), INT)) should produce("A definition of 'x' is not found")
  }

  property("successful GET/IS_DEFINED") {
    ev(expr = IS_DEFINED(NONE)) shouldBe Right(false)
    ev(expr = IS_DEFINED(SOME(CONST_INT(1), OPTION(INT)))) shouldBe Right(true)
    ev(expr = GET(SOME(CONST_INT(1), OPTION(INT)), INT)) shouldBe Right(1)
  }

  property("custom type field access") {
    val pointType     = CustomType("Point", List("X" -> INT, "Y"                     -> INT))
    val pointInstance = Obj(Map("X"                  -> LazyVal(INT)(EitherT.pure(3)), "Y" -> LazyVal(INT)(EitherT.pure(4))))
    ev(
      context = Context(
        typeDefs = Map(pointType.name -> pointType),
        letDefs = Map(("p", (TYPEREF(pointType.name), Coeval.evalOnce(pointInstance)))),
        functions = Map.empty
      ),
      expr = BINARY_OP(GETTER(REF("p", TYPEREF("Point")), "X", INT), SUM_OP, CONST_INT(2), INT)
    ) shouldBe Right(5)
  }

  property("lazy let evaluation doesn't throw if not used") {
    val pointType     = CustomType("Point", List(("X", INT), ("Y", INT)))
    val pointInstance = Obj(Map(("X", LazyVal(INT)(EitherT.pure(3))), ("Y", LazyVal(INT)(EitherT.pure(4)))))
    ev(
      context = Context(
        typeDefs = Map((pointType.name, pointType)),
        letDefs = Map(("p", (TYPEREF(pointType.name), Coeval.evalOnce(pointInstance))), ("badVal", (INT, Coeval(???)))),
        functions = Map.empty
      ),
      expr = BLOCK(Some(LET("Z", REF("badVal", INT))), BINARY_OP(GETTER(REF("p", TYPEREF("Point")), "X", INT), SUM_OP, CONST_INT(2), INT), INT)
    ) shouldBe Right(5)
  }

  property("successful on simple function evaluation") {
    ev(
      context = Context(
        typeDefs = Map.empty,
        letDefs = Map.empty,
        functions = Map(multiplierFunction.name -> multiplierFunction)
      ),
      expr = FUNCTION_CALL(multiplierFunction.name, List(Typed.CONST_INT(3), Typed.CONST_INT(4)), INT)
    ) shouldBe Right(12)
  }
}
