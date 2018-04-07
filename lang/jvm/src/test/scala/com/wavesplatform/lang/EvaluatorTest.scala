package com.wavesplatform.lang

import cats.data.EitherT
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.Terms.Typed._
import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang.TypeInfo._
import com.wavesplatform.lang.ctx._
import com.wavesplatform.lang.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class EvaluatorTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  private def ev[T: TypeInfo](context: Context = Context.empty, expr: EXPR): Either[_, _] = Evaluator[T](context, expr)
  private def simpleDeclarationAndUsage(i: Int)                                           = BLOCK(Some(LET("x", CONST_LONG(i))), REF("x", LONG), LONG)

  property("successful on very deep expressions (stack overflow check)") {
    val term = (1 to 100000).foldLeft[EXPR](CONST_LONG(0))((acc, _) => BINARY_OP(acc, SUM_OP, CONST_LONG(1), LONG))
    ev[Long](expr = term) shouldBe Right(100000)
  }

  property("successful on unused let") {
    ev[Long](
      expr = BLOCK(
        Some(LET("x", CONST_LONG(3))),
        CONST_LONG(3),
        LONG
      )) shouldBe Right(3)
  }

  property("successful on x = y") {
    ev[Long](
      expr = BLOCK(Some(LET("x", CONST_LONG(3))),
                   BLOCK(
                     Some(LET("y", REF("x", LONG))),
                     BINARY_OP(REF("x", LONG), SUM_OP, REF("y", LONG), LONG),
                     LONG
                   ),
                   LONG)) shouldBe Right(6)
  }

  property("successful on simple get") {
    ev[Long](expr = simpleDeclarationAndUsage(3)) shouldBe Right(3)
  }

  property("successful on get used further in expr") {
    ev[Boolean](
      expr = BLOCK(
        Some(LET("x", CONST_LONG(3))),
        BINARY_OP(REF("x", LONG), EQ_OP, CONST_LONG(2), BOOLEAN),
        BOOLEAN
      )) shouldBe Right(false)
  }

  property("successful on multiple lets") {
    ev[Boolean](
      expr = BLOCK(
        Some(LET("x", CONST_LONG(3))),
        BLOCK(Some(LET("y", CONST_LONG(3))), BINARY_OP(REF("x", LONG), EQ_OP, REF("y", LONG), BOOLEAN), BOOLEAN),
        BOOLEAN
      )) shouldBe Right(true)
  }

  property("successful on multiple lets with expression") {
    ev[Boolean](
      expr = BLOCK(
        Some(LET("x", CONST_LONG(3))),
        BLOCK(Some(LET("y", BINARY_OP(CONST_LONG(3), SUM_OP, CONST_LONG(0), LONG))),
              BINARY_OP(REF("x", LONG), EQ_OP, REF("y", LONG), BOOLEAN),
              BOOLEAN),
        BOOLEAN
      )) shouldBe Right(true)
  }

  property("successful on deep type resolution") {
    ev[Long](expr = IF(BINARY_OP(CONST_LONG(1), EQ_OP, CONST_LONG(2), BOOLEAN), simpleDeclarationAndUsage(3), CONST_LONG(4), LONG)) shouldBe Right(4)
  }

  property("successful on same value names in different branches") {
    val expr = IF(BINARY_OP(CONST_LONG(1), EQ_OP, CONST_LONG(2), BOOLEAN), simpleDeclarationAndUsage(3), simpleDeclarationAndUsage(4), LONG)
    ev[Long](expr = expr) shouldBe Right(4)
  }

  property("fails if override") {
    ev[Long](
      expr = BLOCK(
        Some(LET("x", CONST_LONG(3))),
        BLOCK(Some(LET("x", BINARY_OP(CONST_LONG(3), SUM_OP, CONST_LONG(0), LONG))), BINARY_OP(REF("x", LONG), EQ_OP, CONST_LONG(1), LONG), LONG),
        LONG
      )) should produce("already defined")
  }

  property("fails if definition not found") {
    ev[Long](expr = BINARY_OP(REF("x", LONG), SUM_OP, CONST_LONG(2), LONG)) should produce("A definition of 'x' is not found")
  }

  property("custom type field access") {
    val pointType     = PredefType("Point", List("X" -> LONG, "Y"                           -> LONG))
    val pointInstance = Obj(Map("X"                  -> LazyVal(LONG)(EitherT.pure(3)), "Y" -> LazyVal(LONG)(EitherT.pure(4))))
    ev[Long](
      context = Context(
        typeDefs = Map(pointType.name -> pointType),
        letDefs = Map(("p", LazyVal(TYPEREF(pointType.name))(EitherT.pure(pointInstance)))),
        functions = Map.empty
      ),
      expr = BINARY_OP(GETTER(REF("p", TYPEREF("Point")), "X", LONG), SUM_OP, CONST_LONG(2), LONG)
    ) shouldBe Right(5)
  }

  property("lazy let evaluation doesn't throw if not used") {
    val pointType     = PredefType("Point", List(("X", LONG), ("Y", LONG)))
    val pointInstance = Obj(Map(("X", LazyVal(LONG)(EitherT.pure(3))), ("Y", LazyVal(LONG)(EitherT.pure(4)))))
    val context = Context(
      typeDefs = Map((pointType.name, pointType)),
      letDefs = Map(("p", LazyVal(TYPEREF(pointType.name))(EitherT.pure(pointInstance))), ("badVal", LazyVal(LONG)(EitherT.leftT("Error")))),
      functions = Map.empty
    )
    ev[Long](
      context = context,
      expr = BLOCK(Some(LET("Z", REF("badVal", LONG))), BINARY_OP(GETTER(REF("p", TYPEREF("Point")), "X", LONG), SUM_OP, CONST_LONG(2), LONG), LONG)
    ) shouldBe Right(5)
  }

  property("field and value are evaluated maximum once") {
    var fieldCalculated = 0
    var valueCalculated = 0

    val pointType = PredefType("Point", List(("X", LONG), ("Y", LONG)))
    val pointInstance = Obj(Map(("X", LazyVal(LONG)(EitherT.pure {
      fieldCalculated = fieldCalculated + 1
      3
    })), ("Y", LazyVal(LONG)(EitherT.pure(4)))))
    val context = Context(
      typeDefs = Map((pointType.name, pointType)),
      letDefs = Map(("p", LazyVal(TYPEREF(pointType.name))(EitherT.pure(pointInstance))), ("h", LazyVal(LONG)(EitherT.pure {
        valueCalculated = valueCalculated + 1
        4
      }))),
      functions = Map.empty
    )
    ev[Long](
      context = context,
      expr = BINARY_OP(GETTER(REF("p", TYPEREF("Point")), "X", LONG), SUM_OP, GETTER(REF("p", TYPEREF("Point")), "X", LONG), LONG)
    ) shouldBe Right(6)

    ev[Long](
      context = context,
      expr = BINARY_OP(REF("h", LONG), SUM_OP, REF("h", LONG), LONG)
    ) shouldBe Right(8)

    fieldCalculated shouldBe 1
    valueCalculated shouldBe 1
  }

  property("let is evaluated maximum once") {
    var functionEvaluated = 0

    val f = PredefFunction("F", LONG, List(("_", LONG))) { _ =>
      functionEvaluated = functionEvaluated + 1
      Right(1L)
    }

    val context = Context(
      typeDefs = Map.empty,
      letDefs = Map.empty,
      functions = Map(f.header -> f)
    )
    ev[Long](
      context = context,
      expr = BLOCK(Some(LET("X", FUNCTION_CALL("F", List(CONST_LONG(1000)), LONG))), BINARY_OP(REF("X", LONG), SUM_OP, REF("X", LONG), LONG), LONG)
    ) shouldBe Right(2L)

    functionEvaluated shouldBe 1

  }

  property("successful on simple function evaluation") {
    ev[Long](
      context = Context(
        typeDefs = Map.empty,
        letDefs = Map.empty,
        functions = Map(multiplierFunction.header -> multiplierFunction)
      ),
      expr = FUNCTION_CALL(multiplierFunction.name, List(Typed.CONST_LONG(3), Typed.CONST_LONG(4)), LONG)
    ) shouldBe Right(12)
  }
}
