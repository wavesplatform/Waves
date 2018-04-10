package com.wavesplatform.lang

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang.TypeChecker.{TypeCheckResult, TypeCheckerContext}
import com.wavesplatform.lang.ctx._
import com.wavesplatform.lang.ctx.impl.PureContext._
import com.wavesplatform.lang.testing.ScriptGen
import monix.eval.Coeval
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector

class TypeCheckerTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  private val pointType = PredefType("Point", List("x" -> LONG, "y" -> LONG))

  private val idT = PredefFunction("idT", TYPEPARAM('T'), List("p1" -> TYPEPARAM('T')))(Right(_))
  private val extract = PredefFunction("extract", TYPEPARAM('T'), List("p1" -> OPTIONTYPEPARAM(TYPEPARAM('T')))) {
    case Some(vl) :: Nil => Right(vl)
    case _               => Left("extracting from empty option")
  }
  private val undefinedOptionLong = PredefFunction("undefinedOptionLong", OPTION(LONG), List.empty)(_ => ???)
  private val idOptionLong        = PredefFunction("idOptionLong", UNIT, List("opt" -> OPTION(OPTION(LONG))))(_ => Right(()))
  private val unitOnNone          = PredefFunction("unitOnNone", UNIT, List("opt" -> OPTION(NOTHING)))(_ => Right(()))
  private val functionWithTwoPrarmsOfTheSameType =
    PredefFunction("functionWithTwoPrarmsOfTheSameType", TYPEPARAM('T'), List("p1" -> TYPEPARAM('T'), "p2" -> TYPEPARAM('T')))(Right(_))

  private val ctx = Context.build(
    Seq(pointType),
    Map(("None", none)),
    functions = Seq(multiplierFunction, functionWithTwoPrarmsOfTheSameType, idT, unitOnNone, extract, undefinedOptionLong, idOptionLong, some)
  )

  private val typeCheckerContext = TypeCheckerContext.fromContext(ctx)
  property("should infer generic function return type") {
    import Untyped._
    val Right(v) = TypeChecker(typeCheckerContext, FUNCTION_CALL(idT.name, List(CONST_LONG(1))))

    v.tpe shouldBe LONG
  }

  property("should infer inner types") {
    import Untyped._
    val Right(v) = TypeChecker(typeCheckerContext, FUNCTION_CALL(extract.name, List(FUNCTION_CALL(undefinedOptionLong.name, List.empty))))

    v.tpe shouldBe LONG
  }

  treeTypeTest(s"unitOnNone(NONE)")(
    ctx = typeCheckerContext,
    expr = Untyped.FUNCTION_CALL(unitOnNone.name, List(Untyped.REF("None"))),
    expectedResult = Right(Typed.FUNCTION_CALL(unitOnNone.header, List(Typed.REF("None", OPTION(NOTHING))), UNIT))
  )

  rootTypeTest("successful on very deep expressions (stack overflow check)")(
    expr = (1 to 100000).foldLeft[Untyped.EXPR](Untyped.CONST_LONG(0))((acc, _) => Untyped.BINARY_OP(acc, SUM_OP, Untyped.CONST_LONG(1))),
    expectedResult = Right(LONG)
  )

  {
    import Typed._

    treeTypeErasureTests(
      "CONST_LONG"       -> CONST_LONG(0),
      "CONST_BYTEVECTOR" -> CONST_BYTEVECTOR(ByteVector(1, 2, 3)),
      "CONST_STRING"     -> CONST_STRING("abc"),
      "TRUE"             -> TRUE,
      "FALSE"            -> FALSE,
      "SUM"              -> BINARY_OP(CONST_LONG(0), SUM_OP, CONST_LONG(1), LONG),
      "AND"              -> BINARY_OP(TRUE, AND_OP, FALSE, BOOLEAN),
      "OR"               -> BINARY_OP(TRUE, OR_OP, FALSE, BOOLEAN),
      "EQ(LONG)"         -> BINARY_OP(CONST_LONG(0), EQ_OP, CONST_LONG(1), BOOLEAN),
      "EQ(BOOL)"         -> BINARY_OP(TRUE, EQ_OP, FALSE, BOOLEAN),
      "GT"               -> BINARY_OP(CONST_LONG(0), GT_OP, CONST_LONG(1), BOOLEAN),
      "GE"               -> BINARY_OP(CONST_LONG(0), GE_OP, CONST_LONG(1), BOOLEAN),
      "BLOCK" -> BLOCK(
        let = None,
        body = CONST_LONG(0),
        tpe = LONG
      )
    )
  }

  treeTypeTest("GETTER")(
    ctx = TypeCheckerContext(predefTypes = Map(pointType.name -> pointType), varDefs = Map("p" -> TYPEREF("Point")), functionDefs = Map.empty),
    expr = Untyped.GETTER(
      ref = Untyped.REF("p"),
      field = "x"
    ),
    expectedResult = Right(
      Typed.GETTER(
        ref = Typed.REF("p", TYPEREF("Point")),
        field = "x",
        tpe = LONG
      ))
  )

  treeTypeTest("REF(OBJECT)")(
    ctx = TypeCheckerContext(predefTypes = Map(pointType.name -> pointType), varDefs = Map("p" -> TYPEREF("Point")), functionDefs = Map.empty),
    expr = Untyped.REF("p"),
    expectedResult = Right(Typed.REF("p", TYPEREF("Point")))
  )

  treeTypeTest("REF x = y")(
    ctx = TypeCheckerContext(predefTypes = Map(pointType.name -> pointType), varDefs = Map("p" -> TYPEREF("Point")), functionDefs = Map.empty),
    expr = Untyped.REF("p"),
    expectedResult = Right(Typed.REF("p", TYPEREF("Point")))
  )

  treeTypeTest("MULTIPLY(1,2)")(
    ctx = typeCheckerContext,
    expr = Untyped.FUNCTION_CALL(multiplierFunction.name, List(Untyped.CONST_LONG(1), Untyped.CONST_LONG(2))),
    expectedResult = Right(Typed.FUNCTION_CALL(multiplierFunction.header, List(Typed.CONST_LONG(1), Typed.CONST_LONG(2)), LONG))
  )

  treeTypeTest(s"idOptionLong(NONE)")(
    ctx = typeCheckerContext,
    expr = Untyped.FUNCTION_CALL(idOptionLong.name, List(Untyped.REF("None"))),
    expectedResult = Right(Typed.FUNCTION_CALL(idOptionLong.header, List(Typed.REF("None", OPTION(NOTHING))), UNIT))
  )

  treeTypeTest(s"idOptionLong(SOME(NONE))")(
    ctx = typeCheckerContext,
    expr = Untyped.FUNCTION_CALL(idOptionLong.name, List(Untyped.FUNCTION_CALL("Some", List(Untyped.REF("None"))))),
    expectedResult = Right(
      Typed.FUNCTION_CALL(idOptionLong.header,
                          List(Typed.FUNCTION_CALL(some.header, List(Typed.REF("None", OPTION(NOTHING))), OPTION(OPTION(NOTHING)))),
                          UNIT))
  )

  treeTypeTest(s"idOptionLong(SOME(CONST_LONG(3)))")(
    ctx = typeCheckerContext,
    expr =
      Untyped.FUNCTION_CALL(idOptionLong.name, List(Untyped.FUNCTION_CALL("Some", List(Untyped.FUNCTION_CALL("Some", List(Untyped.CONST_LONG(3))))))),
    expectedResult = Right(
      Typed.FUNCTION_CALL(
        idOptionLong.header,
        List(Typed.FUNCTION_CALL(some.header, List(Typed.FUNCTION_CALL(some.header, List(Typed.CONST_LONG(3)), OPTION(LONG))), OPTION(OPTION(LONG)))),
        UNIT
      )
    )
  )

  {
    import Untyped._

    errorTests(
      "can't define LET with the same name as already defined in scope" -> "already defined in the scope" -> BLOCK(
        Some(LET("X", CONST_LONG(1))),
        BLOCK(Some(LET("X", CONST_LONG(2))), TRUE)),
      "can't define LET with the same name as predefined constant" -> "already defined in the scope" -> BLOCK(Some(LET("None", CONST_LONG(2))), TRUE),
      "can't define LET with the same name as predefined function" -> "function with such name is predefined" -> BLOCK(
        Some(LET("Some", CONST_LONG(2))),
        TRUE),
      "BINARY_OP with wrong types"                   -> "The first operand is expected to be LONG" -> BINARY_OP(TRUE, SUM_OP, CONST_LONG(1)),
      "IF can't find common"                         -> "Can't find common type" -> IF(TRUE, TRUE, CONST_LONG(0)),
      "FUNCTION_CALL with wrong amount of arguments" -> "requires 2 arguments" -> FUNCTION_CALL(multiplierFunction.name, List(CONST_LONG(0))),
      "FUNCTION_CALL with upper type"                -> "Non-matching types" -> FUNCTION_CALL(unitOnNone.name, List(FUNCTION_CALL("Some", List(CONST_LONG(3))))),
      "FUNCTION_CALL with wrong type of argument"    -> "Typecheck failed: Non-matching types: expected: LONG, actual: BOOLEAN" -> FUNCTION_CALL(
        multiplierFunction.name,
        List(CONST_LONG(0), FALSE)),
      "FUNCTION_CALL with uncommon types for parameter T" -> "Can't match inferred types" -> FUNCTION_CALL(functionWithTwoPrarmsOfTheSameType.name,
                                                                                                           List(CONST_LONG(1),
                                                                                                                CONST_BYTEVECTOR(ByteVector.empty)))
    )
  }

  private def rootTypeTest(propertyName: String)(expr: Untyped.EXPR, expectedResult: TypeCheckResult[TYPE]): Unit = property(propertyName) {
    TypeChecker(TypeCheckerContext.empty, expr).map(_.tpe) match {
      case Right(x)    => Right(x) shouldBe expectedResult
      case e @ Left(_) => e shouldBe expectedResult
    }
  }

  private def errorTests(exprs: ((String, String), Untyped.EXPR)*): Unit = exprs.foreach {
    case ((label, error), input) =>
      property(s"Error: $label") {
        TypeChecker(typeCheckerContext, input) should produce(error)
      }
  }

  private def treeTypeTest(propertyName: String)(expr: Untyped.EXPR, expectedResult: TypeCheckResult[Typed.EXPR], ctx: TypeCheckerContext): Unit =
    property(propertyName) {
      TypeChecker(ctx, expr) shouldBe expectedResult
    }

  private def treeTypeErasureTests(exprs: (String, Typed.EXPR)*): Unit = exprs.foreach {
    case (exprName, expected) =>
      property(exprName) {
        val erased = erase(expected)
        TypeChecker(TypeCheckerContext.empty, erased) shouldBe Right(expected)
      }
  }

  private def erase(expr: Typed.EXPR): Untyped.EXPR = {
    import cats.syntax.apply._

    def aux(root: Typed.EXPR): Coeval[Untyped.EXPR] =
      Coeval.defer(root match {
        case x: Typed.CONST_LONG       => Coeval(Untyped.CONST_LONG(x.t))
        case x: Typed.CONST_BYTEVECTOR => Coeval(Untyped.CONST_BYTEVECTOR(x.bs))
        case x: Typed.CONST_STRING     => Coeval(Untyped.CONST_STRING(x.s))
        case Typed.TRUE                => Coeval(Untyped.TRUE)
        case Typed.FALSE               => Coeval(Untyped.FALSE)
        case getter: Typed.GETTER      => aux(getter.ref).map(Untyped.GETTER(_, getter.field))
        case binaryOp: Typed.BINARY_OP => (aux(binaryOp.a), aux(binaryOp.b)).mapN(Untyped.BINARY_OP(_, binaryOp.kind, _))
        case block: Typed.BLOCK =>
          aux(block.body).flatMap { t =>
            val x = Untyped.BLOCK(let = None, body = t)
            block.let match {
              case None => Coeval(x)
              case Some(let) =>
                aux(let.value).map {
                  case let: Untyped.LET => x.copy(let = Some(let))
                  case _                => throw new IllegalStateException()
                }
            }
          }
        case ifExpr: Typed.IF => (aux(ifExpr.cond), aux(ifExpr.ifTrue), aux(ifExpr.ifFalse)).mapN(Untyped.IF)
        case ref: Typed.REF   => Coeval(Untyped.REF(ref.key))
        case f: Typed.FUNCTION_CALL =>
          import cats.instances.vector._
          import cats.syntax.all._
          val auxedArgs: Vector[Coeval[Untyped.EXPR]]          = f.args.map(aux).toVector
          val sequencedAuxedArgs: Coeval[Vector[Untyped.EXPR]] = auxedArgs.sequence
          sequencedAuxedArgs.map(args => Untyped.FUNCTION_CALL(f.function.name, args.toList))
      })

    aux(expr)()
  }
}
