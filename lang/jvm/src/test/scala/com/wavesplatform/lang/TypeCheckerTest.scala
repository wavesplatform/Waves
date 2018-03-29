package com.wavesplatform.lang

import com.wavesplatform.lang
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang.ctx._
import com.wavesplatform.lang.TypeChecker.{TypeCheckResult, TypeCheckerContext, TypeDefs}
import com.wavesplatform.lang.WavesContextImpl.none
import com.wavesplatform.lang.testing.ScriptGen
import monix.eval.Coeval
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector

class TypeCheckerTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  private val pointType = PredefType("Point", List("x" -> LONG, "y" -> LONG))

  property("should infer generic function return type") {
    import Untyped._
    val idFunction = PredefFunction("idFunc", TYPEPARAM('T'), List("p1" -> TYPEPARAM('T')))(Right(_))
    val ctx        = Context(Map.empty, Map.empty, functions = Map((idFunction.name, idFunction)))
    val Right(v)   = TypeChecker(TypeCheckerContext.fromContext(ctx), FUNCTION_CALL(idFunction.name, List(CONST_LONG(1))))

    v.tpe shouldBe LONG
  }

  property("should infer inner types") {
    import Untyped._
    val genericFuncWithOptionParam = PredefFunction("idFunc", TYPEPARAM('T'), List("p1" -> OPTIONTYPEPARAM(TYPEPARAM('T')))) {
      case Some(vl) :: Nil => Right(vl)
      case _               => Left("extracting from empty option")
    }

    val optionFunc = PredefFunction("optionFunc", OPTION(LONG), List.empty)(null)

    val ctx =
      Context(
        Map.empty,
        Map.empty,
        functions = Map(
          genericFuncWithOptionParam.name -> genericFuncWithOptionParam,
          optionFunc.name                 -> optionFunc,
          WavesContextImpl.some.name      -> WavesContextImpl.some
        )
      )
    val Right(v) = TypeChecker(TypeCheckerContext.fromContext(ctx),
                               FUNCTION_CALL(genericFuncWithOptionParam.name, List(FUNCTION_CALL(optionFunc.name, List.empty))))

    v.tpe shouldBe LONG
  }

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
    ctx =
      TypeCheckerContext(predefTypes = Map.empty, varDefs = Map.empty, functionDefs = Map(multiplierFunction.name -> multiplierFunction.signature)),
    expr = Untyped.FUNCTION_CALL(multiplierFunction.name, List(Untyped.CONST_LONG(1), Untyped.CONST_LONG(2))),
    expectedResult = Right(Typed.FUNCTION_CALL(multiplierFunction.name, List(Typed.CONST_LONG(1), Typed.CONST_LONG(2)), LONG))
  )

  private val optFunc  = PredefFunction("OPTFUNC", UNIT, List("opt"  -> OPTION(OPTION(LONG))))(_ => Right(()))
  private val noneFunc = PredefFunction("NONEFUNC", UNIT, List("opt" -> OPTION(NOTHING)))(_ => Right(()))
  private val typeCheckerCtx = TypeCheckerContext(
    predefTypes = Map.empty,
    varDefs = Map(("None", none.tpe)),
    functionDefs = Map(lang.WavesContextImpl.some.name -> lang.WavesContextImpl.some.signature,
                       optFunc.name                    -> optFunc.signature,
                       noneFunc.name                   -> noneFunc.signature)
  )

  private val genericFunc = PredefFunction("idFunc", TYPEPARAM('T'), List("p1" -> TYPEPARAM('T'), "p2" -> TYPEPARAM('T')))(Right(_))
  treeTypeTest(s"NONEFUNC(NONE)")(
    ctx = typeCheckerCtx,
    expr = Untyped.FUNCTION_CALL(noneFunc.name, List(Untyped.REF("None"))),
    expectedResult = Right(Typed.FUNCTION_CALL(noneFunc.name, List(Typed.REF("None", OPTION(NOTHING))), UNIT))
  )

  treeTypeTest(s"OPTFUNC(NONE)")(
    ctx = typeCheckerCtx,
    expr = Untyped.FUNCTION_CALL(optFunc.name, List(Untyped.REF("None"))),
    expectedResult = Right(Typed.FUNCTION_CALL(optFunc.name, List(Typed.REF("None", OPTION(NOTHING))), UNIT))
  )

  treeTypeTest(s"OPTFUNC(SOME(NONE))")(
    ctx = typeCheckerCtx,
    expr = Untyped.FUNCTION_CALL(optFunc.name, List(Untyped.FUNCTION_CALL("Some", List(Untyped.REF("None"))))),
    expectedResult = Right(
      Typed.FUNCTION_CALL(optFunc.name, List(Typed.FUNCTION_CALL("Some", List(Typed.REF("None", OPTION(NOTHING))), OPTION(OPTION(NOTHING)))), UNIT))
  )

  treeTypeTest(s"OPTFUNC(SOME(CONST_LONG(3)))")(
    ctx = typeCheckerCtx,
    expr = Untyped.FUNCTION_CALL(optFunc.name, List(Untyped.FUNCTION_CALL("Some", List(Untyped.FUNCTION_CALL("Some", List(Untyped.CONST_LONG(3))))))),
    expectedResult = Right(
      Typed.FUNCTION_CALL(
        optFunc.name,
        List(Typed.FUNCTION_CALL("Some", List(Typed.FUNCTION_CALL("Some", List(Typed.CONST_LONG(3)), OPTION(LONG))), OPTION(OPTION(LONG)))),
        UNIT)
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
      "FUNCTION_CALL with upper type"                -> "Non-matching types" -> FUNCTION_CALL(noneFunc.name, List(FUNCTION_CALL("Some", List(CONST_LONG(3))))),
      "FUNCTION_CALL with wrong type of argument"    -> "Typecheck failed: Non-matching types: expected: LONG, actual: BOOLEAN" -> FUNCTION_CALL(
        multiplierFunction.name,
        List(CONST_LONG(0), FALSE)),
      "FUNCTION_CALL with uncommon types for parameter T" -> "Can't match inferred types" -> FUNCTION_CALL(genericFunc.name,
                                                                                                           List(CONST_LONG(1),
                                                                                                                CONST_BYTEVECTOR(ByteVector.empty)))
    )
  }

  private def rootTypeTest(propertyName: String)(expr: Untyped.EXPR,
                                                 expectedResult: TypeCheckResult[TYPE],
                                                 varDefs: TypeDefs = Map.empty,
                                                 predefTypes: Map[String, PredefType] = Map.empty): Unit = property(propertyName) {
    TypeChecker(TypeCheckerContext(predefTypes, varDefs, Map.empty), expr).map(_.tpe) match {
      case Right(x)    => Right(x) shouldBe expectedResult
      case e @ Left(_) => e shouldBe expectedResult
    }
  }

  private def errorTests(exprs: ((String, String), Untyped.EXPR)*): Unit = exprs.foreach {
    case ((label, error), input) =>
      property(s"Error: $label") {
        TypeChecker(
          TypeCheckerContext(
            Map.empty,
            Map(("None", none.tpe)),
            Map(
              multiplierFunction.name         -> multiplierFunction.signature,
              noneFunc.name                   -> noneFunc.signature,
              genericFunc.name                -> genericFunc.signature,
              lang.WavesContextImpl.some.name -> lang.WavesContextImpl.some.signature
            )
          ),
          input
        ) should produce(error)
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
          sequencedAuxedArgs.map(args => Untyped.FUNCTION_CALL(f.functionName, args.toList))
      })

    aux(expr)()
  }
}
