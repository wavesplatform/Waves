package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang.TypeChecker.{Context, Defs, TypeCheckResult}
import monix.eval.Coeval
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector
import scorex.crypto.encode.Base58

class TypeCheckerTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  private val pointType = CUSTOMTYPE("Point", List("x" -> INT, "y" -> INT))

  rootTypeTest("successful on very deep expressions (stack overflow check)")(
    expr = (1 to 100000).foldLeft[Untyped.EXPR](Untyped.CONST_INT(0))((acc, _) => Untyped.BINARY_OP(acc, SUM_OP, Untyped.CONST_INT(1))),
    expectedResult = Right(INT)
  )

  {
    import Typed._

    treeTypeErasureTests(
      "CONST_INT"        -> CONST_INT(0),
      "CONST_BYTEVECTOR" -> CONST_BYTEVECTOR(ByteVector(1, 2, 3)),
      "TRUE"             -> TRUE,
      "FALSE"            -> FALSE,
      "NONE"             -> NONE,
      "SUM"              -> BINARY_OP(CONST_INT(0), SUM_OP, CONST_INT(1), INT),
      "AND"              -> BINARY_OP(TRUE, AND_OP, FALSE, BOOLEAN),
      "OR"               -> BINARY_OP(TRUE, OR_OP, FALSE, BOOLEAN),
      "EQ(INT)"          -> BINARY_OP(CONST_INT(0), EQ_OP, CONST_INT(1), BOOLEAN),
      "EQ(BOOL)"         -> BINARY_OP(TRUE, EQ_OP, FALSE, BOOLEAN),
      "GT"               -> BINARY_OP(CONST_INT(0), GT_OP, CONST_INT(1), BOOLEAN),
      "GE"               -> BINARY_OP(CONST_INT(0), GE_OP, CONST_INT(1), BOOLEAN),
      "SIG_VERIFY" -> SIG_VERIFY(
        message = CONST_BYTEVECTOR(ByteVector(Base58.decode("333").get)),
        signature = CONST_BYTEVECTOR(ByteVector(Base58.decode("222").get)),
        publicKey = CONST_BYTEVECTOR(ByteVector(Base58.decode("111").get))
      ),
      "IS_DEFINED(NONE)" -> IS_DEFINED(NONE),
      "IS_DEFINED(SOME)" -> IS_DEFINED(SOME(TRUE, OPTION(BOOLEAN))),
      "LET"        -> LET("x", CONST_INT(0)),
      "BLOCK" -> BLOCK(
        let = None,
        body = CONST_INT(0),
        tpe = INT
      ),
      "IF" -> IF(
        cond = TRUE,
        ifTrue = SOME(TRUE, OPTION(BOOLEAN)),
        ifFalse = NONE,
        tpe = OPTION(BOOLEAN)
      ),
      "GET(SOME)" -> GET(SOME(TRUE, OPTION(BOOLEAN)), BOOLEAN),
      "GET(NONE)" -> GET(NONE, NOTHING),
      "SOME" -> SOME(TRUE, OPTION(BOOLEAN)),
      "BLOCK(LET(X), REF(y) = x)" -> BLOCK(
        let = Some(LET("x", CONST_INT(0))),
        body = LET("y", REF("x", INT)),
        tpe = UNIT
      )
    )
  }

  treeTypeTest("GETTER")(
    predefTypes = Map(pointType.name -> pointType),
    varDefs = Map("p"                -> TYPEREF("Point")),
    expr = Untyped.GETTER(
      ref = Untyped.REF("p"),
      field = "x"
    ),
    expectedResult = Right(
      Typed.GETTER(
        ref = Typed.REF("p", TYPEREF("Point")),
        field = "x",
        tpe = INT
      ))
  )

  treeTypeTest("REF(OBJECT)")(
    predefTypes = Map(pointType.name -> pointType),
    varDefs = Map("p"                -> TYPEREF("Point")),
    expr = Untyped.REF("p"),
    expectedResult = Right(Typed.REF("p", TYPEREF("Point")))
  )

  treeTypeTest("REF x = y")(
    predefTypes = Map(pointType.name -> pointType),
    varDefs = Map("p"                -> TYPEREF("Point")),
    expr = Untyped.REF("p"),
    expectedResult = Right(Typed.REF("p", TYPEREF("Point")))
  )

  {
    import Untyped._

    errorTests(
      "BINARY_OP wrong types" -> "The first operand is expected to be INT" -> BINARY_OP(TRUE, SUM_OP, CONST_INT(1)),
      "IF con't find common" -> "Can't find common type" -> IF(TRUE, TRUE, CONST_INT(0))
    )
  }

  private def rootTypeTest(propertyName: String)(expr: Untyped.EXPR,
                                                 expectedResult: TypeCheckResult[TYPE],
                                                 varDefs: Defs = Map.empty,
                                                 predefTypes: Map[String, CUSTOMTYPE] = Map.empty): Unit = property(propertyName) {
    TypeChecker(Context(predefTypes, varDefs), expr).map(_.tpe) match {
      case Right(x)    => Right(x) shouldBe expectedResult
      case e @ Left(_) => e shouldBe expectedResult
    }
  }

  private def errorTests(exprs: ((String, String), Untyped.EXPR)*): Unit = exprs.foreach {
    case ((label, error), input) =>
      property(s"Error: $label") {
        TypeChecker(Context.empty, input) should produce(error)
      }
  }

  private def treeTypeTest(propertyName: String)(expr: Untyped.EXPR,
                                                 expectedResult: TypeCheckResult[Typed.EXPR],
                                                 predefTypes: Map[String, CUSTOMTYPE],
                                                 varDefs: Defs): Unit = property(propertyName) {
    TypeChecker(Context(predefTypes, varDefs), expr) shouldBe expectedResult
  }

  private def treeTypeErasureTests(exprs: (String, Typed.EXPR)*): Unit = exprs.foreach {
    case (exprName, expected) =>
      property(exprName) {
        val erased = erase(expected)
        TypeChecker(Context.empty, erased) shouldBe Right(expected)
      }
  }

  private def erase(expr: Typed.EXPR): Untyped.EXPR = {
    import cats.syntax.apply._

    def aux(root: Typed.EXPR): Coeval[Untyped.EXPR] =
      Coeval.defer(root match {
        case x: Typed.CONST_INT        => Coeval(Untyped.CONST_INT(x.t))
        case x: Typed.CONST_BYTEVECTOR => Coeval(Untyped.CONST_BYTEVECTOR(x.bs))
        case Typed.TRUE                => Coeval(Untyped.TRUE)
        case Typed.FALSE               => Coeval(Untyped.FALSE)
        case Typed.NONE                => Coeval(Untyped.NONE)

        case getter: Typed.GETTER        => aux(getter.ref).map(Untyped.GETTER(_, getter.field))
        case binaryOp: Typed.BINARY_OP   => (aux(binaryOp.a), aux(binaryOp.b)).mapN(Untyped.BINARY_OP(_, binaryOp.kind, _))
        case sigVerify: Typed.SIG_VERIFY => (aux(sigVerify.message), aux(sigVerify.signature), aux(sigVerify.publicKey)).mapN(Untyped.SIG_VERIFY)
        case isDefined: Typed.IS_DEFINED => aux(isDefined.opt).map(Untyped.IS_DEFINED)
        case let: Typed.LET              => aux(let.value).map(Untyped.LET(let.name, _))
        case block: Typed.BLOCK =>
          aux(block.body).flatMap { t =>
            val x = Untyped.BLOCK(let = None, body = t)
            block.let match {
              case None => Coeval(x)
              case Some(let) =>
                aux(let).map {
                  case let: Untyped.LET => x.copy(let = Some(let))
                  case _                => throw new IllegalStateException()
                }
            }
          }
        case ifExpr: Typed.IF => (aux(ifExpr.cond), aux(ifExpr.ifTrue), aux(ifExpr.ifFalse)).mapN(Untyped.IF)
        case ref: Typed.REF   => Coeval(Untyped.REF(ref.key))
        case get: Typed.GET   => aux(get.opt).map(Untyped.GET)
        case some: Typed.SOME => aux(some.t).map(Untyped.SOME)
      })

    aux(expr)()
  }
}
