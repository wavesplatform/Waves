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
    expr = (1 to 100000).foldLeft[Untyped.EXPR](Untyped.CONST_INT(0))((acc, _) =>
      Untyped.BINARY_OP(Untyped.BLOCK(None, acc), SUM_OP, Untyped.BLOCK(None, Untyped.CONST_INT(1)))),
    expectedResult = Right(INT)
  )

  {
    import Typed._

    treeTypeErasureTest(
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
        message = BLOCK(None, CONST_BYTEVECTOR(ByteVector(Base58.decode("333").get)), BYTEVECTOR),
        signature = BLOCK(None, CONST_BYTEVECTOR(ByteVector(Base58.decode("222").get)), BYTEVECTOR),
        publicKey = BLOCK(None, CONST_BYTEVECTOR(ByteVector(Base58.decode("111").get)), BYTEVECTOR)
      ),
      "IS_DEFINED" -> IS_DEFINED(BLOCK(None, NONE, OPTION(NOTHING))),
      "LET"        -> LET("x", BLOCK(None, CONST_INT(0), INT)),
      "BLOCK" -> BLOCK(
        let = None,
        body = CONST_INT(0),
        tpe = INT
      ),
      "IF" -> IF(
        cond = BLOCK(None, TRUE, BOOLEAN),
        ifTrue = BLOCK(None, SOME(BLOCK(None, TRUE, BOOLEAN), OPTION(BOOLEAN)), OPTION(BOOLEAN)),
        ifFalse = BLOCK(None, NONE, OPTION(NOTHING)),
        tpe = OPTION(BOOLEAN)
      ),
      "GET(SOME)" -> GET(
        opt = BLOCK(
          let = None,
          body = SOME(BLOCK(None, TRUE, BOOLEAN), OPTION(BOOLEAN)),
          tpe = OPTION(BOOLEAN)
        ),
        tpe = BOOLEAN
      ),
      "GET(NONE)" -> GET(
        opt = BLOCK(
          let = None,
          body = BLOCK(None, NONE, OPTION(NOTHING)),
          tpe = OPTION(NOTHING)
        ),
        tpe = NOTHING
      ),
      "SOME" -> SOME(BLOCK(None, TRUE, BOOLEAN), OPTION(BOOLEAN)),
      "BLOCK(LET(X), REF(y) = x)" -> BLOCK(
        let = Some(
          LET(
            name = "x",
            value = BLOCK(
              let = None,
              body = CONST_INT(0),
              tpe = INT
            )
          )),
        body = BLOCK(
          let = None,
          body = LET("y", BLOCK(None, REF("x", INT), INT)),
          tpe = UNIT
        ),
        tpe = UNIT
      )
    )
  }

  treeTypeTest("GETTER")(
    predefTypes = Map(pointType.name -> pointType),
    varDefs = Map("p"                -> TYPEREF("Point")),
    expr = Untyped.GETTER(
      ref = Untyped.BLOCK(None, Untyped.REF("p")),
      field = "x"
    ),
    expectedResult = Right(
      Typed.GETTER(
        ref = Typed.BLOCK(None, Typed.REF("p", TYPEREF("Point")), TYPEREF("Point")),
        field = "x",
        tpe = INT
      ))
  )

  // ==== cut

  //  property("Typechecking") {
  //    ev(expr = BINARY_OP(CONST_INT(2), EQ_OP, CONST_INT(2), INT)) shouldBe Right(true)
  //  }
  //
  //  property("successful Typechecking Some") {
  //    ev(expr = BINARY_OP(SOME(CONST_INT(2), OPTION(INT)), EQ_OP, SOME(CONST_INT(2), OPTION(INT)), BOOLEAN)) shouldBe Right(true)
  //  }
  //
  //  property("successful Typechecking Option") {
  //    ev(expr = BINARY_OP(SOME(CONST_INT(2), OPTION(INT)), EQ_OP, NONE, BOOLEAN)) shouldBe Right(false)
  //    ev(expr = BINARY_OP(NONE, EQ_OP, SOME(CONST_INT(2), OPTION(INT)), BOOLEAN)) shouldBe Right(false)
  //  }
  //
  //  property("successful nested Typechecking Option") {
  //    ev(expr = BINARY_OP(SOME(SOME(SOME(CONST_INT(2), OPTION(INT)))), EQ_OP, NONE, BOOLEAN)) shouldBe Right(false)
  //    ev(expr = BINARY_OP(SOME(NONE), EQ_OP, SOME(SOME(CONST_INT(2))), BOOLEAN)) shouldBe Right(false)
  //  }
  //
  //  property("fails if nested Typechecking Option finds mismatch") {
  //    ev(expr = BINARY_OP(SOME(SOME(FALSE)), EQ_OP, SOME(SOME(CONST_INT(2))), BOOLEAN)) should produce("Typecheck failed")
  //  }
  //
  //  property("successful resolve strongest type") {
  //    ev(expr = GET(IF(TRUE, SOME(CONST_INT(3)), SOME(CONST_INT(2)), OPTION(INT)))) shouldBe Right(3)
  //    ev(expr = GET(IF(TRUE, SOME(CONST_INT(3)), NONE))) shouldBe Right(3)
  //    ev(expr = BINARY_OP(CONST_INT(1), SUM_OP, GET(IF(TRUE, SOME(CONST_INT(3)), NONE)), BOOLEAN)) shouldBe Right(4)
  //  }
  //  property("fails if types do not match") {
  //    ev(
  //      expr = BLOCK(
  //        Some(LET("x", CONST_INT(3))),
  //        BLOCK(Some(LET("y", BINARY_OP(CONST_INT(3), EQ_OP, CONST_INT(0), INT))), BINARY_OP(REF("x", INT), EQ_OP, REF("y", INT), INT), INT),
  //        INT
  //      )) should produce("Typecheck failed")
  //  }
  // property("fails if 'IF' branches lead to different types") {
  // ev(expr = IF(BINARY_OP(CONST_INT(1), EQ_OP, CONST_INT(2), INT), CONST_INT(0), CONST_BYTEVECTOR(ByteVector.empty), INT)) should produce("Typecheck failed")
  //}

  // ====

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

  private def rootTypeTest(propertyName: String)(expr: Untyped.EXPR,
                                                 expectedResult: TypeCheckResult[TYPE],
                                                 varDefs: Defs = Map.empty,
                                                 predefTypes: Map[String, CUSTOMTYPE] = Map.empty): Unit = property(propertyName) {
    TypeChecker(Context(predefTypes, varDefs), expr).map(_.tpe) match {
      case Right(x)    => Right(x) shouldBe expectedResult
      case e @ Left(_) => e shouldBe expectedResult
    }
  }

  private def treeTypeTest(propertyName: String)(expr: Untyped.EXPR,
                                                 expectedResult: TypeCheckResult[Typed.EXPR],
                                                 predefTypes: Map[String, CUSTOMTYPE],
                                                 varDefs: Defs): Unit = property(propertyName) {
    TypeChecker(Context(predefTypes, varDefs), expr) shouldBe expectedResult
  }

  private def treeTypeErasureTest(exprs: (String, Typed.EXPR)*): Unit = exprs.foreach {
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
