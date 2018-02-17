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
    expr = (1 to 100000).foldLeft[Untyped.EXPR](Untyped.CONST_INT(0))((acc, _) => Untyped.SUM(Untyped.BLOCK(None, acc), Untyped.BLOCK(None, Untyped.CONST_INT(1)))),
    expectedResult = Right(INT)
  )

  {
    import Typed._

    treeTypeErasureTest(
      "CONST_INT" -> CONST_INT(0),
      "CONST_BYTEVECTOR" -> CONST_BYTEVECTOR(ByteVector(1, 2, 3)),
      "TRUE" -> TRUE,
      "FALSE" -> FALSE,
      "NONE" -> NONE,
      "SUM" -> SUM(BLOCK(None, CONST_INT(0), INT), BLOCK(None, CONST_INT(1), INT)),
      "AND" -> AND(BLOCK(None, TRUE, BOOLEAN), BLOCK(None, FALSE, BOOLEAN)),
      "OR" -> OR(BLOCK(None, TRUE, BOOLEAN), BLOCK(None, FALSE, BOOLEAN)),
      "EQ" -> EQ(BLOCK(None, CONST_INT(0), INT), BLOCK(None, CONST_INT(1), INT)),
      "GT" -> GT(BLOCK(None, CONST_INT(0), INT), BLOCK(None, CONST_INT(1), INT)),
      "GE" -> GE(BLOCK(None, CONST_INT(0), INT), BLOCK(None, CONST_INT(1), INT)),
      "SIG_VERIFY" -> SIG_VERIFY(
        message = BLOCK(None, CONST_BYTEVECTOR(ByteVector(Base58.decode("333").get)), BYTEVECTOR),
        signature = BLOCK(None, CONST_BYTEVECTOR(ByteVector(Base58.decode("222").get)), BYTEVECTOR),
        publicKey = BLOCK(None, CONST_BYTEVECTOR(ByteVector(Base58.decode("111").get)), BYTEVECTOR)
      ),
      "IS_DEFINED" -> IS_DEFINED(BLOCK(None, NONE, OPTION(NOTHING))),
      "LET" -> LET("x", BLOCK(None, CONST_INT(0), INT)),
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
        let = Some(LET(
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
    varDefs = Map("p" -> TYPEREF("Point")),
    expr = Untyped.GETTER(
      ref = Untyped.BLOCK(None, Untyped.REF("p")),
      field = "x"
    ),
    expectedResult = Right(Typed.GETTER(
      ref = Typed.BLOCK(None, Typed.REF("p", TYPEREF("Point")), TYPEREF("Point")),
      field = "x",
      tpe = INT
    ))
  )

  treeTypeTest("REF(OBJECT)")(
    predefTypes = Map(pointType.name -> pointType),
    varDefs = Map("p" -> TYPEREF("Point")),
    expr = Untyped.REF("p"),
    expectedResult = Right(Typed.REF("p", TYPEREF("Point")))
  )

  treeTypeTest("REF x = y")(
    predefTypes = Map(pointType.name -> pointType),
    varDefs = Map("p" -> TYPEREF("Point")),
    expr = Untyped.REF("p"),
    expectedResult = Right(Typed.REF("p", TYPEREF("Point")))
  )

  private def rootTypeTest(propertyName: String)
                          (expr: Untyped.EXPR,
                           expectedResult: TypeCheckResult[TYPE],
                           varDefs: Defs = Map.empty,
                           predefTypes: Map[String, CUSTOMTYPE] = Map.empty): Unit = property(propertyName) {
    TypeChecker(Context(predefTypes, varDefs), expr).map(_.tpe) match {
      case Right(x) => Right(x) shouldBe expectedResult
      case e@Left(_) => e shouldBe expectedResult
    }
  }

  private def treeTypeTest(propertyName: String)
                          (expr: Untyped.EXPR,
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

    def aux(root: Typed.EXPR): Coeval[Untyped.EXPR] = Coeval.defer(root match {
      case x: Typed.CONST_INT => Coeval(Untyped.CONST_INT(x.t))
      case x: Typed.CONST_BYTEVECTOR => Coeval(Untyped.CONST_BYTEVECTOR(x.bs))
      case Typed.TRUE => Coeval(Untyped.TRUE)
      case Typed.FALSE => Coeval(Untyped.FALSE)
      case Typed.NONE => Coeval(Untyped.NONE)

      case getter: Typed.GETTER => aux(getter.ref).map {
        case (ref: Untyped.BLOCK) => Untyped.GETTER(field = getter.field, ref = ref)
        case _ => throw new IllegalStateException()
      }
      case sum: Typed.SUM => (aux(sum.a), aux(sum.b)).mapN {
        case (first: Untyped.BLOCK, second: Untyped.BLOCK) => Untyped.SUM(first, second)
        case _ => throw new IllegalStateException()
      }
      case and: Typed.AND => (aux(and.a), aux(and.b)).mapN {
        case (first: Untyped.BLOCK, second: Untyped.BLOCK) => Untyped.AND(first, second)
        case _ => throw new IllegalStateException()
      }
      case or: Typed.OR => (aux(or.a), aux(or.b)).mapN {
        case (first: Untyped.BLOCK, second: Untyped.BLOCK) => Untyped.OR(first, second)
        case _ => throw new IllegalStateException()
      }
      case eq: Typed.EQ => (aux(eq.a), aux(eq.b)).mapN {
        case (first: Untyped.BLOCK, second: Untyped.BLOCK) => Untyped.EQ(first, second)
        case _ => throw new IllegalStateException()
      }
      case gt: Typed.GT => (aux(gt.a), aux(gt.b)).mapN {
        case (first: Untyped.BLOCK, second: Untyped.BLOCK) => Untyped.GT(first, second)
        case _ => throw new IllegalStateException()
      }
      case ge: Typed.GE => (aux(ge.a), aux(ge.b)).mapN {
        case (first: Untyped.BLOCK, second: Untyped.BLOCK) => Untyped.GE(first, second)
        case _ => throw new IllegalStateException()
      }
      case sigVerify: Typed.SIG_VERIFY => (aux(sigVerify.message), aux(sigVerify.signature), aux(sigVerify.publicKey)).mapN {
        case (message: Untyped.BLOCK, signature: Untyped.BLOCK, publicKey: Untyped.BLOCK) =>
          Untyped.SIG_VERIFY(message = message, signature = signature, publicKey = publicKey)
        case _ => throw new IllegalStateException()
      }
      case isDefined: Typed.IS_DEFINED => aux(isDefined.opt).map {
        case (t: Untyped.BLOCK) => Untyped.IS_DEFINED(opt = t)
        case _ => throw new IllegalStateException()
      }
      case let: Typed.LET => aux(let.value).map {
        case (value: Untyped.BLOCK) => Untyped.LET(name = let.name, value = value)
        case _ => throw new IllegalStateException()
      }
      case block: Typed.BLOCK => aux(block.body).flatMap { t =>
        val x = Untyped.BLOCK(let = None, body = t)
        block.let match {
          case None => Coeval(x)
          case Some(let) => aux(let).map {
            case let: Untyped.LET => x.copy(let = Some(let))
            case _ => throw new IllegalStateException()
          }
        }
      }
      case ifExpr: Typed.IF => (aux(ifExpr.cond), aux(ifExpr.ifTrue), aux(ifExpr.ifFalse)).mapN {
        case (cond: Untyped.BLOCK, ifTrue: Untyped.BLOCK, ifFalse: Untyped.BLOCK) =>
          Untyped.IF(cond = cond, ifTrue = ifTrue, ifFalse = ifFalse)
        case _ => throw new IllegalStateException()
      }
      case ref: Typed.REF => Coeval(Untyped.REF(key = ref.key))
      case get: Typed.GET => aux(get.opt).map {
        case (t: Untyped.BLOCK) => Untyped.GET(opt = t)
        case _ => throw new IllegalStateException()
      }
      case some: Typed.SOME => aux(some.t).map {
        case (t: Untyped.BLOCK) => Untyped.SOME(t = t)
        case _ => throw new IllegalStateException()
      }
    })

    aux(expr)()
  }
}
