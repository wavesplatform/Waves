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
    expr = (1 to 100000).foldLeft[Terms.Expr](CONST_INT(0))((acc, _) => SUM(Block(None, acc), Block(None, CONST_INT(1)))),
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
      "SUM" -> SUM(Block(None, CONST_INT(0), INT), Block(None, CONST_INT(1), INT)),
      "AND" -> AND(Block(None, TRUE, BOOLEAN), Block(None, FALSE, BOOLEAN)),
      "OR" -> OR(Block(None, TRUE, BOOLEAN), Block(None, FALSE, BOOLEAN)),
      "EQ" -> EQ(Block(None, CONST_INT(0), INT), Block(None, CONST_INT(1), INT)),
      "GT" -> GT(Block(None, CONST_INT(0), INT), Block(None, CONST_INT(1), INT)),
      "GE" -> GE(Block(None, CONST_INT(0), INT), Block(None, CONST_INT(1), INT)),
      "SIG_VERIFY" -> SIG_VERIFY(
        message = Block(None, CONST_BYTEVECTOR(ByteVector(Base58.decode("333").get)), BYTEVECTOR),
        signature = Block(None, CONST_BYTEVECTOR(ByteVector(Base58.decode("222").get)), BYTEVECTOR),
        publicKey = Block(None, CONST_BYTEVECTOR(ByteVector(Base58.decode("111").get)), BYTEVECTOR)
      ),
      "IS_DEFINED" -> IS_DEFINED(Block(None, NONE, OPTION(NOTHING))),
      "LET" -> LET("x", Block(None, CONST_INT(0), INT)),
      "BLOCK" -> Block(
        let = None,
        t = CONST_INT(0),
        exprType = INT
      ),
      "IF" -> IF(
        cond = Block(None, TRUE, BOOLEAN),
        ifTrue = Block(None, SOME(Block(None, TRUE, BOOLEAN), OPTION(BOOLEAN)), OPTION(BOOLEAN)),
        ifFalse = Block(None, NONE, OPTION(NOTHING)),
        exprType = OPTION(BOOLEAN)
      ),
      "GET(SOME)" -> GET(
        t = Block(
          let = None,
          t = SOME(Block(None, TRUE, BOOLEAN), OPTION(BOOLEAN)),
          exprType = OPTION(BOOLEAN)
        ),
        exprType = BOOLEAN
      ),
      "GET(NONE)" -> GET(
        t = Block(
          let = None,
          t = Block(None, NONE, OPTION(NOTHING)),
          exprType = OPTION(NOTHING)
        ),
        exprType = NOTHING
      ),
      "SOME" -> SOME(Block(None, TRUE, BOOLEAN), OPTION(BOOLEAN)),

      "BLOCK(LET(X), REF(y) = x)" -> Block(
        let = Some(LET(
          name = "x",
          value = Block(
            let = None,
            t = CONST_INT(0),
            exprType = INT
          )
        )),
        t = Block(
          let = None,
          t = LET("y", Block(None, REF("x", INT), INT)),
          exprType = UNIT
        ),
        exprType = UNIT
      )
    )
  }

  treeTypeTest("GETTER")(
    predefTypes = Map(pointType.name -> pointType),
    varDefs = Map("p" -> TYPEREF("Point")),
    expr = GETTER(
      ref = Block(None, REF("p")),
      field = "x"
    ),
    expectedResult = Right(Typed.GETTER(
      ref = Typed.Block(None, Typed.REF("p", TYPEREF("Point")), TYPEREF("Point")),
      field = "x",
      exprType = INT
    ))
  )

  treeTypeTest("REF(OBJECT)")(
    predefTypes = Map(pointType.name -> pointType),
    varDefs = Map("p" -> TYPEREF("Point")),
    expr = REF("p"),
    expectedResult = Right(Typed.REF("p", TYPEREF("Point")))
  )

  treeTypeTest("REF x = y")(
    predefTypes = Map(pointType.name -> pointType),
    varDefs = Map("p" -> TYPEREF("Point")),
    expr = REF("p"),
    expectedResult = Right(Typed.REF("p", TYPEREF("Point")))
  )

  private def rootTypeTest(propertyName: String)
                          (expr: Expr,
                           expectedResult: TypeCheckResult[Type],
                           varDefs: Defs = Map.empty,
                           predefTypes: Map[String, CUSTOMTYPE] = Map.empty): Unit = property(propertyName) {
    TypeChecker(Context(predefTypes, varDefs), expr).map(_.exprType) match {
      case Right(x) => Right(x) shouldBe expectedResult
      case e@Left(_) => e shouldBe expectedResult
    }
  }

  private def treeTypeTest(propertyName: String)
                          (expr: Expr,
                           expectedResult: TypeCheckResult[Typed.Expr],
                           predefTypes: Map[String, CUSTOMTYPE] = Map.empty,
                           varDefs: Defs = Map.empty): Unit = property(propertyName) {
    TypeChecker(Context(predefTypes, varDefs), expr) shouldBe expectedResult
  }

  private def treeTypeErasureTest(exprs: (String, Typed.Expr)*): Unit = exprs.foreach {
    case (exprName, expected) =>
      property(exprName) {
        val erased = erase(expected)
        TypeChecker(Context.empty, erased) shouldBe Right(expected)
      }
  }

  private def erase(expr: Typed.Expr): Expr = {
    import cats.syntax.apply._

    def aux(root: Typed.Expr): Coeval[Expr] = Coeval.defer(root match {
      case x: Typed.CONST_INT => Coeval(CONST_INT(x.t))
      case x: Typed.CONST_BYTEVECTOR => Coeval(CONST_BYTEVECTOR(x.bs))
      case Typed.TRUE => Coeval(TRUE)
      case Typed.FALSE => Coeval(FALSE)
      case Typed.NONE => Coeval(NONE)

      case getter: Typed.GETTER => aux(getter.ref).map {
        case (ref: Block) => GETTER(field = getter.field, ref = ref, exprType = None)
        case _ => throw new IllegalStateException()
      }
      case sum: Typed.SUM => (aux(sum.i1), aux(sum.i2)).mapN {
        case (first: Block, second: Block) => SUM(first, second)
        case _ => throw new IllegalStateException()
      }
      case and: Typed.AND => (aux(and.t1), aux(and.t2)).mapN {
        case (first: Block, second: Block) => AND(first, second)
        case _ => throw new IllegalStateException()
      }
      case or: Typed.OR => (aux(or.t1), aux(or.t2)).mapN {
        case (first: Block, second: Block) => OR(first, second)
        case _ => throw new IllegalStateException()
      }
      case eq: Typed.EQ => (aux(eq.t1), aux(eq.t2)).mapN {
        case (first: Block, second: Block) => EQ(first, second)
        case _ => throw new IllegalStateException()
      }
      case gt: Typed.GT => (aux(gt.t1), aux(gt.t2)).mapN {
        case (first: Block, second: Block) => GT(first, second)
        case _ => throw new IllegalStateException()
      }
      case ge: Typed.GE => (aux(ge.t1), aux(ge.t2)).mapN {
        case (first: Block, second: Block) => GE(first, second)
        case _ => throw new IllegalStateException()
      }
      case sigVerify: Typed.SIG_VERIFY => (aux(sigVerify.message), aux(sigVerify.signature), aux(sigVerify.publicKey)).mapN {
        case (message: Block, signature: Block, publicKey: Block) =>
          SIG_VERIFY(message = message, signature = signature, publicKey = publicKey)
        case _ => throw new IllegalStateException()
      }
      case isDefined: Typed.IS_DEFINED => aux(isDefined.t).map {
        case (t: Block) => IS_DEFINED(t = t)
        case _ => throw new IllegalStateException()
      }
      case let: Typed.LET => aux(let.value).map {
        case (value: Block) => LET(name = let.name, value = value)
        case _ => throw new IllegalStateException()
      }
      case block: Typed.Block => aux(block.t).flatMap { t =>
        val x = Block(let = None, t = t, exprType = None)
        block.let match {
          case None => Coeval(x)
          case Some(let) => aux(let).map {
            case let: LET => x.copy(let = Some(let))
            case _ => throw new IllegalStateException()
          }
        }
      }
      case ifExpr: Typed.IF => (aux(ifExpr.cond), aux(ifExpr.ifTrue), aux(ifExpr.ifFalse)).mapN {
        case (cond: Block, ifTrue: Block, ifFalse: Block) =>
          IF(cond = cond, ifTrue = ifTrue, ifFalse = ifFalse, exprType = None)
        case _ => throw new IllegalStateException()
      }
      case ref: Typed.REF => Coeval(REF(key = ref.key, exprType = None))
      case get: Typed.GET => aux(get.t).map {
        case (t: Block) => GET(t = t, exprType = None)
        case _ => throw new IllegalStateException()
      }
      case some: Typed.SOME => aux(some.t).map {
        case (t: Block) => SOME(t = t, exprType = None)
        case _ => throw new IllegalStateException()
      }
    })

    aux(expr)()
  }
}
