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

  treeTypeErasureTest(
    "CONST_INT" -> CONST_INT(0),
    "CONST_BYTEVECTOR" -> CONST_BYTEVECTOR(ByteVector(1, 2, 3)),
    "TRUE" -> TRUE,
    "FALSE" -> FALSE,
    "NONE" -> NONE,
    "SUM" -> SUM(Block(None, CONST_INT(0), Some(INT)), Block(None, CONST_INT(1), Some(INT))),
    "AND" -> AND(Block(None, TRUE, Some(BOOLEAN)), Block(None, FALSE, Some(BOOLEAN))),
    "OR" -> OR(Block(None, TRUE, Some(BOOLEAN)), Block(None, FALSE, Some(BOOLEAN))),
    "EQ" -> EQ(Block(None, CONST_INT(0), Some(INT)), Block(None, CONST_INT(1), Some(INT))),
    "GT" -> GT(Block(None, CONST_INT(0), Some(INT)), Block(None, CONST_INT(1), Some(INT))),
    "GE" -> GE(Block(None, CONST_INT(0), Some(INT)), Block(None, CONST_INT(1), Some(INT))),
    "SIG_VERIFY" -> SIG_VERIFY(
      message = Block(None, CONST_BYTEVECTOR(ByteVector(Base58.decode("333").get)), Some(BYTEVECTOR)),
      signature = Block(None, CONST_BYTEVECTOR(ByteVector(Base58.decode("222").get)), Some(BYTEVECTOR)),
      publicKey = Block(None, CONST_BYTEVECTOR(ByteVector(Base58.decode("111").get)), Some(BYTEVECTOR))
    ),
    "IS_DEFINED" -> IS_DEFINED(Block(None, NONE, Some(OPTION(NOTHING)))),
    "LET" -> LET("x", Block(None, CONST_INT(0), Some(INT))),
    "BLOCK" -> Block(
      let = None,
      t = CONST_INT(0),
      exprType = Some(INT)
    ),
    "IF" -> IF(
      cond = Block(None, TRUE, Some(BOOLEAN)),
      ifTrue = Block(None, SOME(Block(None, TRUE, Some(BOOLEAN)), Some(OPTION(BOOLEAN))), Some(OPTION(BOOLEAN))),
      ifFalse = Block(None, NONE, Some(OPTION(NOTHING))),
      exprType = Some(OPTION(BOOLEAN))
    ),
    "GET(SOME)" -> GET(
      t = Block(
        let = None,
        t = SOME(Block(None, TRUE, Some(BOOLEAN)), Some(OPTION(BOOLEAN))),
        exprType = Some(OPTION(BOOLEAN))
      ),
      exprType = Some(BOOLEAN)
    ),
    "GET(NONE)" -> GET(
      t = Block(
        let = None,
        t = Block(None, NONE, Some(OPTION(NOTHING))),
        exprType = Some(OPTION(NOTHING))
      ),
      exprType = Some(NOTHING)
    ),
    "SOME" -> SOME(Block(None, TRUE, Some(BOOLEAN)), Some(OPTION(BOOLEAN)))
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

  treeTypeTest("REF")(
    predefTypes = Map(pointType.name -> pointType),
    varDefs = Map("p" -> TYPEREF("Point")),
    expr = REF("p"),
    expectedResult = Right(REF("p", Some(TYPEREF("Point"))))
  )

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

  private def treeTypeErasureTest(exprs: (String, Expr)*): Unit = exprs.foreach {
    case (exprName, expected) =>
      property(exprName) {
        val erased = erase(expected)
        TypeChecker(Context.empty, erased) shouldBe Right(expected)
      }
  }

  private def erase(expr: Expr): Expr = {
    import cats.syntax.apply._

    def aux(root: Expr): Coeval[Expr] = Coeval.defer(root match {
      case getter: GETTER => aux(getter.ref).map {
        case (ref: Block) => getter.copy(ref = ref, exprType = None)
        case _ => throw new IllegalStateException()
      }
      case sum: SUM => (aux(sum.i1), aux(sum.i2)).mapN {
        case (first: Block, second: Block) => sum.copy(first, second)
        case _ => throw new IllegalStateException()
      }
      case and: AND => (aux(and.t1), aux(and.t2)).mapN {
        case (first: Block, second: Block) => and.copy(first, second)
        case _ => throw new IllegalStateException()
      }
      case or: OR => (aux(or.t1), aux(or.t2)).mapN {
        case (first: Block, second: Block) => or.copy(first, second)
        case _ => throw new IllegalStateException()
      }
      case eq: EQ => (aux(eq.t1), aux(eq.t2)).mapN {
        case (first: Block, second: Block) => eq.copy(first, second)
        case _ => throw new IllegalStateException()
      }
      case gt: GT => (aux(gt.t1), aux(gt.t2)).mapN {
        case (first: Block, second: Block) => gt.copy(first, second)
        case _ => throw new IllegalStateException()
      }
      case ge: GE => (aux(ge.t1), aux(ge.t2)).mapN {
        case (first: Block, second: Block) => ge.copy(first, second)
        case _ => throw new IllegalStateException()
      }
      case sigVerify: SIG_VERIFY => (aux(sigVerify.message), aux(sigVerify.signature), aux(sigVerify.publicKey)).mapN {
        case (message: Block, signature: Block, publicKey: Block) =>
          sigVerify.copy(message = message, signature = signature, publicKey = publicKey)
        case _ => throw new IllegalStateException()
      }
      case isDefined: IS_DEFINED => aux(isDefined.t).map {
        case (t: Block) => isDefined.copy(t = t)
        case _ => throw new IllegalStateException()
      }
      case let: LET => aux(let.value).map {
        case (value: Block) => let.copy(value = value)
        case _ => throw new IllegalStateException()
      }
      case block: Block => aux(block.t).flatMap { t =>
        val x = block.copy(t = t, exprType = None)
        block.let match {
          case None => Coeval(x)
          case Some(let) => aux(let).map {
            case let: LET => x.copy(let = Some(let))
            case _ => throw new IllegalStateException()
          }
        }
      }
      case ifExpr: IF => (aux(ifExpr.cond), aux(ifExpr.ifTrue), aux(ifExpr.ifFalse)).mapN {
        case (cond: Block, ifTrue: Block, ifFalse: Block) =>
          ifExpr.copy(cond = cond, ifTrue = ifTrue, ifFalse = ifFalse, exprType = None)
        case _ => throw new IllegalStateException()
      }
      case ref: REF => Coeval(ref.copy(exprType = None))
      case get: GET => aux(get.t).map {
        case (t: Block) => get.copy(t = t, exprType = None)
        case _ => throw new IllegalStateException()
      }
      case some: SOME => aux(some.t).map {
        case (t: Block) => some.copy(t = t, exprType = None)
        case _ => throw new IllegalStateException()
      }
      case x => Coeval(x)
    })

    aux(expr)()
  }
}
