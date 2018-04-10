package com.wavesplatform.lang.typechecker

import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang.TypeChecker
import com.wavesplatform.lang.TypeChecker.TypeCheckerContext
import com.wavesplatform.lang.ctx.impl.PureContext
import com.wavesplatform.lang.testing.ScriptGen
import monix.eval.Coeval
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector

class ErasureTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {
  def treeTypeErasureTests(exprs: (String, Typed.EXPR)*): Unit = exprs.foreach {
    case (exprName, expected) =>
      property(exprName) {
        val erased = erase(expected)
        TypeChecker(TypeCheckerContext.fromContext(PureContext.instance), erased) shouldBe Right(expected)
      }
  }

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
