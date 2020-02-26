package com.wavesplatform.lang.v1.evaluator

import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.BaseFunction
import com.wavesplatform.lang.v1.traits.Environment

object PartialEvaluator extends App {

  // Definition of Done: traverse a tree, update 1 node to evaluated
//
  case class Context(functions: Map[FunctionHeader.Native, BaseFunction[Environment]])
//
//  def traverse(b: LET_BLOCK): Unit =
//    visit(Context(Map.empty), b, List.empty)
//
//
//  def visit(ctx: Context, e: EXPR, parentRef: List[EXPR]): Boolean = {
//    println(s"Visiting $e, parent:$parentRef")
//    e match {
////      case GETTER(expr, field)           => visit(ctx, expr, e +: parentRef)
////      case LET_BLOCK(let, body)          => visit(ctx, body, e +: parentRef)
////      case BLOCK(dec, body)              => visit(ctx, body, e +: parentRef)
////      case IF(cond, ifTrue, ifFalse)     => visit(ctx, cond, e +: parentRef)
////      case REF(key)                      => visitRef(ctx, key, parentRef)
//      case f @ FUNCTION_CALL(_, args) => {
//        if (args.forall(_.isInstanceOf[EVALUATED])) {
//          lazy val visit0 = visit(ctx, args(0), f +: parentRef)
//          lazy val visit1 = visit(ctx, args(1), f +: parentRef)
//          visit0 || visit1
//        } else {
//          val newNode = CONST_LONG(args(0).asInstanceOf[CONST_LONG].t + args(1).asInstanceOf[CONST_LONG].t)
//          true
//        }
//      }
//      case evaluated: EVALUATED => false
//    }
//  }
//
//  private val initialExpr: LET_BLOCK = LET_BLOCK(
//    LET("a", CONST_LONG(1)),
//    LET_BLOCK(
//      LET("b", CONST_LONG(2)),
//      LET_BLOCK(
//        LET("c", CONST_LONG(3)),
//        REF("b")
//      )
//    )
//  )

//  case class VisitResult(a: Boolean, newValue: Option[EVALUATED])

  sealed trait VisitResult
  case object TreeUpdated           extends VisitResult
  case object Nop                   extends VisitResult
  case class NewValue(e: EVALUATED) extends VisitResult

  def visitRef(ctx: Context, key: String, parentRef: List[EXPR]): VisitResult =
    parentRef match {
      case LET_BLOCK(l @ LET(`key`, value), _) :: nextParentRef =>
        value match {
          case evaluated: EVALUATED =>
            NewValue(evaluated)
          case _ =>
            visit(ctx, value, nextParentRef) match {
              case NewValue(newValue) =>
                l.value = newValue
                TreeUpdated
              case r => r
            }

        }
      case _ :: next => visitRef(ctx, key, next)
      case Nil       => ???
    }

  def visit(ctx: Context, e: EXPR, parentRefs: List[EXPR]): VisitResult = {
//    println(s"Visiting $e")
    e match {
      case lb @ LET_BLOCK(let, body) => visit(ctx, body, lb +: parentRefs)
      case r @ REF(key)              => visitRef(ctx, key, parentRefs)

      case f @ FUNCTION_CALL(_, args) => {
        if (!args.forall(_.isInstanceOf[EVALUATED])) {
          val visitResultArg0 = visit(ctx, args(0), f +: parentRefs) match {
            case NewValue(newValue) =>
              f.args = newValue +: f.args.tail
              TreeUpdated
            case r => r
          }
          visitResultArg0 match {
            case TreeUpdated => TreeUpdated
            case _ =>
              visit(ctx, args(1), f +: parentRefs) match {
                case NewValue(newValue) =>
                  f.args = f.args.init :+ newValue
                  TreeUpdated
                case r => r
              }

          }

        } else {
          NewValue(CONST_LONG(args(0).asInstanceOf[CONST_LONG].t + args(1).asInstanceOf[CONST_LONG].t))
        }
      }
      case _: EVALUATED => Nop
    }
  }
  def buildSum(a: EXPR, b: EXPR) = FUNCTION_CALL(FunctionHeader.Native(1), List(a, b))

  val expr = buildSum(
    buildSum(
      buildSum(CONST_LONG(1), CONST_LONG(2)),
      buildSum(CONST_LONG(3), CONST_LONG(4))
    ),
    CONST_LONG(5)
  )

  println(expr)
  println(visit(Context(Map.empty), expr, List.empty))
  println(expr)

  println(visit(Context(Map.empty), expr, List.empty))
  println(expr)

  println(visit(Context(Map.empty), expr, List.empty))
  println(expr)

  println(visit(Context(Map.empty), expr, List.empty))
  println(expr)

  println(visit(Context(Map.empty), expr, List.empty))
  println(expr)

  val expr2 = LET_BLOCK(
    LET("a", buildSum(CONST_LONG(1), CONST_LONG(2))),
    buildSum(
      buildSum(
        buildSum(CONST_LONG(3), CONST_LONG(4)),
        REF("a")
      ),
      CONST_LONG(5)
    )
  )

  println(expr2)
  println(visit(Context(Map.empty), expr2, List.empty))
  println(expr2)

  println(visit(Context(Map.empty), expr2, List.empty))
  println(expr2)

  println(visit(Context(Map.empty), expr2, List.empty))
  println(expr2)

  println(visit(Context(Map.empty), expr2, List.empty))
  println(expr2)

  println(visit(Context(Map.empty), expr2, List.empty))
  println(expr2)
}
