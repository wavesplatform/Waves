/*
package com.wavesplatform.lang.v1.evaluator

import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.v1.compiler.Decompiler
import com.wavesplatform.lang.v1.{BaseGlobal, FunctionHeader}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.BaseFunction
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.traits.Environment
import jdk.nashorn.internal.objects.Global

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
  case class TreeUpdated(consumed:Int)           extends VisitResult
  case object Nop                   extends VisitResult
  case class NewValue(e: EVALUATED) extends VisitResult

  def visitRef(alreadyConsumed:Int, ctx: Context, key: String, parentRef: List[EXPR]): VisitResult =
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

      case BLOCK(l @ LET(`key`, value), _) :: nextParentRef =>
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

  def visitUserFunc(ctx: Context, name: String, parentRef: List[EXPR]): VisitResult =
    parentRef match {
      case BLOCK(l @ FUNC(`name`, _, body), _) :: nextParentRef =>
        body match {
          case evaluated: EVALUATED =>
            NewValue(evaluated)
          case _ =>
            visit(ctx, body, nextParentRef) match {
              case NewValue(newValue) =>
                l.body = newValue
                TreeUpdated
              case r => r
            }

        }
      case _ :: next => visitUserFunc(ctx, name, next)
      case Nil       => ???
    }

  def visit(ctx: Context, e: EXPR, parentRefs: List[EXPR]): VisitResult = {
//    println(s"Visiting $e")
    e match {
      case lb @ LET_BLOCK(let, body) => visit(ctx, body, lb +: parentRefs)
      case lb @ BLOCK(func, body)    => visit(ctx, body, lb +: parentRefs)
      case r @ REF(key)              => visitRef(ctx, key, parentRefs)

      case f @ FUNCTION_CALL(FunctionHeader.Native(_), args) => {
        if (!args.forall(_.isInstanceOf[EVALUATED])) {
          val visitResultArg0 = visit(ctx, args(0), f +: parentRefs) match {
            case NewValue(newValue) =>
              f.args = newValue +: f.args.tail
              TreeUpdated()
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

      case f @ FUNCTION_CALL(FunctionHeader.User(_, name), _) => visitUserFunc(ctx, name, parentRefs)


      case _: EVALUATED => Nop
    }
  }
  def buildSum(a: EXPR, b: EXPR) = FUNCTION_CALL(FunctionHeader.Native(1), List(a, b))

/*  val expr = buildSum(
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
  println(expr2)*/

  val expr =
    BLOCK(
      LET("a", FUNCTION_CALL(FunctionHeader.Native(FunctionIds.SUM_LONG), List(CONST_LONG(1), CONST_LONG(1)))),
      BLOCK(
        LET("b", REF("a")),
        BLOCK(
          FUNC("g", Nil, BLOCK(
            LET("a", FUNCTION_CALL(
              FunctionHeader.Native(FunctionIds.SUM_LONG),
              List(
                FUNCTION_CALL(
                  FunctionHeader.Native(FunctionIds.SUM_LONG),
                  List(CONST_LONG(2), CONST_LONG(2))
                ),
                CONST_LONG(2)
              )
            )),
            BLOCK(
              LET("c", REF("a")),
              FUNCTION_CALL(
                FunctionHeader.Native(FunctionIds.SUM_LONG),
                List(REF("c"),
                  FUNCTION_CALL(
                    FunctionHeader.Native(FunctionIds.SUM_LONG),
                    List(REF("b"), REF("a"))
                  )
                )
              )
            )
          )),
          FUNCTION_CALL(
            FunctionHeader.Native(FunctionIds.SUM_LONG),
            List(FUNCTION_CALL(FunctionHeader.User("g"), Nil), REF("a"))
          )
        )
      )
    )
  /*
                              # Complexity  Value
       let a = 1 + 1          # 1 (once)    2
       let b = a              # 1 (once)    2
                              #
       func g() = {           #
         let a = 2 + 2 + 2    # 2 (once)    6
         let c = a            # 1 (once)    6
         c + b + a            # 5           14
       }

       g() + a                # 7           16
                              # Total: 12   Result: 16
  */

  import cats.implicits._
  private val Global: BaseGlobal = com.wavesplatform.lang.Global // Hack for IDEA

  private val version = V4
  private val ctx =
    PureContext.build(Global, version).withEnvironment[Environment] |+|
      WavesContext.build(DirectiveSet.contractDirectiveSet)


  (1 to 20) foreach { i =>
    println(visit(Context(Map.empty), expr, List.empty))
    println(Decompiler(expr, ctx.decompilerContext))
    println()
  }

}
*/
