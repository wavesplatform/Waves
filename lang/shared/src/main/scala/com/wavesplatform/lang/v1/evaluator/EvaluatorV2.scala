package com.wavesplatform.lang.v1.evaluator

import cats.Id
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, NativeFunction, UserFunction}
import com.wavesplatform.lang.v1.traits.Environment

import scala.annotation.tailrec

class EvaluatorV2(
  val ctx: EvaluationContext[Environment, Id],
  val stdLibVersion: StdLibVersion
) {

  def apply(expr: EXPR, limit: Int): (EXPR, Int) = {
    var ref = expr
    val unused = root(expr, ref = _, limit, Nil)
    (ref, unused)
  }

  private def root(expr: EXPR, update: EXPR => Unit, limit: Int, parentBlocks: List[BLOCK_DEF]): Int = {
    expr match {
      case b: BLOCK_DEF =>
        root(
          expr = b.body,
          update = {
            case ev: EVALUATED => update(ev)
            case nonEvaluated  => b.body = nonEvaluated
          },
          limit = limit,
          parentBlocks = b :: parentBlocks
        )
      case g: GETTER =>
        val unused = root(
          expr = g.expr,
          update = g.expr = _,
          limit = limit,
          parentBlocks = parentBlocks
        )
        g.expr match {
          case co: CaseObj if unused > 0 =>
            update(co.fields(g.field))
            unused - 1
          case _: CaseObj =>
            unused
          case ev: EVALUATED =>
            throw new IllegalArgumentException(s"GETTER of non-case-object $ev")
          case _ =>
            unused
        }
      case i: IF =>
        val unused = root(
          expr = i.cond,
          update = i.cond = _,
          limit = limit,
          parentBlocks = parentBlocks
        )
        if (unused < 0) throw new Error("Unused < 0")
        i.cond match {
          case TRUE | FALSE if unused == 0 =>
            unused
          case TRUE if unused > 0 =>
            update(i.ifTrue)
            root(
              expr = i.ifTrue,
              update = update,
              limit = unused - 1,
              parentBlocks = parentBlocks
            )
          case FALSE if unused > 0 =>
            update(i.ifFalse)
            root(
              expr = i.ifFalse,
              update = update,
              limit = unused - 1,
              parentBlocks = parentBlocks
            )
          case e: EVALUATED => throw new IllegalArgumentException("Non-boolean result in cond")
          case _ => unused
        }

      case REF(key) =>
        ctx.letDefs.get(key)
          .map { v =>
            val globalValue = v.value.value.explicitGet()
            update(globalValue)
            limit - 1
          }
          .getOrElse(visitRef(key, update, limit, parentBlocks))
      case fc: FUNCTION_CALL =>
        val unusedArgsEval = fc.args.indices.foldLeft(limit) {
          case (unused, argIndex) =>
            if (unused < 0) throw new Error("Unused < 0")
            else if (unused == 0)
              unused
            else
              root(
                expr = fc.args(argIndex),
                update = argValue => fc.args = fc.args.updated(argIndex, argValue),
                limit = unused,
                parentBlocks
              )
        }
        if (fc.args.forall(_.isInstanceOf[EVALUATED])) {
          fc.function match {
            case FunctionHeader.Native(_) =>
              val NativeFunction(_, costByVersion, _, ev, _) = ctx.functions(fc.function).asInstanceOf[NativeFunction[Environment]]
              val cost = costByVersion(stdLibVersion).toInt
              if (unusedArgsEval < cost) {
                unusedArgsEval
              } else {
                update(ev[Id]((ctx.environment, fc.args.asInstanceOf[List[EVALUATED]])).explicitGet())
                unusedArgsEval - cost
              }
            case FunctionHeader.User(_, name) =>
              if (unusedArgsEval > 0)
                ctx.functions.get(fc.function)
                  .map(_.asInstanceOf[UserFunction[Environment]])
                  .map(f => FUNC(f.name, f.args.toList, f.ev[Id](ctx.environment)))
                  .orElse(findUserFunction(name, parentBlocks))
                  .map { signature =>
                    val argsWithExpr =
                      (signature.args zip fc.args)
                        .foldRight(signature.body.deepCopy) {
                          case ((argName, argValue), argsWithExpr) =>
                            BLOCK(LET(argName, argValue), argsWithExpr)
                        }
                    update(argsWithExpr)
                    root(argsWithExpr, update, unusedArgsEval, parentBlocks)
                  }
                  .getOrElse {
                    val objectType = ctx.typeDefs(name).asInstanceOf[CASETYPEREF]  // todo handle absence
                    val fields = objectType.fields.map(_._1) zip fc.args.asInstanceOf[List[EVALUATED]]
                    root(CaseObj(objectType, fields.toMap), update, unusedArgsEval, parentBlocks)
                  }
              else
                unusedArgsEval
          }
        } else {
          unusedArgsEval
        }
      case evaluated: EVALUATED =>
        update(evaluated)
        limit
    }
  }

  @tailrec
  private def visitRef(key: String, update: EVALUATED => Unit, limit: Int, parentBlocks: List[BLOCK_DEF]): Int =
    parentBlocks match {
      case LET_BLOCK(l @ LET(`key`, _), _) :: nextParentBlocks => evaluateRef(update, limit, l, nextParentBlocks)
      case BLOCK(l @ LET(`key`, _), _) :: nextParentBlocks     => evaluateRef(update, limit, l, nextParentBlocks)
      case _ :: nextParentBlocks                               => visitRef(key, update, limit, nextParentBlocks)
      case Nil                                                 => throw new NoSuchElementException("")
    }

  private def evaluateRef(
    update: EVALUATED => Unit,
    limit: Int,
    let: LET,
    nextParentBlocks: List[BLOCK_DEF]
  ) = {
      val unused = root(
        expr = let.value,
        update = let.value = _,
        limit = limit,
        parentBlocks = nextParentBlocks
      )
      if (unused < 0) throw new Error("Unused < 0")
      let.value match {
        case ev: EVALUATED if unused > 0 =>
          update(ev)
          unused - 1
        case _ =>
          unused
      }
  }

  @tailrec
  private def findUserFunction(name: String, parentBlocks: List[BLOCK_DEF]): Option[FUNC] =
    parentBlocks match {
      case (l: LET_BLOCK) :: xs                        => findUserFunction(name, xs)
      case (b @ BLOCK(f @ FUNC(`name`, _, _), _)) :: _ => Some(f)
      case _ :: xs                                     => findUserFunction(name, xs)
      case Nil                                         => None
    }
}