package com.wavesplatform.lang.v1.evaluator

import cats.implicits._
import cats.{Eval, Id}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ctx.{LoggedEvaluationContext, NativeFunction, UserFunction}
import com.wavesplatform.lang.v1.traits.Environment

import scala.annotation.tailrec

class EvaluatorV2(
    val ctx: LoggedEvaluationContext[Environment, Id],
    val stdLibVersion: StdLibVersion
) {

  def apply(expr: EXPR, limit: Int): (EXPR, Int) = {
    var ref    = expr.deepCopy
    val unused = root(ref, v => Eval.later { ref = v }, limit, Nil).value
    (ref, unused)
  }

  private def root(expr: EXPR, update: EXPR => Eval[Unit], limit: Int, parentBlocks: List[BLOCK_DEF]): Eval[Int] =
    expr match {
      case b: BLOCK_DEF =>
        root(
          expr = b.body,
          update = {
            case ev: EVALUATED => Eval.defer(update(ev))
            case nonEvaluated  => Eval.later(b.body = nonEvaluated)
          },
          limit = limit,
          parentBlocks = b :: parentBlocks
        )
      case g: GETTER =>
        root(
          expr = g.expr,
          update = v => Eval.later(g.expr = v),
          limit = limit,
          parentBlocks = parentBlocks
        ).flatMap { unused =>
          g.expr match {
            case co: CaseObj if unused > 0 =>
              update(co.fields(g.field)).map(_ => unused - 1)
            case _: CaseObj =>
              Eval.now(unused)
            case ev: EVALUATED =>
              throw new IllegalArgumentException(s"GETTER of non-case-object $ev")
            case _ =>
              Eval.now(unused)
          }
        }
      case i: IF =>
        root(
          expr = i.cond,
          update = v => Eval.later(i.cond = v),
          limit = limit,
          parentBlocks = parentBlocks
        ).flatMap { unused =>
          if (unused < 0) throw new Error("Unused < 0")
          i.cond match {
            case TRUE | FALSE if unused == 0 =>
              Eval.now(unused)
            case TRUE if unused > 0 =>
              update(i.ifTrue).flatMap(
                _ =>
                  root(
                    expr = i.ifTrue,
                    update = update,
                    limit = unused - 1,
                    parentBlocks = parentBlocks
                  )
              )
            case FALSE if unused > 0 =>
              update(i.ifFalse).flatMap(
                _ =>
                  root(
                    expr = i.ifFalse,
                    update = update,
                    limit = unused - 1,
                    parentBlocks = parentBlocks
                  )
              )
            case _: EVALUATED => throw new IllegalArgumentException("Non-boolean result in cond")
            case _            => Eval.now(unused)
          }
        }

      case REF(key) =>
        ctx.ec.letDefs
          .get(key)
          .map { v =>
            val globalValue = v.value.value.explicitGet()
            update(globalValue).map(_ => limit - 1)
          }
          .getOrElse(Eval.defer(visitRef(key, update, limit, parentBlocks)))
      case fc: FUNCTION_CALL =>
        fc.args.indices.toStream
          .foldM(limit) {
            case (unused, argIndex) =>
              if (unused < 0) throw new Error("Unused < 0")
              else if (unused == 0)
                Eval.now(unused)
              else
                root(
                  expr = fc.args(argIndex),
                  update = argValue => Eval.later(fc.args = fc.args.updated(argIndex, argValue)),
                  limit = unused,
                  parentBlocks
                )
          }
          .flatMap { unusedArgsEval =>
            if (fc.args.forall(_.isInstanceOf[EVALUATED])) {
              fc.function match {
                case FunctionHeader.Native(_) =>
                  val NativeFunction(_, costByVersion, _, ev, _) = ctx.ec.functions(fc.function).asInstanceOf[NativeFunction[Environment]]
                  val cost                                       = costByVersion(stdLibVersion).toInt
                  if (unusedArgsEval < cost) {
                    Eval.now(unusedArgsEval)
                  } else {
                    update(ev[Id]((ctx.ec.environment, fc.args.asInstanceOf[List[EVALUATED]])).explicitGet())
                      .map(_ => unusedArgsEval - cost)

                  }
                case FunctionHeader.User(_, name) =>
                  if (unusedArgsEval > 0)
                    ctx.ec.functions
                      .get(fc.function)
                      .map(_.asInstanceOf[UserFunction[Environment]])
                      .map(f => FUNC(f.name, f.args.toList, f.ev[Id](ctx.ec.environment)))
                      .orElse(findUserFunction(name, parentBlocks))
                      .map { signature =>
                        val argsWithExpr =
                          (signature.args zip fc.args)
                            .foldRight(signature.body.deepCopy) {
                              case ((argName, argValue), argsWithExpr) =>
                                BLOCK(LET(argName, argValue), argsWithExpr)
                            }
                        update(argsWithExpr).flatMap(_ => root(argsWithExpr, update, unusedArgsEval, parentBlocks))
                      }
                      .getOrElse {
                        val objectType = ctx.ec.typeDefs(name).asInstanceOf[CASETYPEREF] // todo handle absence
                        val fields     = objectType.fields.map(_._1) zip fc.args.asInstanceOf[List[EVALUATED]]
                        root(CaseObj(objectType, fields.toMap), update, unusedArgsEval, parentBlocks)
                      } else
                    Eval.now(unusedArgsEval)
              }
            } else {
              Eval.now(unusedArgsEval)
            }
          }
      case evaluated: EVALUATED =>
        update(evaluated).map(_ => limit)

      case f: FAILED_EXPR => throw new Error(s"Unexpected $f")
    }

  @tailrec
  private def visitRef(key: String, update: EVALUATED => Eval[Unit], limit: Int, parentBlocks: List[BLOCK_DEF]): Eval[Int] =
    parentBlocks match {
      case LET_BLOCK(l @ LET(`key`, _), _) :: nextParentBlocks => evaluateRef(update, limit, l, nextParentBlocks)
      case BLOCK(l @ LET(`key`, _), _) :: nextParentBlocks     => evaluateRef(update, limit, l, nextParentBlocks)
      case _ :: nextParentBlocks                               => visitRef(key, update, limit, nextParentBlocks)
      case Nil                                                 => throw new NoSuchElementException("")
    }

  private def evaluateRef(
      update: EVALUATED => Eval[Unit],
      limit: Int,
      let: LET,
      nextParentBlocks: List[BLOCK_DEF]
  ): Eval[Int] =
    root(
      expr = let.value,
      update = v => Eval.later(let.value = v),
      limit = limit,
      parentBlocks = nextParentBlocks
    ).flatMap { unused =>
      if (unused < 0) throw new Error("Unused < 0")
      else
        let.value match {
          case ev: EVALUATED if unused > 0 =>
            Eval.later(ctx.l(let.name)(Right(ev))).flatMap(_ => update(ev).map(_ => unused - 1))
          case ev: EVALUATED =>
            Eval.later(ctx.l(let.name)(Right(ev))).map(_ => unused)
          case _ =>
            Eval.now(unused)
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
