package com.wavesplatform.lang.v1.evaluator

import cats.Id
import cats.implicits._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, LoggedEvaluationContext, NativeFunction, UserFunction}
import com.wavesplatform.lang.v1.traits.Environment
import monix.eval.Coeval

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Try

class EvaluatorV2(
    val ctx: LoggedEvaluationContext[Environment, Id],
    val stdLibVersion: StdLibVersion
) {

  def apply(expr: EXPR, limit: Int, evaluateAll: Boolean = true): (EXPR, Int) = {
    var ref    = expr.deepCopy.value
    val unused = root(ref, v => Coeval.delay { ref = v }, limit, Nil, evaluateAll).value()
    (ref, unused)
  }

  private def root(expr: EXPR, _update: EXPR => Coeval[Unit], limit: Int, parentBlocks: List[BLOCK_DEF], evaluateAll: Boolean): Coeval[Int] = {
    val update =
      if (evaluateAll || (expr.isInstanceOf[FUNCTION_CALL] && expr.asInstanceOf[FUNCTION_CALL].function.isExternal))
        _update
      else (_: EXPR) => Coeval.now(())
    expr match {
      case b: BLOCK_DEF =>
        Coeval.defer(
          root(
            expr = b.body,
            _update = {
              case ev: EVALUATED => Coeval.defer(update(ev))
              case nonEvaluated  => Coeval.delay(b.body = nonEvaluated)
            },
            limit = limit,
            parentBlocks = b :: parentBlocks,
            evaluateAll
          )
        )
      case g: GETTER =>
        Coeval.defer(
          root(
            expr = g.expr,
            _update = v => Coeval.delay(g.expr = v),
            limit = limit,
            parentBlocks = parentBlocks,
            evaluateAll
          ).flatMap { unused =>
            g.expr match {
              case co: CaseObj if unused > 0 =>
                update(co.fields(g.field)).map(_ => unused - 1)
              case _: CaseObj =>
                Coeval.now(unused)
              case ev: EVALUATED =>
                throw new IllegalArgumentException(s"GETTER of non-case-object $ev")
              case _ =>
                Coeval.now(unused)
            }
          }
        )
      case i: IF =>
        Coeval.defer(
          root(
            expr = i.cond,
            _update = (v: EXPR) => Coeval.delay(i.cond = v),
            limit = limit,
            parentBlocks = parentBlocks,
            evaluateAll
          ).flatMap { unused =>
            i.cond match {
              case TRUE | FALSE if unused <= 0 =>
                Coeval.now(unused)
              case TRUE if unused > 0 =>
                update(i.ifTrue).flatMap(
                  _ =>
                    root(
                      expr = i.ifTrue,
                      _update = update,
                      limit = unused - 1,
                      parentBlocks = parentBlocks,
                      evaluateAll
                    )
                )
              case FALSE if unused > 0 =>
                update(i.ifFalse).flatMap(
                  _ =>
                    root(
                      expr = i.ifFalse,
                      _update = update,
                      limit = unused - 1,
                      parentBlocks = parentBlocks,
                      evaluateAll
                    )
                )
              case _ if !evaluateAll && unused > 0 =>
                root(
                  expr = i.ifTrue,
                  _update = (v: EXPR) => Coeval.delay(i.ifTrue = v),
                  limit = unused,
                  parentBlocks = parentBlocks,
                  evaluateAll
                ).flatMap(
                    unusedAfterIfTrue =>
                      if (unusedAfterIfTrue > 0)
                        root(
                          expr = i.ifFalse,
                          _update = (v: EXPR) => Coeval.delay(i.ifFalse = v),
                          limit = unusedAfterIfTrue,
                          parentBlocks = parentBlocks,
                          evaluateAll
                        )
                      else
                        Coeval.now(unusedAfterIfTrue)
                  )

              case _: EVALUATED => throw new IllegalArgumentException("Non-boolean result in cond")
              case _            => Coeval.now(unused)
            }
          }
        )

      case REF(key) =>
        Coeval.defer {
          visitRef(key, update, limit, parentBlocks, evaluateAll)
            .orElse(if (evaluateAll) findGlobalVar(key, update, limit) else Some(Coeval.now(limit)))
            .getOrElse(throw new NoSuchElementException(s"A definition of '$key' not found"))
        }

      case fc: FUNCTION_CALL =>
        val forceEvaluateArgs = evaluateAll || fc.function.isExternal
        val evaluatedArgs =
          Coeval.defer {
            fc.args.indices
              .to(LazyList)
              .foldM(limit) {
                case (unused, argIndex) =>
                  if (unused <= 0)
                    Coeval.now(unused)
                  else
                    root(
                      expr = fc.args(argIndex),
                      _update = argValue => Coeval.delay(fc.args = fc.args.updated(argIndex, argValue)),
                      limit = unused,
                      parentBlocks,
                      forceEvaluateArgs
                    )
              }
          }
        evaluatedArgs
          .flatMap { unusedArgsComplexity =>
            if (fc.args.forall(_.isInstanceOf[EVALUATED])) {
              fc.function match {
                case FunctionHeader.Native(_) if (fc.function.isExternal || evaluateAll) =>
                  val NativeFunction(_, costByVersion, _, ev, _) =
                    ctx.ec.functions
                      .getOrElse(fc.function, throw new RuntimeException(s"function '${fc.function}' not found"))
                      .asInstanceOf[NativeFunction[Environment]]
                  val cost = costByVersion(stdLibVersion).toInt
                  if (unusedArgsComplexity < cost)
                    Coeval.now(unusedArgsComplexity)
                  else {
                    update(ev[Id]((ctx.ec.environment, fc.args.asInstanceOf[List[EVALUATED]])).explicitGet())
                      .map(_ => unusedArgsComplexity - cost)
                  }

                case FunctionHeader.User(_, name) =>
                  ctx.ec.functions
                    .get(fc.function)
                    .map(_.asInstanceOf[UserFunction[Environment]])
                    .map(f => FUNC(f.name, f.args.toList, f.ev[Id](ctx.ec.environment)))
                    .orElse(findUserFunction(name, parentBlocks))
                    .map { signature =>
                      val argsWithExpr =
                        (signature.args zip fc.args)
                          .foldRight(signature.body.deepCopy.value) {
                            case ((argName, argValue), argsWithExpr) =>
                              BLOCK(LET(argName, argValue), argsWithExpr)
                          }
                      _update(argsWithExpr).flatMap(
                        _ =>
                          if (unusedArgsComplexity > 0)
                            root(argsWithExpr, _update, unusedArgsComplexity, parentBlocks, evaluateAll)
                          else
                            Coeval.now(unusedArgsComplexity)
                      )
                    }
                    .getOrElse {
                      if (unusedArgsComplexity > 0) {
                        val caseType =
                          ctx.ec.typeDefs.get(name) match {
                            case Some(caseType: CASETYPEREF) => Coeval.now(caseType)
                            case _                           => Coeval.raiseError(new NoSuchElementException(s"Function or type '$name' not found"))
                          }
                        caseType.flatMap { objectType =>
                          val fields = objectType.fields.map(_._1) zip fc.args.asInstanceOf[List[EVALUATED]]
                          root(CaseObj(objectType, fields.toMap), update, unusedArgsComplexity, parentBlocks, evaluateAll)
                        }
                      } else
                        Coeval.now(unusedArgsComplexity)
                    }
                case _ =>
                  Coeval.now(limit)

              }
            } else {
              fc.function match {
                case FunctionHeader.Native(name) =>
                  Coeval.now(unusedArgsComplexity)
                case FunctionHeader.User(_, name) =>
                  ctx.ec.functions
                    .get(fc.function)
                    .map(_.asInstanceOf[UserFunction[Environment]])
                    .map(f => FUNC(f.name, f.args.toList, f.ev[Id](ctx.ec.environment)))
                    .orElse(findUserFunction(name, parentBlocks))
                    .map { signature =>
                      val argsWithExpr =
                        (signature.args zip fc.args)
                          .foldRight(signature.body.deepCopy.value) {
                            case ((argName, argValue), argsWithExpr) =>
                              BLOCK(LET(argName, argValue), argsWithExpr)
                          }
                      _update(argsWithExpr).flatMap(
                        _ =>
                          if (unusedArgsComplexity > 0)
                            root(argsWithExpr, _update, unusedArgsComplexity, parentBlocks, evaluateAll)
                          else
                            Coeval.now(unusedArgsComplexity)
                      )
                    }
                    .getOrElse {
                      Coeval.now(unusedArgsComplexity)
                    }
              }
            }
          }
      case evaluated: EVALUATED =>
        update(evaluated).map(_ => limit)

      case f: FAILED_EXPR => throw new Error(s"Unexpected $f")
    }
  }

  @tailrec
  private def visitRef(
      key: String,
      update: EVALUATED => Coeval[Unit],
      limit: Int,
      parentBlocks: List[BLOCK_DEF],
      evaluateAll: Boolean
  ): Option[Coeval[Int]] =
    parentBlocks match {
      case LET_BLOCK(l @ LET(`key`, _, _), _) :: nextParentBlocks => Some(evaluateRef(update, limit, l, nextParentBlocks, evaluateAll))
      case BLOCK(l @ LET(`key`, _, _), _) :: nextParentBlocks     => Some(evaluateRef(update, limit, l, nextParentBlocks, evaluateAll))
      case _ :: nextParentBlocks                                  => visitRef(key, update, limit, nextParentBlocks, evaluateAll)
      case Nil                                                    => None
    }

  private def evaluateRef(
      update: EVALUATED => Coeval[Unit],
      limit: Int,
      let: LET,
      nextParentBlocks: List[BLOCK_DEF],
      evaluateAll: Boolean
  ): Coeval[Int] = {
    val wasLogged = let.value match {
      case evaluated: EVALUATED if evaluated.wasLogged => true
      case _                                           => false
    }
    if (!evaluateAll && let.checked) {
      Coeval.now(limit)
    } else
      root(
        expr = let.value,
        _update = v =>
          Coeval
            .delay(let.value = v)
            .map(
              _ =>
                let.value match {
                  case evaluated: EVALUATED if !wasLogged =>
                    ctx.l(let.name)(Right(evaluated))
                    evaluated.wasLogged = true
                  case _ => ()
                }
            ),
        limit = limit,
        parentBlocks = nextParentBlocks,
        evaluateAll
      ).flatMap { unused =>
          let.checked = true
          if (unused < 0) throw new Error("Unused < 0")
          else
            let.value match {
              case ev: EVALUATED if unused > 0 =>
                if (evaluateAll)
                  update(ev).map(_ => unused - 1)
                else
                  Coeval.now(unused)
              case _ =>
                Coeval.now(unused)
            }
        }
        .onErrorHandle { e =>
          val error = if (e.getMessage != null) e.getMessage else e.toString
          if (!wasLogged) ctx.l(let.name)(Left(error))
          throw e
        }
  }

  private def findGlobalVar(key: String, update: EVALUATED => Coeval[Unit], limit: Int): Option[Coeval[Int]] =
    ctx.ec.letDefs
      .get(key)
      .map { v =>
        val globalValue = v.value.value.explicitGet()
        update(globalValue).map(_ => limit - 1)
      }

  @tailrec
  private def findUserFunction(name: String, parentBlocks: List[BLOCK_DEF]): Option[FUNC] =
    parentBlocks match {
      case (_: LET_BLOCK) :: xs                  => findUserFunction(name, xs)
      case BLOCK(f @ FUNC(`name`, _, _), _) :: _ => Some(f)
      case _ :: xs                               => findUserFunction(name, xs)
      case Nil                                   => None
    }
}

object EvaluatorV2 {
  def applyLimited(
      expr: EXPR,
      limit: Int,
      ctx: EvaluationContext[Environment, Id],
      stdLibVersion: StdLibVersion
  ): Either[(ExecutionError, Log[Id]), (EXPR, Int, Log[Id])] = {
    val log       = ListBuffer[LogItem[Id]]()
    val loggedCtx = LoggedEvaluationContext[Environment, Id](name => value => log.append((name, value)), ctx)
    val evaluator = new EvaluatorV2(loggedCtx, stdLibVersion)
    Try(evaluator(expr, limit)).toEither
      .bimap(
        err => (err.getMessage, log.toList),
        { case (expr, unused) => (expr, unused, log.toList) }
      )
  }

  def applyOrDefault(
      ctx: EvaluationContext[Environment, Id],
      expr: EXPR,
      stdLibVersion: StdLibVersion,
      complexityLimit: Int,
      default: EVALUATED
  ): Either[(ExecutionError, Log[Id]), (EVALUATED, Log[Id])] =
    EvaluatorV2
      .applyLimited(expr, complexityLimit, ctx, stdLibVersion)
      .flatMap {
        case (expr, _, log) =>
          expr match {
            case evaluated: EVALUATED => Right((evaluated, log))
            case _                    => Right((default, log))
          }
      }

  def applyCompleted(
      ctx: EvaluationContext[Environment, Id],
      expr: EXPR,
      stdLibVersion: StdLibVersion
  ): Either[(ExecutionError, Log[Id]), (EVALUATED, Log[Id])] =
    EvaluatorV2
      .applyLimited(expr, Int.MaxValue, ctx, stdLibVersion)
      .flatMap {
        case (expr, _, log) =>
          expr match {
            case evaluated: EVALUATED => Right((evaluated, log))
            case expr: EXPR           => Left((s"Unexpected incomplete evaluation result $expr", log))
          }
      }
}
