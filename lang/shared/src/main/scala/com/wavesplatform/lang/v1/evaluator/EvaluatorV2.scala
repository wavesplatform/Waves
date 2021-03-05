package com.wavesplatform.lang.v1.evaluator

import cats.Id
import cats.implicits._
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2.EvaluationException
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, LoggedEvaluationContext, NativeFunction, UserFunction}
import com.wavesplatform.lang.v1.traits.Environment
import monix.eval.Coeval

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class EvaluatorV2(
    val ctx: LoggedEvaluationContext[Environment, Id],
    val stdLibVersion: StdLibVersion
) {
  def apply(expr: EXPR, limit: Int): (EXPR, Int) =
    applyCoeval(expr, limit).value()

  def applyCoeval(expr: EXPR, limit: Int): Coeval[(EXPR, Int)] = {
    var ref = expr.deepCopy.value
    root(ref, v => Coeval.delay { ref = v }, limit, Nil)
      .map((ref, _))
  }

  private def root(
      expr: EXPR,
      update: EXPR => Coeval[Unit],
      limit: Int,
      parentBlocks: List[BLOCK_DEF]
  ): Coeval[Int] = {
    def evaluateFunctionArgs(fc: FUNCTION_CALL): Coeval[Int] =
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
                  update = argValue => Coeval.delay(fc.args = fc.args.updated(argIndex, argValue)),
                  limit = unused,
                  parentBlocks
                )
          }
      }

    def evaluateNativeFunction(fc: FUNCTION_CALL, limit: Int): Coeval[Int] = {
      val function =
        ctx.ec.functions
          .getOrElse(fc.function, throw EvaluationException(s"function '${fc.function}' not found", limit))
          .asInstanceOf[NativeFunction[Environment]]
      val cost = function.costByLibVersion(stdLibVersion).toInt
      if (limit < cost)
        Coeval.now(limit)
      else {
        function.ev
          .evaluateExtended[Id](ctx.ec.environment, fc.args.asInstanceOf[List[EVALUATED]], limit - cost)
          .flatMap {
            case (result, additionalComplexity) =>
              val totalCost        = cost + additionalComplexity
              val unusedComplexity = limit - totalCost
              result.fold(
                error => throw EvaluationException(error, unusedComplexity),
                evaluated => update(evaluated).map(_ => unusedComplexity)
              )
          }
      }
    }

    def evaluateUserFunction(fc: FUNCTION_CALL, limit: Int, name: String): Option[Coeval[Int]] = {
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
          update(argsWithExpr)
            .flatMap(
              _ => root(argsWithExpr, update, limit, parentBlocks)
            )
        }
    }

    def evaluateConstructor(fc: FUNCTION_CALL, limit: Int, name: String): Coeval[Int] = {
      val caseType =
        ctx.ec.typeDefs.get(name) match {
          case Some(caseType: CASETYPEREF) => Coeval.now(caseType)
          case _                           => Coeval.raiseError(new NoSuchElementException(s"Function or type '$name' not found"))
        }
      caseType.flatMap { objectType =>
        val fields = objectType.fields.map(_._1) zip fc.args.asInstanceOf[List[EVALUATED]]
        root(CaseObj(objectType, fields.toMap), update, limit, parentBlocks)
      }
    }

    expr match {
      case b: BLOCK_DEF =>
        Coeval.defer(
          root(
            expr = b.body,
            update = {
              case ev: EVALUATED => Coeval.defer(update(ev))
              case nonEvaluated  => Coeval.delay(b.body = nonEvaluated)
            },
            limit = limit,
            parentBlocks = b :: parentBlocks
          )
        )
      case g: GETTER =>
        Coeval.defer(
          root(
            expr = g.expr,
            update = v => Coeval.delay(g.expr = v),
            limit = limit,
            parentBlocks = parentBlocks
          ).flatMap { unused =>
            g.expr match {
              case co: CaseObj if unused > 0 =>
                update(co.fields(g.field)).map(_ => unused - 1)
              case _: CaseObj =>
                Coeval.now(unused)
              case ev: EVALUATED =>
                throw EvaluationException(s"GETTER of non-case-object $ev", unused)
              case _ =>
                Coeval.now(unused)
            }
          }
        )
      case i: IF =>
        Coeval.defer(
          root(
            expr = i.cond,
            update = v => Coeval.delay(i.cond = v),
            limit = limit,
            parentBlocks = parentBlocks
          ).flatMap { unused =>
            i.cond match {
              case TRUE | FALSE if unused <= 0 =>
                Coeval.now(unused)
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
              case _: EVALUATED => throw EvaluationException("Non-boolean result in cond", unused)
              case _            => Coeval.now(unused)
            }
          }
        )

      case REF(key) =>
        Coeval.defer {
          visitRef(key, update, limit, parentBlocks)
            .orElse(findGlobalVar(key, update, limit))
            .getOrElse(throw EvaluationException(s"A definition of '$key' not found", limit))
        }

      case fc: FUNCTION_CALL =>
        evaluateFunctionArgs(fc)
          .flatMap { unusedArgsComplexity =>
            val argsEvaluated = fc.args.forall(_.isInstanceOf[EVALUATED])
            if (argsEvaluated && unusedArgsComplexity > 0)
              fc.function match {
                case FunctionHeader.Native(_) =>
                  evaluateNativeFunction(fc, unusedArgsComplexity)
                case FunctionHeader.User(_, name) =>
                  evaluateUserFunction(fc, unusedArgsComplexity, name)
                    .getOrElse(evaluateConstructor(fc, unusedArgsComplexity, name))
              } else
              Coeval.now(unusedArgsComplexity)
          }

      case evaluated: EVALUATED =>
        update(evaluated).map(_ => limit)

      case f: FAILED_EXPR => throw EvaluationException(s"Unexpected $f", limit)
    }
  }

  @tailrec
  private def visitRef(key: String, update: EVALUATED => Coeval[Unit], limit: Int, parentBlocks: List[BLOCK_DEF]): Option[Coeval[Int]] =
    parentBlocks match {
      case LET_BLOCK(l @ LET(`key`, _), _) :: nextParentBlocks => Some(evaluateRef(update, limit, l, nextParentBlocks))
      case BLOCK(l @ LET(`key`, _), _) :: nextParentBlocks     => Some(evaluateRef(update, limit, l, nextParentBlocks))
      case _ :: nextParentBlocks                               => visitRef(key, update, limit, nextParentBlocks)
      case Nil                                                 => None
    }

  private def evaluateRef(
      update: EVALUATED => Coeval[Unit],
      limit: Int,
      let: LET,
      nextParentBlocks: List[BLOCK_DEF]
  ): Coeval[Int] = {
    val wasLogged = let.value match {
      case evaluated: EVALUATED if evaluated.wasLogged => true
      case _                                           => false
    }
    root(
      expr = let.value,
      update = v =>
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
      parentBlocks = nextParentBlocks
    ).flatMap { unused =>
        let.value match {
          case ev: EVALUATED if unused > 0 => update(ev).map(_ => unused - 1)
          case _                           => Coeval.now(unused)
        }
      }
      .onErrorHandle { e =>
        if (!wasLogged) {
          val error = if (e.getMessage != null) e.getMessage else e.toString
          ctx.l(let.name)(Left(error))
        }
        throw if (e.isInstanceOf[EvaluationException])
          e
        else
          EvaluationException(e.getMessage, limit)
      }
  }

  private def findGlobalVar(key: String, update: EVALUATED => Coeval[Unit], limit: Int): Option[Coeval[Int]] =
    ctx.ec.letDefs
      .get(key)
      .map { v =>
        val globalValue = v.value.value.fold(e => throw EvaluationException(e, limit), identity)
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
  ): Either[(ExecutionError, Log[Id]), (EXPR, Int, Log[Id])] =
    applyLimitedCoeval(expr, limit, ctx, stdLibVersion)
      .value()
      .leftMap { case (e, _, unused) => (e, unused) }

  def applyLimitedCoeval(
      expr: EXPR,
      limit: Int,
      ctx: EvaluationContext[Environment, Id],
      stdLibVersion: StdLibVersion
  ): Coeval[Either[(ExecutionError, Int, Log[Id]), (EXPR, Int, Log[Id])]] = {
    val log       = ListBuffer[LogItem[Id]]()
    val loggedCtx = LoggedEvaluationContext[Environment, Id](name => value => log.append((name, value)), ctx)
    val evaluator = new EvaluatorV2(loggedCtx, stdLibVersion)
    evaluator
      .applyCoeval(expr, limit)
      .redeem(
        {
          case e: EvaluationException => Left((e.getMessage, e.unusedComplexity, log.toList))
          case e                      => Left((e.getMessage, limit, log.toList))
        }, { case (expr, unused)      => Right((expr, unused, log.toList)) }
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
      .map {
        case (expr, _, log) =>
          expr match {
            case evaluated: EVALUATED => (evaluated, log)
            case _                    => (default, log)
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

  case class EvaluationException(message: String, unusedComplexity: Int) extends RuntimeException(message)
}
