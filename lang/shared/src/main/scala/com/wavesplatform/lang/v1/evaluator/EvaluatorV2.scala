package com.wavesplatform.lang.v1.evaluator

import cats.Id
import cats.instances.lazyList.*
import cats.syntax.either.*
import cats.syntax.foldable.*
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ContextfulNativeFunction.{Extended, Simple}
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, LoggedEvaluationContext, NativeFunction, UserFunction}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.{CommonError, ExecutionError}
import monix.eval.Coeval
import shapeless.syntax.std.tuple.*

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class EvaluatorV2(
    val ctx: LoggedEvaluationContext[Environment, Id],
    val stdLibVersion: StdLibVersion,
    val correctFunctionCallScope: Boolean,
    val newMode: Boolean,
    val checkConstructorArgsTypes: Boolean = false
) {
  private val overheadCost: Int = if (newMode) 0 else 1

  private def root(
      expr: EXPR,
      update: EXPR => EvaluationResult[Unit],
      limit: Int,
      parentBlocks: List[BLOCK_DEF]
  ): EvaluationResult[Int] = {
    def evaluateFunctionArgs(fc: FUNCTION_CALL): EvaluationResult[Int] =
      Defer {
        fc.args.indices
          .to(LazyList)
          .foldM(limit) { case (unused, argIndex) =>
            if (unused <= 0)
              EvaluationResult(unused)
            else
              root(
                expr = fc.args(argIndex),
                update = argValue => EvaluationResult(fc.args = fc.args.updated(argIndex, argValue)),
                limit = unused,
                parentBlocks
              )
          }
      }

    def evaluateFunction(fc: FUNCTION_CALL, startArgs: List[EXPR], limit: Int): EvaluationResult[Int] = {
      val r = fc.function match {
        case FunctionHeader.Native(_) =>
          evaluateNativeFunction(fc, limit)
        case FunctionHeader.User(_, name) =>
          evaluateUserFunction(fc, limit, name, startArgs)
            .getOrElse(evaluateConstructor(fc, limit, name))
      }
      if (newMode)
        r.map(unused => if (unused == limit) unused - 1 else unused)
      else
        r
    }

    def evaluateNativeFunction(fc: FUNCTION_CALL, limit: Int): EvaluationResult[Int] =
      for {
        function <- EvaluationResult(Coeval {
          ctx.ec.functions
            .get(fc.function)
            .toRight((CommonError(s"function '${fc.function}' not found"), limit))
        })
        cost = function.costByLibVersion(stdLibVersion).toInt
        result <-
          if (limit < cost)
            EvaluationResult(limit)
          else
            doEvaluateNativeFunction(fc, function.asInstanceOf[NativeFunction[Environment]], limit, cost)
      } yield result

    def doEvaluateNativeFunction(fc: FUNCTION_CALL, function: NativeFunction[Environment], limit: Int, cost: Int): EvaluationResult[Int] = {
      val args = fc.args.asInstanceOf[List[EVALUATED]]
      val evaluation = function.ev match {
        case f: Extended[Environment] => f.evaluate[Id](ctx.ec.environment, args, limit - cost)
        case f: Simple[Environment]   => Coeval((f.evaluate(ctx.ec.environment, args), limit - cost))
      }
      for {
        (result, unusedComplexity) <- EvaluationResult(
          evaluation
            .map { case (result, evaluatedComplexity) =>
              result.bimap((_, evaluatedComplexity), (_, evaluatedComplexity))
            }
            .onErrorHandleWith {
              case _: SecurityException =>
                Coeval(Left((CommonError(s"""An access to ${function.ev} is denied"""), 0)))
              case e: Throwable =>
                val error = e.getMessage match {
                  case null => e.toString
                  case msg  => msg
                }
                Coeval(Left((CommonError(s"""An error during run ${function.ev}: ${e.getClass} $error"""), 0)))
            }
        )
        _ <- update(result)
      } yield unusedComplexity
    }

    def evaluateUserFunction(fc: FUNCTION_CALL, limit: Int, name: String, startArgs: List[EXPR]): Option[EvaluationResult[Int]] =
      ctx.ec.functions
        .get(fc.function)
        .map(_.asInstanceOf[UserFunction[Environment]])
        .map { f =>
          val func = FUNC(f.name, f.args.toList, f.ev[Id](ctx.ec.environment, startArgs))
          val precalculatedLimit =
            if (newMode) {
              val cost = f.costByLibVersion(stdLibVersion).toInt
              Some(limit - cost)
            } else
              None
          (func, precalculatedLimit, parentBlocks)
        }
        .orElse(findUserFunction(name, parentBlocks).map { case (func, blocks) => (func, None, blocks) })
        .map { case (signature, precalculatedLimitOpt, functionScopeBlocks) =>
          val argsWithExpr =
            (signature.args zip fc.args)
              .foldRight(signature.body.deepCopy.value) { case ((argName, argValue), argsWithExpr) =>
                BLOCK(LET(argName, argValue), argsWithExpr)
              }
          update(argsWithExpr)
            .flatMap { _ =>
              val blocks = if (correctFunctionCallScope) functionScopeBlocks else parentBlocks
              root(argsWithExpr, update, precalculatedLimitOpt.getOrElse(limit), blocks)
            }
            .map(r => precalculatedLimitOpt.getOrElse(r))
        }

    def evaluateConstructor(fc: FUNCTION_CALL, limit: Int, name: String): EvaluationResult[Int] =
      for {
        objectType <- ctx.ec.typeDefs.get(name) match {
          case Some(caseType: CASETYPEREF) => EvaluationResult(caseType)
          case _                           => EvaluationResult(s"Function or type '$name' not found", limit)
        }
        passedArgs = fc.args.asInstanceOf[List[EVALUATED]]
        _ <- if (checkConstructorArgsTypes) doCheckConstructorArgsTypes(objectType, passedArgs, limit) else EvaluationResult(())
        fields = objectType.fields.map(_._1) zip passedArgs
        r <- root(CaseObj(objectType, fields.toMap), update, limit, parentBlocks)
      } yield r

    def doCheckConstructorArgsTypes(objectType: CASETYPEREF, passedArgs: List[EVALUATED], limit: Int): EvaluationResult[Unit] = {
      def str[T](l: List[T]) = l.mkString("(", ", ", ")")

      if (objectType.fields.size != passedArgs.size)
        EvaluationResult(s"Constructor ${objectType.name} expected ${objectType.fields.size} args, but ${str(passedArgs)} was passed", limit)
      else {
        val fieldTypes      = objectType.fields.map(_._2)
        val passedArgsTypes = passedArgs.map(_.getType)
        if (!(fieldTypes zip passedArgsTypes).forall { case (fieldType, passedType) => fieldType >= passedType })
          EvaluationResult(s"Passed args ${str(passedArgs)} are unsuitable for constructor ${objectType.name}${str(fieldTypes)}", limit)
        else
          EvaluationResult(())
      }
    }

    expr match {
      case b: BLOCK_DEF =>
        Defer {
          root(
            expr = b.body,
            update = {
              case ev: EVALUATED => Defer(update(ev))
              case nonEvaluated  => EvaluationResult(b.body = nonEvaluated)
            },
            limit = limit,
            parentBlocks = b :: parentBlocks
          )
        }
      case g: GETTER =>
        Defer {
          root(
            expr = g.expr,
            update = v => EvaluationResult(g.expr = v),
            limit = limit,
            parentBlocks = parentBlocks
          ).flatMap { unused =>
            g.expr match {
              case co: CaseObj if unused > 0 => update(co.fields(g.field)).map(_ => unused - overheadCost)
              case _: CaseObj                => EvaluationResult(unused)
              case ev: EVALUATED             => EvaluationResult(s"GETTER of non-case-object $ev with field '${g.field}", unused)
              case _                         => EvaluationResult(unused)
            }
          }
        }
      case i: IF =>
        Defer {
          root(
            expr = i.cond,
            update = v => EvaluationResult(i.cond = v),
            limit = limit,
            parentBlocks = parentBlocks
          ).flatMap { unused =>
            i.cond match {
              case TRUE | FALSE if unused <= 0 =>
                EvaluationResult(unused)
              case TRUE if unused > 0 =>
                update(i.ifTrue).flatMap(_ =>
                  root(
                    expr = i.ifTrue,
                    update = update,
                    limit = unused - overheadCost,
                    parentBlocks = parentBlocks
                    )
                )
              case FALSE if unused > 0 =>
                update(i.ifFalse).flatMap(_ =>
                  root(
                    expr = i.ifFalse,
                    update = update,
                    limit = unused - overheadCost,
                    parentBlocks = parentBlocks
                    )
                )
              case _: EVALUATED => EvaluationResult("Non-boolean result in cond", unused)
              case _            => EvaluationResult(unused)
            }
          }
        }

      case REF(key) =>
        Defer {
          visitRef(key, update, limit, parentBlocks)
            .orElse(findGlobalVar(key, update, limit))
            .getOrElse(EvaluationResult(s"A definition of '$key' not found", limit))
        }

      case fc: FUNCTION_CALL =>
        val startArgs = fc.args
        evaluateFunctionArgs(fc)
          .flatMap { unusedArgsComplexity =>
            val argsEvaluated = fc.args.forall(_.isInstanceOf[EVALUATED])
            if (argsEvaluated && unusedArgsComplexity > 0)
              evaluateFunction(fc, startArgs, unusedArgsComplexity)
            else
              EvaluationResult(unusedArgsComplexity)
          }

      case evaluated: EVALUATED =>
        update(evaluated).map(_ => limit)

      case f: FAILED_EXPR => EvaluationResult(s"Unexpected $f", limit)
    }
  }

  @tailrec
  private def visitRef(
      key: String,
      update: EVALUATED => EvaluationResult[Unit],
      limit: Int,
      parentBlocks: List[BLOCK_DEF]
  ): Option[EvaluationResult[Int]] =
    parentBlocks match {
      case LET_BLOCK(l @ LET(`key`, _), _) :: nextParentBlocks => Some(evaluateRef(update, limit, l, nextParentBlocks))
      case BLOCK(l @ LET(`key`, _), _) :: nextParentBlocks     => Some(evaluateRef(update, limit, l, nextParentBlocks))
      case _ :: nextParentBlocks                               => visitRef(key, update, limit, nextParentBlocks)
      case Nil                                                 => None
    }

  private def evaluateRef(
      update: EVALUATED => EvaluationResult[Unit],
      limit: Int,
      let: LET,
      nextParentBlocks: List[BLOCK_DEF]
  ): EvaluationResult[Int] = {
    val result = root(
      expr = let.value,
      update = v =>
        EvaluationResult(let.value = v)
          .map(_ =>
            let.value match {
              case e: EVALUATED => ctx.log(let, Right(e))
              case _            =>
            }
          ),
      limit = limit,
      parentBlocks = nextParentBlocks
    ).flatMap { unused =>
      let.value match {
        case ev: EVALUATED if unused > 0 => update(ev).map(_ => unused - overheadCost)
        case _                           => EvaluationResult(unused)
      }
    }
    logError(let, result)
  }

  private def logError(let: LET, r: EvaluationResult[Int]): EvaluationResult[Int] =
    EvaluationResult(
      r.value
        .map(_.leftMap { case l @ (error, _) =>
          ctx.log(let, Left(error))
          l
        })
    )

  private def findGlobalVar(key: String, update: EVALUATED => EvaluationResult[Unit], limit: Int): Option[EvaluationResult[Int]] =
    ctx.ec.letDefs
      .get(key)
      .map(
        _.value.value.fold(
          error => EvaluationResult(error.message, limit),
          value => update(value).map(_ => limit - overheadCost)
        )
      )

  @tailrec
  private def findUserFunction(name: String, parentBlocks: List[BLOCK_DEF]): Option[(FUNC, List[BLOCK_DEF])] =
    parentBlocks match {
      case (_: LET_BLOCK) :: xs                            => findUserFunction(name, xs)
      case BLOCK(f @ FUNC(`name`, _, _), _) :: scopeBlocks => Some((f, scopeBlocks))
      case _ :: xs                                         => findUserFunction(name, xs)
      case Nil                                             => None
    }
}

object EvaluatorV2 {
  def applyLimitedCoeval(
      expr: EXPR,
      limit: Int,
      ctx: EvaluationContext[Environment, Id],
      stdLibVersion: StdLibVersion,
      correctFunctionCallScope: Boolean,
      newMode: Boolean,
      checkConstructorArgsTypes: Boolean = false
  ): Coeval[Either[(ExecutionError, Int, Log[Id]), (EXPR, Int, Log[Id])]] = {
    val log       = ListBuffer[LogItem[Id]]()
    val loggedCtx = LoggedEvaluationContext[Environment, Id](name => value => log.append((name, value)), ctx)
    var ref       = expr.deepCopy.value
    new EvaluatorV2(loggedCtx, stdLibVersion, correctFunctionCallScope, newMode, checkConstructorArgsTypes)
      .root(ref, v => EvaluationResult { ref = v }, limit, Nil)
      .map((ref, _))
      .value
      .redeem(
        e => Left((e.getMessage, limit, log.toList)),
        _.bimap(_ :+ log.toList, _ :+ log.toList)
      )
  }

  def applyOrDefault(
      ctx: EvaluationContext[Environment, Id],
      expr: EXPR,
      stdLibVersion: StdLibVersion,
      complexityLimit: Int,
      correctFunctionCallScope: Boolean,
      newMode: Boolean,
      handleExpr: EXPR => Either[ExecutionError, EVALUATED]
  ): (Log[Id], Int, Either[ExecutionError, EVALUATED]) =
    EvaluatorV2
      .applyLimitedCoeval(expr, complexityLimit, ctx, stdLibVersion, correctFunctionCallScope, newMode)
      .value()
      .fold(
        { case (error, complexity, log) => (log, complexity, Left(error)) },
        { case (result, complexity, log) =>
          result match {
            case evaluated: EVALUATED => (log, complexity, Right(evaluated))
            case expr: EXPR           => (log, complexity, handleExpr(expr))
          }
        }
      )

  def applyCompleted(
      ctx: EvaluationContext[Environment, Id],
      expr: EXPR,
      stdLibVersion: StdLibVersion,
      correctFunctionCallScope: Boolean,
      newMode: Boolean
  ): (Log[Id], Int, Either[ExecutionError, EVALUATED]) =
    applyOrDefault(
      ctx,
      expr,
      stdLibVersion,
      Int.MaxValue,
      correctFunctionCallScope,
      newMode,
      expr => Left(s"Unexpected incomplete evaluation result $expr")
    )
}
