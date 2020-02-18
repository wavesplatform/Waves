package com.wavesplatform.lang.v1.evaluator

import cats.{Eval, Id}
import cats.implicits._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.{CASETYPEREF, FINAL}
import com.wavesplatform.lang.v1.evaluator.ctx.{BaseFunction, EvaluationContext, LazyVal, NativeFunction, UserFunction}
import com.wavesplatform.lang.v1.traits.Environment

class EvaluatorV2(limit: Int, stdLibVersion: StdLibVersion) {
  object Context {
    def apply(context: EvaluationContext[Environment, Id]): Context = {
      def extractLet(let: LazyVal[Id]) =
        (let.value.value.explicitGet(), Context(environment = context.environment), true)

      Context(
        functions = context.functions,
        //lets = context.letDefs.mapValues(extractLet), todo
        types = context.typeDefs,
        environment = context.environment
      )
    }
  }

  case class Context(
    lets: Map[String, (EXPR, Boolean)] = Map(),
    functions: Map[FunctionHeader, BaseFunction[Environment]] = Map(),
    types: Map[String, FINAL] = Map(),
    environment: Environment[Id],
    cost: Int = 0
  ) {
    val isExhausted: Boolean =
      cost >= limit

    def withCost(addCost: Int): Context =
      copy(cost = cost + addCost)

    def withLet(newLet: LET, isEvaluated: Boolean): Context =
      copy(lets = lets + (newLet.name -> (newLet.value, isEvaluated)))

    def withoutLet(let: String): Context =
      copy(lets = lets - let)

    def withFunction(function: UserFunction[Environment]): Context =
      copy(functions = functions + (function.header -> function))

    def withoutFunction(header: FunctionHeader): Context =
      copy(functions = functions - header)
  }

  def root(expr: EXPR, ctx: Context): Eval[(EXPR, Context)] =
    if (ctx.isExhausted)
      Eval.now((expr, ctx))
    else
      expr match {
        case LET_BLOCK(let, body)          => evaluateLetBlock(let, body, ctx)
        case BLOCK(let: LET, body)         => evaluateLetBlock(let, body, ctx)
        case BLOCK(func: FUNC, body)       => evaluateFunctionBlock(func, body, ctx)
        case GETTER(obj, field)            => evaluateGetter(obj, field, ctx)
        case IF(cond, ifTrue, ifFalse)     => evaluateIfBlock(cond, ifTrue, ifFalse, ctx)
        case FUNCTION_CALL(function, args) => evaluateFunctionCall(function, args, ctx)
        case REF(key)                      => evaluateRef(key, ctx)
        case e: EVALUATED                  => evaluated(e, ctx)
      }

  private def evaluated(e: EVALUATED, ctx: Context): Eval[(EXPR, Context)] =
    Eval.now((e, ctx))

  private def evaluateLetBlock(let: LET, nextExpr: EXPR, ctx: Context): Eval[(EXPR, Context)] = {
    root(nextExpr, ctx.withLet(let, isEvaluated = false))
      .map {
        case (nextExprResult, nextExprCtx) =>
          val resultExpr =
            if (nextExprResult.isInstanceOf[EVALUATED])
              nextExprResult
            else {
              val letDecl = LET(let.name, nextExprCtx.lets(let.name)._1)
              BLOCK(letDecl, nextExprResult)
            }
          val overlapFixedCtx =
            ctx.lets.get(let.name)
              .fold(nextExprCtx.withoutLet(let.name)) {
                case (expr, isEvaluated) => nextExprCtx.withLet(LET(let.name, expr), isEvaluated)
              }
          (resultExpr, overlapFixedCtx)
      }
  }

  private def evaluateFunctionBlock(funcDef: FUNC, nextExpr: EXPR, ctx: Context): Eval[(EXPR, Context)] = {
    val function = UserFunction[Environment](funcDef.name, 0, null, funcDef.args.map(n => (n, null)): _*)(funcDef.body)
    root(nextExpr, ctx.withFunction(function))
      .map {
        case (nextExprResult, nextExprCtx) =>
          val resultExpr =
            if (nextExprResult.isInstanceOf[EVALUATED]) nextExprResult
            else BLOCK(funcDef, nextExprResult)
          val overlapFixedCtx =
            ctx.functions
              .get(function.header)
              .collect { case f: UserFunction[Environment] => f }
              .fold(nextExprCtx.withoutFunction(function.header))(nextExprCtx.withFunction)
          (resultExpr, overlapFixedCtx)
      }
  }

  private def evaluateRef(key: String, ctx: Context): Eval[(EXPR, Context)] = {
    val (letExpr, isEvaluated) = ctx.lets(key)
    if (isEvaluated)
      Eval.now((letExpr, ctx.withCost(1)))
    else
      root(letExpr, ctx)
        .map {
          case (letValue, resultCtx) =>
            if (resultCtx.isExhausted)
              (REF(key), resultCtx.withLet(LET(key, letValue), isEvaluated = false))
            else
              (letValue, resultCtx.withLet(LET(key, letValue), isEvaluated = true).withCost(1))
        }
  }

  private def evaluateFunctionCall(
    header: FunctionHeader,
    args: List[EXPR],
    ctx: Context
  ): Eval[(EXPR, Context)] =
    for {
      (resultCtx, resultArgs) <- evaluateFunctionArgs(args, ctx)
      result                  <- evaluateFunctionExpr(header, resultCtx, args, resultArgs)
    } yield result

  private def evaluateFunctionExpr(
    header: FunctionHeader,
    ctx: Context,
    startArgs : List[EXPR],
    resultArgs: List[EXPR]
  ): Eval[(EXPR, Context)] =
    if (ctx.isExhausted) {
      val call = FUNCTION_CALL(header, resultArgs ::: startArgs.drop(resultArgs.size))
      Eval.now((call, ctx))
    } else {
      val evaluatedArgs = resultArgs.asInstanceOf[List[EVALUATED]] //todo type safety
      ctx.functions.get(header)
        .fold(
          Eval.now((tryCreateObject(header.funcName, evaluatedArgs, ctx), ctx))
        )(
          evaluateBaseFunctionBody(_, ctx, evaluatedArgs)
        )
    }

  private def evaluateFunctionArgs(args: List[EXPR], ctx: Context): Eval[(Context, List[EXPR])] =
    args.foldM((ctx, List.empty[EXPR])) {
      case ((currentCtx, evaluatedArgs), nextArgExpr) =>
        if (currentCtx.isExhausted)
          Eval.now((currentCtx, evaluatedArgs))
        else
          root(nextArgExpr, currentCtx)
            .map { case (evaluatedArg, newCtx) =>
              (newCtx, evaluatedArgs :+ evaluatedArg) // todo optimize?
            }
    }

  private def tryCreateObject(constructor: String, args: List[EVALUATED], ctx: Context): EXPR = {
    val objectType = ctx.types(constructor).asInstanceOf[CASETYPEREF]
    val fields = objectType.fields.map(_._1) zip args
    CaseObj(objectType, fields.toMap)
  }

  private def evaluateBaseFunctionBody(
    function: BaseFunction[Environment],
    ctx: Context,
    evaluatedArgs: List[EVALUATED]
  ): Eval[(EXPR, Context)] =
    function match {
      case NativeFunction(_, costByVersion, _, ev, _) =>
        val cost = costByVersion(stdLibVersion).toInt
        val result =
          if (ctx.cost + cost > limit)
            (FUNCTION_CALL(function.header, evaluatedArgs), ctx)  //todo test
          else {
            val result = ev[Id]((ctx.environment, evaluatedArgs)).explicitGet()
            (result, ctx.withCost(cost))
          }
        Eval.now(result)
      case UserFunction(_, _, _, signature, expr, _) =>
        val argsWithExpr =
          (signature.args zip evaluatedArgs)
            .foldRight(expr[Id](ctx.environment)) {
              case (((argName, _), argValue), argsWithExpr) => //todo maybe check for existence
                BLOCK(LET(argName, argValue), argsWithExpr)
            }
        root(argsWithExpr, ctx)
    }

  private def evaluateIfBlock(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR, ctx: Context): Eval[(EXPR, Context)] =
    for {
      (condResult, condResultCtx) <- root(cond, ctx)
      result <-
        if (condResultCtx.isExhausted)
          Eval.now((IF(condResult, ifTrue, ifFalse), condResultCtx))
        else
          condResult match {
            case TRUE  => root(ifTrue,  condResultCtx.withCost(1))
            case FALSE => root(ifFalse, condResultCtx.withCost(1))
            case _     => ???                                       // todo ???
          }
    } yield result

  private def evaluateGetter(obj: EXPR, field: String, ctx: Context): Eval[(EXPR, Context)] = {
    root(obj, ctx).map {
      case (exprResult, exprResultCtx) =>
        val fields = exprResult.asInstanceOf[CaseObj].fields
        if (exprResultCtx.isExhausted)
          (GETTER(exprResult, field), exprResultCtx)
        else
          (fields(field), exprResultCtx.withCost(1))                // todo handle absence
    }
  }
}
