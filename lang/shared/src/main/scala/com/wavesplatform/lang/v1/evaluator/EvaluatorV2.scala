//package com.wavesplatform.lang.v1.evaluator
//
//import cats.{Eval, Id}
//import cats.implicits._
//import com.wavesplatform.common.utils.EitherExt2
//import com.wavesplatform.lang.directives.values.StdLibVersion
//import com.wavesplatform.lang.v1.FunctionHeader
//import com.wavesplatform.lang.v1.compiler.Terms._
//import com.wavesplatform.lang.v1.compiler.Types.{CASETYPEREF, FINAL}
//import com.wavesplatform.lang.v1.evaluator.ctx.{BaseFunction, EvaluationContext, NativeFunction, UserFunction}
//import com.wavesplatform.lang.v1.evaluator.EvaluatorV2._
//import com.wavesplatform.lang.v1.traits.Environment
//
//import scala.annotation.tailrec
//import scala.collection.mutable
//
//object EvaluatorV2 {
//  sealed trait EvaluatorResult
//  case class Success(expr: EXPR)    extends EvaluatorResult
//  case object Stop                  extends EvaluatorResult
//}
//
//class EvaluatorV2(limit: Int, stdLibVersion: StdLibVersion) {
//  object Context {
//    def apply(context: EvaluationContext[Environment, Id]): Context = {
///*
//      def extractLet(let: LazyVal[Id]) =
//        (let.value.value.explicitGet(), Context(environment = context.environment, ???), true)
//*/
//
//      Context(
//        functions = context.functions,
//        //lets = context.letDefs.mapValues(extractLet), todo
//        types = context.typeDefs,
//        environment = context.environment
//      )
//    }
//  }
//
//  case class Context(
//    functions: Map[FunctionHeader, BaseFunction[Environment]] = Map(),
//    types: Map[String, FINAL] = Map(),
//    environment: Environment[Id],
//    cost: Int = 0
//  ) {
//    val isExhausted: Boolean =
//      cost >= limit
//
//    def withCost(addCost: Int): Context =
//      copy(cost = cost + addCost)
//  }
//
//  def root(expr: EXPR, ctx: Context, parentBlocks: List[BLOCK_DEF], update: EXPR => Unit): Eval[(EvaluatorResult, Context)] =
//    if (ctx.isExhausted) {
//      Eval.now((Stop, ctx))
//    } else
//      expr match {
//        case b: BLOCK_DEF     => evaluateBlock(b, ctx, parentBlocks, update)
//        case g: GETTER        => evaluateGetter(g, ctx, parentBlocks)
//        case i: IF            => evaluateIfBlock(i, ctx, parentBlocks)
//        case f: FUNCTION_CALL => evaluateFunctionCall(f, ctx, parentBlocks, update)
//        case REF(key)         => evaluateRef(key, ctx, parentBlocks)
//        case e: EVALUATED     => evaluated(e, ctx)
//      }
//
//  private def evaluateBlock(
//    block: EXPR with BLOCK_DEF,
//    ctx: Context,
//    parentBlocks: List[BLOCK_DEF],
//    update: EXPR => Unit
//  ) =
//    root(block.body, ctx, block :: parentBlocks, update)
//      .map { case (result, resultCtx) =>
//        result match {
//          case Stop =>
//            (Stop, resultCtx)
//
//          case s@Success(ev) if (ev.isInstanceOf[EVALUATED]) =>
//            parentBlocks.headOption.fold(update(ev))(_.body = ev)
//            (s, resultCtx)
//
//          case s@Success(ev)  =>
//            (Stop, resultCtx)
//        }
//      }
//
//  private def evaluated(e: EVALUATED, ctx: Context): Eval[(EvaluatorResult, Context)] =
//    Eval.now((Success(e), ctx))
//
//  private def evaluateRef(key: String, ctx: Context, parentBlocks: List[BLOCK_DEF]): Eval[(EvaluatorResult, Context)] = {
//    val (let, letParentBlocks) = findLet(key, parentBlocks)
//    let.value match {
//      case ev: EVALUATED =>
//        Eval.now((Success(ev), ctx.withCost(1)))
//      case expr =>
//        root(expr, ctx, letParentBlocks, let.value = _)
//          .map { case (letValue, resultCtx) =>
//            letValue match {
//              case Success(ev) if resultCtx.isExhausted =>
//                let.value = ev
//                (Stop, resultCtx)
//
//              case s@Success(ev) =>
//                let.value = ev
//                (s, resultCtx.withCost(1))
//
//              case r =>
//                (r, resultCtx)
//            }
//          }
//    }
//  }
//
//  @tailrec
//  private def findLet(key: String, parentBlocks: List[BLOCK_DEF]): (LET, List[BLOCK_DEF]) =
//    parentBlocks match {
//      case LET_BLOCK(let@LET(`key`, _), _) :: letParentBlocks => (let, letParentBlocks)
//      case BLOCK(let@LET(`key`, _), _) :: letParentBlocks     => (let, letParentBlocks)
//      case _ :: nextParentBlocks                              => findLet(key, nextParentBlocks)
//      case Nil                                                => throw new RuntimeException(s"Let `$key` not found")
//    }
//
//  private def evaluateFunctionCall(
//    call: FUNCTION_CALL,
//    ctx: Context,
//    parentBlocks: List[BLOCK_DEF],
//    update: EXPR => Unit
//  ): Eval[(EvaluatorResult, Context)] =
//    for {
//      (resultCtx, resultArgs) <- evaluateFunctionArgs(call.args, ctx, parentBlocks)
//      result                  <- evaluateFunctionCallExpr(call, resultCtx, resultArgs, parentBlocks)
//    } yield result
//
//  private def evaluateFunctionArgs(
//    args: List[EXPR],
//    ctx: Context,
//    parentBlocks: List[BLOCK_DEF]
//  ): Eval[(Context, List[EXPR])] = {
//    val buffer = args.toBuffer
//    args.zipWithIndex
//      .foldM(ctx) {
//        case (currentCtx, (nextArgExpr, i)) =>
//          if (currentCtx.isExhausted)
//            Eval.now(currentCtx)
//          else
//            root(nextArgExpr, currentCtx, parentBlocks, buffer(i) = _).map(_._2)
//      }
//      .map((_, buffer.toList))
//  }
//
//  private def evaluateFunctionCallExpr(
//    call: FUNCTION_CALL,
//    ctx: Context,
//    resultArgs: List[EXPR],
//    parentBlocks: List[BLOCK_DEF]
//  ): Eval[(EvaluatorResult, Context)] = {
//    val updatedArgs = resultArgs ::: call.args.drop(resultArgs.size)
//    call.args = updatedArgs
//    if (ctx.isExhausted) {
//      Eval.now((Success(call.copy(args = updatedArgs)), ctx))
//    } else {
//      val evaluatedArgs = updatedArgs.asInstanceOf[List[EVALUATED]]        //todo type safety
//      ctx.functions.get(call.function)
//        .orElse(findUserFunction(call.function.funcName, parentBlocks))
//        .fold(
//          Eval.now((tryCreateObject(call.function.funcName, evaluatedArgs, ctx), ctx))
//        )(
//          evaluateFunctionBody(_, call, ctx, evaluatedArgs, parentBlocks)
//        )
//    }
//  }
//
//  private def findUserFunction(name: String, parentBlocks: List[BLOCK_DEF]): Option[UserFunction[Environment]] =
//    parentBlocks match {
//      case BLOCK(func@FUNC(`name`, _, _), _) :: _ => Some(mapToUserFunction(func))
//      case _ :: nextParentBlocks                  => findUserFunction(name, nextParentBlocks)
//      case Nil                                    => None
//    }
//
//  private def mapToUserFunction(func: FUNC): UserFunction[Environment] =
//    UserFunction[Environment](func.name, 0, null, func.args.map(n => (n, null)): _*)(func.body)
//
//  private def tryCreateObject(constructor: String, args: List[EVALUATED], ctx: Context): EvaluatorResult = {
//    val objectType = ctx.types(constructor).asInstanceOf[CASETYPEREF]  // todo handle absence
//    val fields = objectType.fields.map(_._1) zip args
//    Success(CaseObj(objectType, fields.toMap))
//  }
//
//  private def evaluateFunctionBody(
//    function: BaseFunction[Environment],
//    call: FUNCTION_CALL,
//    ctx: Context,
//    evaluatedArgs: List[EVALUATED],
//    parentBlocks: List[BLOCK_DEF]
//  ): Eval[(EvaluatorResult, Context)] =
//    function match {
//      case NativeFunction(_, costByVersion, _, ev, _) =>
//        val cost = costByVersion(stdLibVersion).toInt
//        val result =
//          if (ctx.cost + cost > limit) {
//            call.args = evaluatedArgs
//            (Stop, ctx)            //todo test
//          } else {
//            val result = ev[Id]((ctx.environment, evaluatedArgs)).explicitGet()
//            (Success(result), ctx.withCost(cost))
//          }
//        Eval.now(result)
//      case UserFunction(_, _, _, signature, expr, _) =>
//        val argsWithExpr =
//          (signature.args zip evaluatedArgs)
//            .foldRight(expr[Id](ctx.environment).deepCopy()) {
//              case (((argName, _), argValue), argsWithExpr) => //todo maybe check for existence
//                BLOCK(LET(argName, argValue), argsWithExpr)
//            }
//        root(argsWithExpr, ctx, parentBlocks)
//    }
//
//  private def evaluateIfBlock(
//    i: IF,
//    ctx: Context,
//    parentBlocks: List[BLOCK_DEF]
//  ): Eval[(EvaluatorResult, Context)] =
//    for {
//      (condResult, condResultCtx) <- root(i.cond, ctx, parentBlocks)
//      result <-
//        condResult match {
//          case Stop =>
//            Eval.now((Stop, condResultCtx))
//          case Success(ev) if condResultCtx.isExhausted =>
//            i.cond = ev
//            Eval.now((Stop, condResultCtx))
//          case Success(ev) =>
//            ev match {
//              case TRUE  =>
//                //condResultCtx.setRef(i.ifTrue)
//                root(i.ifTrue,  condResultCtx.withCost(1), parentBlocks)
//              case FALSE =>
//                //condResultCtx.setRef(i.ifFalse)
//                root(i.ifFalse, condResultCtx.withCost(1), parentBlocks)
//              case _     => ???                                                    // todo ???
//            }
//        }
//    } yield result
//
//  private def evaluateGetter(
//    getter: GETTER,
//    ctx: Context,
//    parentBlocks: List[BLOCK_DEF]
//  ): Eval[(EvaluatorResult, Context)] =
//    root(getter.expr, ctx, parentBlocks).map {
//      case (exprResult, exprResultCtx) =>
//        exprResult match {
//          case Stop =>
//            (Stop, exprResultCtx)
//          case Success(ev) if exprResultCtx.isExhausted =>
//            getter.expr = ev
//            (Stop, exprResultCtx)
//          case Success(ev) =>
//            val fields = ev.asInstanceOf[CaseObj].fields
//            (Success(fields(getter.field)), exprResultCtx.withCost(1))  // todo handle absence
//        }
//    }
//}
