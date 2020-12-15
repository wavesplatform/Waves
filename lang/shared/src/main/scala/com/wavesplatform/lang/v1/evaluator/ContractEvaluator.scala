package com.wavesplatform.lang.v1.evaluator

import cats.Id
import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.VerifierFunction
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Bindings
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, LazyVal}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.{AttachedPayments, Recipient}

object ContractEvaluator {

  val DEFAULT_FUNC_NAME = "default"

  case class Invocation(
      funcCall: FUNCTION_CALL,
      caller: Recipient.Address,
      callerPk: ByteStr,
      payments: AttachedPayments,
      transactionId: ByteStr,
      fee: Long,
      feeAssetId: Option[ByteStr]
  )

  def buildSyntheticCall(contract: DApp, call: EXPR): EXPR = {
    val callables = contract.callableFuncs.flatMap { cf =>
      val argName = cf.annotation.invocationArgName
      val invocation = Invocation(
        null,
        Recipient.Address(ByteStr(new Array[Byte](26))),
        ByteStr(new Array[Byte](32)),
        AttachedPayments.Single(None),
        ByteStr(new Array[Byte](32)),
        0L,
        None
      )
      LET(argName, Bindings.buildInvocation(invocation, StdLibVersion.VersionDic.default)) :: cf.u :: Nil
    }

    foldDeclarations(contract.decs ++ callables, BLOCK(LET("__synthetic_call", TRUE), call))
  }

  private def buildExprFromInvocation(c: DApp, i: Invocation, version: StdLibVersion): Either[String, EXPR] = {
    val functionName = i.funcCall.function.funcName

    val contractFuncAndCallOpt = c.callableFuncs.find(_.u.name == functionName).map((_, i.funcCall))

    contractFuncAndCallOpt match {
      case None =>
        val otherFuncs = c.decs.filter(_.isInstanceOf[FUNC]).map(_.asInstanceOf[FUNC].name)
        val message =
          if (otherFuncs contains functionName)
            s"function '$functionName exists in the script but is not marked as @Callable, therefore cannot not be invoked"
          else s"@Callable function '$functionName' doesn't exist in the script"
        message.asLeft[EXPR]

      case Some((f, fc)) =>
        val takingArgsNumber = f.u.args.size
        val passedArgsNumber = fc.args.size
        if (takingArgsNumber == passedArgsNumber) {
          foldDeclarations(
            c.decs,
            BLOCK(
              LET(f.annotation.invocationArgName, Bindings.buildInvocation(i, version)),
              BLOCK(f.u, fc)
            )
          ).asRight[ExecutionError]
        } else {
          s"function '$functionName takes $takingArgsNumber args but $passedArgsNumber were(was) given"
            .asLeft[EXPR]
        }
    }
  }

  private def foldDeclarations(dec: List[DECLARATION], block: BLOCK) =
    dec.foldRight(block)((d, e) => BLOCK(d, e))

  def verify(
      decls: List[DECLARATION],
      v: VerifierFunction,
      ctx: EvaluationContext[Environment, Id],
      evaluate: (EvaluationContext[Environment, Id], EXPR) => Either[(ExecutionError, Log[Id]), (EVALUATED, Log[Id])],
      entity: CaseObj
  ): Either[(ExecutionError, Log[Id]), (EVALUATED, Log[Id])] = {
    val verifierBlock =
      BLOCK(
        LET(v.annotation.invocationArgName, entity),
        BLOCK(v.u, FUNCTION_CALL(FunctionHeader.User(v.u.name), List(entity)))
      )

    evaluate(ctx, foldDeclarations(decls, verifierBlock))
  }

  def apply(
      ctx: EvaluationContext[Environment, Id],
      dApp: DApp,
      i: Invocation,
      version: StdLibVersion
  ): Either[(ExecutionError, Log[Id]), (ScriptResult, Log[Id])] =
    for {
      expr              <- buildExprFromInvocation(dApp, i, version).leftMap((_, Nil))
      (evaluation, log) <- EvaluatorV1().applyWithLogging[EVALUATED](ctx, expr)
      result            <- ScriptResult.fromObj(ctx, i.transactionId, evaluation, version).leftMap((_, log))
    } yield (result, log)

  def applyV2(
      ctx: EvaluationContext[Environment, Id],
      freezingLets: Map[String, LazyVal[Id]],
      dApp: DApp,
      i: Invocation,
      version: StdLibVersion,
      limit: Int
  ): Either[(ExecutionError, Log[Id]), (ScriptResult, Log[Id])] =
    buildExprFromInvocation(dApp, i, version)
      .leftMap((_, Nil))
      .flatMap(applyV2(ctx, freezingLets, _, version, i.transactionId, limit))

  def applyV2(
      ctx: EvaluationContext[Environment, Id],
      freezingLets: Map[String, LazyVal[Id]],
      expr: EXPR,
      version: StdLibVersion,
      transactionId: ByteStr,
      limit: Int
  ): Either[(ExecutionError, Log[Id]), (ScriptResult, Log[Id])] = {
    val exprWithLets =
      freezingLets.foldLeft(expr) {
        case (buildingExpr, (letName, letValue)) =>
          BLOCK(LET(letName, letValue.value.value.explicitGet()), buildingExpr)
      }
    EvaluatorV2
      .applyLimited(exprWithLets, limit, ctx, version)
      .flatMap {
        case (expr, unusedComplexity, log) =>
          val result =
            expr match {
              case value: EVALUATED => ScriptResult.fromObj(ctx, transactionId, value, version)
              case expr: EXPR       => Right(IncompleteResult(expr, unusedComplexity))
            }
          result.bimap((_, log), (_, log))
      }
  }
}
