package com.wavesplatform.lang.v1.evaluator
import cats.Id
import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.VerifierFunction
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Bindings
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, LoggedEvaluationContext}
import com.wavesplatform.lang.v1.task.imports
import com.wavesplatform.lang.v1.task.imports.raiseError
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.{AttachedPayments, Ord, Recipient, Tx}

import scala.util.Try

object ContractEvaluator {

  val DEFAULT_FUNC_NAME = "default"

  case class Invocation(
      funcCall: FUNCTION_CALL,
      caller: Recipient.Address,
      callerPk: ByteStr,
      payments: AttachedPayments,
      dappAddress: ByteStr,
      transactionId: ByteStr,
      fee: Long,
      feeAssetId: Option[ByteStr]
  )

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

  private def withDecls(dec: List[DECLARATION], block: BLOCK): EvalM[Id, Environment, EVALUATED] =
    EvaluatorV1().evalExpr(foldDeclarations(dec, block))

  private def foldDeclarations(dec: List[DECLARATION], block: BLOCK) =
    dec.foldRight(block)((d, e) => BLOCK(d, e))

  private def verifierBlock(v: VerifierFunction, entity: CaseObj) =
    BLOCK(LET(v.annotation.invocationArgName, entity), BLOCK(v.u, FUNCTION_CALL(FunctionHeader.User(v.u.name), List(entity))))

  def verify(decls: List[DECLARATION], v: VerifierFunction, tx: Tx): EvalM[Id, Environment, EVALUATED] =
    withDecls(decls, verifierBlock(v, Bindings.transactionObject(tx, proofsEnabled = true)))

  def verify(decls: List[DECLARATION], v: VerifierFunction, ord: Ord): EvalM[Id, Environment, EVALUATED] =
    withDecls(decls, verifierBlock(v, Bindings.orderObject(ord, proofsEnabled = true)))

  def apply(
      ctx: EvaluationContext[Environment, Id],
      dApp: DApp,
      i: Invocation,
      version: StdLibVersion
  ): Either[(ExecutionError, Log[Id]), (ScriptResult, Log[Id])] =
    for {
      expr <- buildExprFromInvocation(dApp, i, version).leftMap((_, Nil))
      (log, evaluation) = EvaluatorV1().applyWithLogging[EVALUATED](ctx, expr)
      result <- evaluation.flatMap(r => ScriptResult.fromObj(ctx, i.transactionId, r, version)).leftMap((_, log))
    } yield (result, log)

  def applyV2(ctx: EvaluationContext[Environment, Id], dApp: DApp, i: Invocation, version: StdLibVersion): Either[String, ScriptResult] =
    buildExprFromInvocation(dApp, i, version)
      .flatMap(applyV2(ctx, _, version, i.transactionId))

  def applyV2(
      ctx: EvaluationContext[Environment, Id],
      expr: EXPR,
      version: StdLibVersion,
      transactionId: ByteStr
  ): Either[ExecutionError, ScriptResult] = {
    val evaluator = new EvaluatorV2(ctx, version)
    Try(evaluator(expr.deepCopy, ContractLimits.MaxComplexityByVersion(version))).toEither
      .leftMap(_.getMessage)
      .flatMap {
        case (value: EVALUATED, _)          => ScriptResult.fromObj(ctx, transactionId, value, version)
        case (expr: EXPR, unusedComplexity) => Right(IncompleteResult(expr, unusedComplexity))
      }
  }
}
