package com.wavesplatform.lang.v1.evaluator

import cats.Id
import cats.syntax.either.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.VerifierFunction
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Bindings
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain.{AttachedPayments, Recipient}
import com.wavesplatform.lang.{CommonError, ExecutionError}
import monix.eval.Coeval

object ContractEvaluator {

  val DEFAULT_FUNC_NAME = "default"

  case class Invocation(
      funcCall: FUNCTION_CALL,
      caller: Recipient.Address,
      callerPk: ByteStr,
      originCaller: Recipient.Address,
      originCallerPublicKey: ByteStr,
      payments: AttachedPayments,
      transactionId: ByteStr,
      fee: Long,
      feeAssetId: Option[ByteStr]
  )

  // Class for passing invocation argument object in error log during script execution
  case class ExprWithInvArg(expr: EXPR, invArg: Option[LET])
  case class LogExtraInfo(invokedFuncName: Option[String] = None, invArg: Option[LET] = None, dAppAddress: Option[Address] = None)

  def buildSyntheticCall(contract: DApp, call: EXPR, callerAddress: ByteStr, callerPk: ByteStr): EXPR = {
    val callables = contract.callableFuncs.flatMap { cf =>
      val argName = cf.annotation.invocationArgName
      val invocation = Invocation(
        null,
        Recipient.Address(callerAddress),
        callerPk,
        Recipient.Address(callerAddress),
        callerPk,
        AttachedPayments.Single(None),
        ByteStr(new Array[Byte](32)),
        0L,
        None
      )
      LET(argName, Bindings.buildInvocation(invocation, StdLibVersion.VersionDic.default)) :: cf.u :: Nil
    }
    foldDeclarations(contract.decs ++ callables, call)
  }

  def buildExprFromInvocation(c: DApp, i: Invocation, version: StdLibVersion): Either[ExecutionError, ExprWithInvArg] = {
    val functionName = i.funcCall.function.funcName

    val contractFuncAndCallOpt = c.callableFuncs.find(_.u.name == functionName).map((_, i.funcCall))

    contractFuncAndCallOpt match {
      case None =>
        val otherFuncs = c.decs.filter(_.isInstanceOf[FUNC]).map(_.asInstanceOf[FUNC].name)
        val message =
          if (otherFuncs contains functionName)
            s"function '$functionName exists in the script but is not marked as @Callable, therefore cannot not be invoked"
          else s"@Callable function '$functionName' doesn't exist in the script"
        CommonError(message).asLeft[ExprWithInvArg]

      case Some((f, fc)) =>
        val takingArgsNumber = f.u.args.size
        val passedArgsNumber = fc.args.size
        if (takingArgsNumber == passedArgsNumber) {
          val invocationArgLet = LET(f.annotation.invocationArgName, Bindings.buildInvocation(i, version))
          ExprWithInvArg(
            foldDeclarations(
              c.decs,
              BLOCK(
                invocationArgLet,
                BLOCK(f.u, fc)
              )
            ),
            Some(invocationArgLet)
          ).asRight[ExecutionError]
        } else {
          CommonError(s"function '$functionName takes $takingArgsNumber args but $passedArgsNumber were(was) given")
            .asLeft[ExprWithInvArg]
        }
    }
  }

  private def foldDeclarations(dec: List[DECLARATION], expr: EXPR) =
    dec.foldRight(expr)((d, e) => BLOCK(d, e))

  def verify(
      decls: List[DECLARATION],
      v: VerifierFunction,
      evaluate: (EXPR, LogExtraInfo) => (Log[Id], Int, Either[ExecutionError, EVALUATED]),
      entity: CaseObj
  ): (Log[Id], Int, Either[ExecutionError, EVALUATED]) = {
    val invocationArgLet = LET(v.annotation.invocationArgName, entity)
    val verifierBlock =
      BLOCK(
        invocationArgLet,
        BLOCK(v.u, FUNCTION_CALL(FunctionHeader.User(v.u.name), List(entity)))
      )

    evaluate(foldDeclarations(decls, verifierBlock), LogExtraInfo(invokedFuncName = Some(v.u.name), invArg = Some(invocationArgLet)))
  }

  def applyV2Coeval(
      ctx: EvaluationContext[Environment, Id],
      dApp: DApp,
      dAppAddress: ByteStr,
      i: Invocation,
      version: StdLibVersion,
      limit: Int,
      correctFunctionCallScope: Boolean,
      newMode: Boolean,
      enableExecutionLog: Boolean,
      fixedThrownError: Boolean
  ): Coeval[Either[(ExecutionError, Int, Log[Id]), (ScriptResult, Log[Id])]] =
    Coeval
      .now(buildExprFromInvocation(dApp, i, version).leftMap((_, limit, Nil)))
      .flatMap {
        case Right(value) =>
          applyV2Coeval(
            ctx,
            value.expr,
            LogExtraInfo(invokedFuncName = Some(i.funcCall.function.funcName), invArg = value.invArg, dAppAddress = Some(Address(dAppAddress))),
            version,
            i.transactionId,
            limit,
            correctFunctionCallScope,
            newMode,
            enableExecutionLog,
            fixedThrownError
          )
        case Left(error) => Coeval.now(Left(error))
      }

  private def applyV2Coeval(
      ctx: EvaluationContext[Environment, Id],
      expr: EXPR,
      logExtraInfo: LogExtraInfo,
      version: StdLibVersion,
      transactionId: ByteStr,
      limit: Int,
      correctFunctionCallScope: Boolean,
      newMode: Boolean,
      enableExecutionLog: Boolean,
      fixedThrownError: Boolean
  ): Coeval[Either[(ExecutionError, Int, Log[Id]), (ScriptResult, Log[Id])]] =
    EvaluatorV2
      .applyLimitedCoeval(
        expr,
        logExtraInfo,
        limit,
        ctx,
        version,
        correctFunctionCallScope,
        newMode,
        enableExecutionLog = enableExecutionLog,
        fixedThrownError = fixedThrownError
      )
      .map(_.flatMap { case (expr, unusedComplexity, log) =>
        val result =
          expr match {
            case value: EVALUATED => ScriptResult.fromObj(ctx, transactionId, value, version, unusedComplexity)
            case expr: EXPR       => Right(IncompleteResult(expr, unusedComplexity))
          }
        result.bimap((_, unusedComplexity, log), (_, log))
      })
}
