package com.wavesplatform.lang.v1.evaluator
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.VerifierFunction
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Bindings
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, LoggedEvaluationContext}
import com.wavesplatform.lang.v1.task.imports.{raiseError, _}
import com.wavesplatform.lang.v1.traits.domain.Tx.{ScriptTransfer, Pmt}
import com.wavesplatform.lang.v1.traits.domain.{Ord, Recipient, Tx}

object ContractEvaluator {
  case class Invocation(fc: FUNCTION_CALL, caller: Recipient.Address, callerPk: ByteStr, payment: Option[(Long, Option[ByteStr])], dappAddress: ByteStr)

  def eval(c: DApp, i: Invocation): EvalM[EVALUATED] = {
    val functionName = i.fc.function.asInstanceOf[FunctionHeader.User].name
    c.cfs.find(_.u.name == functionName) match {
      case None =>
        val otherFuncs = c.dec.filter(_.isInstanceOf[FUNC]).map(_.asInstanceOf[FUNC].name)
        val message =
          if (otherFuncs contains functionName)
            s"function '$functionName exists in the script but is not marked as @Callable, therefore cannot not be invoked"
          else s"@Callable function '$functionName doesn't exist in the script"
        raiseError[LoggedEvaluationContext, ExecutionError, EVALUATED](message)
      case Some(f) =>
        val zeroExpr = Right(
          BLOCK(
            LET(
              f.annotation.invocationArgName,
              Bindings
                .buildInvocation(i.caller, i.callerPk, i.payment.map { case (a, t) => Pmt(t, a) }, Recipient.Address(i.dappAddress))
            ),
            BLOCK(f.u, i.fc)
          ))

        for {
          ze <- liftEither(zeroExpr)
          expr = c.dec.foldRight(ze)((d, e) => BLOCK(d, e))
          r <- EvaluatorV1.evalExpr(expr)
        } yield r
    }
  }

  def verify(v: VerifierFunction, tx: Tx): EvalM[EVALUATED] = {
    val t = Bindings.transactionObject(tx, proofsEnabled = true)
    val expr =
      BLOCK(LET(v.annotation.invocationArgName, t), BLOCK(v.u, FUNCTION_CALL(FunctionHeader.User(v.u.name), List(t))))
    EvaluatorV1.evalExpr(expr)
  }

  def verify(v: VerifierFunction, ord: Ord): EvalM[EVALUATED] = {
    val t = Bindings.orderObject(ord, proofsEnabled = true)
    val expr =
      BLOCK(LET(v.annotation.invocationArgName, t), BLOCK(v.u, FUNCTION_CALL(FunctionHeader.User(v.u.name), List(t))))
    EvaluatorV1.evalExpr(expr)
  }

  def verify(v: VerifierFunction, ct: ScriptTransfer): EvalM[EVALUATED] = {
    val t = Bindings.scriptTransfer(ct)
    val expr =
      BLOCK(LET(v.annotation.invocationArgName, t), BLOCK(v.u, FUNCTION_CALL(FunctionHeader.User(v.u.name), List(t))))
    EvaluatorV1.evalExpr(expr)
  }

  def apply(ctx: EvaluationContext, c: DApp, i: Invocation): Either[ExecutionError, ScriptResult] =
    EvaluatorV1.evalWithLogging(ctx, eval(c, i))._2.flatMap(ScriptResult.fromObj)
}
