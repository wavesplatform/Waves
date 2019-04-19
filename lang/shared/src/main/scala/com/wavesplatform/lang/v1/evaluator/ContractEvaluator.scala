package com.wavesplatform.lang.v1.evaluator
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.VerifierFunction
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Bindings
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, LoggedEvaluationContext}
import com.wavesplatform.lang.v1.task.imports.raiseError
import com.wavesplatform.lang.v1.traits.domain.Tx.{Pmt, ScriptTransfer}
import com.wavesplatform.lang.v1.traits.domain.{Ord, Recipient, Tx}
import cats.implicits._

object ContractEvaluator {
  case class Invocation(fc: FUNCTION_CALL,
                        caller: Recipient.Address,
                        callerPk: ByteStr,
                        payment: Option[(Long, Option[ByteStr])],
                        dappAddress: ByteStr)

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
        withDecls(
          c.dec,
          BLOCK(
            LET(
              f.annotation.invocationArgName,
              Bindings
                .buildInvocation(i.caller, i.callerPk, i.payment.map { case (a, t) => Pmt(t, a) }, Recipient.Address(i.dappAddress))
            ),
            BLOCK(f.u, i.fc)
          )
        )
    }
  }

  private def withDecls(dec: List[DECLARATION], block: BLOCK): EvalM[EVALUATED] =
    EvaluatorV1.evalExpr(dec.foldRight(block)((d, e) => BLOCK(d, e)))

  private def verifierBlock(v: VerifierFunction, entity: CaseObj) =
    BLOCK(LET(v.annotation.invocationArgName, entity), BLOCK(v.u, FUNCTION_CALL(FunctionHeader.User(v.u.name), List(entity))))

  def verify(decls: List[DECLARATION], v: VerifierFunction, tx: Tx): EvalM[EVALUATED] =
    withDecls(decls, verifierBlock(v, Bindings.transactionObject(tx, proofsEnabled = true)))

  def verify(decls: List[DECLARATION], v: VerifierFunction, ord: Ord): EvalM[EVALUATED] =
    withDecls(decls, verifierBlock(v, Bindings.orderObject(ord, proofsEnabled = true)))

  def verify(decls: List[DECLARATION], v: VerifierFunction, ct: ScriptTransfer): EvalM[EVALUATED] =
    withDecls(decls, verifierBlock(v, Bindings.scriptTransfer(ct)))

  def apply(ctx: EvaluationContext, c: DApp, i: Invocation): Either[(ExecutionError, Log), ScriptResult] = {
    val (log, result) = EvaluatorV1.evalWithLogging(ctx, eval(c, i))
    result
      .flatMap(ScriptResult.fromObj)
      .leftMap((_, log))
  }
}
