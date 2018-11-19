package com.wavesplatform.lang.v1.evaluator
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, LoggedEvaluationContext}
import com.wavesplatform.lang.v1.task.imports.raiseError
import scodec.bits.ByteVector

object ContractEvaluator {
  case class Invokation(func: String, args: List[EVALUATED], invoker: ByteVector)

  def eval(ctx: EvaluationContext, c: Contract, i: Invokation): EvalM[EVALUATED] = {
    c.cfs.find(_.u.name == i.func) match {
      case None => raiseError[LoggedEvaluationContext, ExecutionError, EVALUATED](s"Callable function '${i.func} doesn't exist in the contract")
      case Some(f) =>
        val expr =
          BLOCKV2(
            LET(f.c.pubKeyArgName, CONST_BYTEVECTOR(i.invoker)),
            BLOCKV2(f.u, FUNCTION_CALL(FunctionHeader.User(f.u.name), i.args))
          )
        EvaluatorV1.evalExpr(expr)
    }
  }

}
