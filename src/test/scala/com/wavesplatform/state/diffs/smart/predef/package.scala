package com.wavesplatform.state.diffs.smart

import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.utils.dummyTypeCheckerContext
import fastparse.core.Parsed.Success
import monix.eval.Coeval
import scorex.transaction.Transaction
import scorex.transaction.smart.BlockchainContext

package object predef {
  val networkByte: Byte = 'u'

  def runScript[T](script: String, tx: Transaction = null): Either[String, T] = {
    val Success(expr, _) = Parser(script)
    if (expr.size == 1) {
      val Right(typedExpr) = CompilerV1(dummyTypeCheckerContext, expr.head)
      val r                = EvaluatorV1[T](BlockchainContext.build(networkByte, Coeval(tx), Coeval(???), null), typedExpr)
      r._2
    } else {
      Left(expr.mkString("\n"))
    }
  }

}
