package com.wavesplatform.state.diffs.smart

import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.utils.dummyCompilerContext
import fastparse.core.Parsed.Success
import monix.eval.Coeval
import scorex.transaction.Transaction
import scorex.transaction.smart.BlockchainContext

package object predef {
  val networkByte: Byte = 'u'

  def runScript[T](script: String, tx: Transaction = null, networkByte: Byte = networkByte): Either[String, T] = {
    val Success(expr, _) = Parser(script)
    for {
      _             <- Either.cond(expr.size == 1, (), expr.mkString("\n"))
      compileResult <- CompilerV1(dummyCompilerContext, expr.head)
      (typedExpr, tpe) = compileResult
      r <- EvaluatorV1[T](BlockchainContext.build(networkByte, Coeval(tx), Coeval(???), null), typedExpr)._2
    } yield r
  }

}
