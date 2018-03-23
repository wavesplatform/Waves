package com.wavesplatform.state2.diffs.smart

import com.wavesplatform.lang.Evaluator
import com.wavesplatform.utils.dummyTypeCheckerContext
import fastparse.core.Parsed.Success
import monix.eval.Coeval
import scorex.transaction.Transaction
import scorex.transaction.smart.BlockchainContext
import scala.reflect.runtime.universe.TypeTag
package object predef {
  val networkByte: Byte = 'u'
  def runScript[T: TypeTag](script: String, tx: Transaction = null): Either[String, T] = {
    val Success(expr, _) = com.wavesplatform.lang.Parser(script)
    val Right(typedExpr) = com.wavesplatform.lang.TypeChecker(dummyTypeCheckerContext, expr)
    Evaluator[T](new BlockchainContext(networkByte, Coeval(tx), Coeval(???), null).build(), typedExpr)
  }
}
