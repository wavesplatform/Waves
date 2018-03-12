package com.wavesplatform.state2.diffs.smart.predef

import com.wavesplatform.{NoShrink, TransactionGen}
import com.wavesplatform.lang.Evaluator
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import com.wavesplatform.state2.diffs.smart.dummyTypeCheckerContext
import fastparse.core.Parsed.Success
import monix.eval.Coeval
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import scodec.bits.ByteVector
import scorex.transaction.Transaction
import scorex.transaction.smart.ConsensusContext

class OptionTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  private val extractScript =
    """
      |
      | EXTRACT(TX.ASSETID)
      |
    """.stripMargin

  private val isDefinedScript =
    """
      |
      | ISDEFINED(TX.ASSETID)
      |
    """.stripMargin

  private def runScript[T](script: String, tx: Transaction): Either[String, T] = {
    val Success(expr, index) = com.wavesplatform.lang.Parser(script)
    val Right(typedExpr)     = com.wavesplatform.lang.TypeChecker(dummyTypeCheckerContext, expr)
    Evaluator[T](new ConsensusContext(Coeval(tx), Coeval(???), null).build(), typedExpr)
  }

  property("should extract transaction assetId if exists") {
    forAll(transferGen) {
      case (transfer) =>
        val result = runScript[ByteVector](extractScript, transfer)
        transfer.assetId match {
          case Some(v) => v.arr sameElements transfer.assetId.get.arr
          case None    => result should produce("from empty option")
        }
    }
  }
}
