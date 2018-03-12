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

  property("should extract transaction assetId if exists") {

    forAll(transferGen) {
      case (transfer) =>
        val Success(expr, index) = com.wavesplatform.lang.Parser(extractScript)
        val Right(typedExpr)     = com.wavesplatform.lang.TypeChecker(dummyTypeCheckerContext, expr)
        val result               = Evaluator[ByteVector](new ConsensusContext(Coeval(transfer), Coeval(???), null).build(), typedExpr)

        if (transfer.assetId.isDefined) {
          result.explicitGet().toArray sameElements transfer.assetId.get.arr
        } else {
          result should produce("from empty option")
      }
    }

  }
}
