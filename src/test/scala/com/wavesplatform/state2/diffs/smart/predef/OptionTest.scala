package com.wavesplatform.state2.diffs.smart.predef

import com.wavesplatform.{NoShrink, TransactionGen}
import com.wavesplatform.lang.Evaluator
import com.wavesplatform.state2.diffs._
import com.wavesplatform.state2._
import com.wavesplatform.utils._
import fastparse.core.Parsed.Success
import monix.eval.Coeval
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import scodec.bits.ByteVector
import scorex.transaction.Transaction
import scorex.transaction.smart.BlockchainContext

class OptionTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  private val extractScript =
    """
      |
      | extract(tx.assetId)
      |
    """.stripMargin

  private val isDefinedScript =
    """
      |
      | isDefined(tx.assetId)
      |
    """.stripMargin

  private def runScript[T](script: String, tx: Transaction = null): Either[String, T] = {
    val Success(expr, _) = com.wavesplatform.lang.Parser(script)
    val Right(typedExpr) = com.wavesplatform.lang.TypeChecker(dummyTypeCheckerContext, expr)
    Evaluator[T](new BlockchainContext(Coeval(tx), Coeval(???), null).build(), typedExpr)
  }

  property("should extract transaction assetId if exists") {
    forAll(transferGen) {
      case (transfer) =>
        val result = runScript[ByteVector](extractScript, transfer)
        transfer.assetId match {
          case Some(v) => result.explicitGet().toArray sameElements v.arr
          case None    => result should produce("from empty option")
        }
    }
  }

  property("isDefined should return true if assetId exists") {
    forAll(transferGen) {
      case (transfer) =>
        val result = runScript[Boolean](isDefinedScript, transfer)
        transfer.assetId.isDefined shouldEqual result.right.get
    }
  }

  property("Some/None/extract/isDefined") {
    runScript("Some(3)".stripMargin) shouldBe Right(Some(3))
    runScript("None".stripMargin) shouldBe Right(None)
    runScript("isDefined(Some(3))".stripMargin) shouldBe Right(true)
    runScript("isDefined(None)".stripMargin) shouldBe Right(false)
    runScript("extract(Some(3))".stripMargin) shouldBe Right(3)
    runScript("extract(None)".stripMargin) should produce("Extract from empty option")


  }
}
