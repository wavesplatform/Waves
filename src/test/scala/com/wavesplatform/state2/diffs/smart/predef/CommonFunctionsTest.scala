package com.wavesplatform.state2.diffs.smart.predef

import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector

class CommonFunctionsTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("extract should transaction assetId if exists") {
    forAll(transferGen) {
      case (transfer) =>
        val result = runScript[ByteVector]("extract(tx.assetId)", transfer)
        transfer.assetId match {
          case Some(v) => result.explicitGet().toArray sameElements v.arr
          case None    => result should produce("from empty option")
        }
    }
  }

  property("isDefined should return true if assetId exists") {
    forAll(transferGen) {
      case (transfer) =>
        val result = runScript[Boolean]("isDefined(tx.assetId)", transfer)
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

  property("size") {
    runScript("size(base58'')".stripMargin) shouldBe Right(0L)
    val arr = Array(1: Byte, 2: Byte, 3: Byte)
    runScript(s"size(base58'${ByteStr(arr).base58}')".stripMargin) shouldBe Right(3L)
  }
}
