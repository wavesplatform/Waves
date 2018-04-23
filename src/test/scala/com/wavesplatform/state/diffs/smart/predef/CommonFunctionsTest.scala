package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.lang.TypeInfo._
import com.wavesplatform.state._
import com.wavesplatform.state.diffs._
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
    runScript[Any]("Some(3)".stripMargin) shouldBe Right(Some(3L))
    runScript[Any]("None".stripMargin) shouldBe Right(None)
    runScript[Boolean]("isDefined(Some(3))".stripMargin) shouldBe Right(true)
    runScript[Boolean]("isDefined(None)".stripMargin) shouldBe Right(false)
    runScript[Long]("extract(Some(3))".stripMargin) shouldBe Right(3L)
    runScript[Long]("extract(None)".stripMargin) should produce("Extract from empty option")
  }

  property("size") {
    val arr = Array(1: Byte, 2: Byte, 3: Byte)
    runScript[Long]("size(base58'')".stripMargin) shouldBe Right(0L)
    runScript[Long](s"size(base58'${ByteStr(arr).base58}')".stripMargin) shouldBe Right(3L)
  }
}
