package com.wavesplatform.lang.v1.evaluator
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.state.diffs.ProduceError._
import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.{CASETYPEREF, FINAL}
import com.wavesplatform.lang.v1.traits.domain.DataItem
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ScriptResultTest extends PropSpec with PropertyChecks with Matchers with NoShrink {

  val el       = List.empty[(String, FINAL)]
  val address1 = ByteStr.fromBytes(19: Byte)
  val address2 = ByteStr.fromBytes(20: Byte)
  val asset    = ByteStr.fromBytes(21: Byte)
  val noAsset  = com.wavesplatform.lang.v1.evaluator.ctx.impl.unit

  val writeSetObj = CaseObj(
    CASETYPEREF("WriteSet", el),
    Map("data" -> ARR(IndexedSeq(CaseObj(CASETYPEREF("DataEntry", el), Map("key" -> CONST_STRING("xxx"), "value" -> CONST_LONG(42)))))))

  val transferSetObj = CaseObj(
    CASETYPEREF("TransferSet", el),
    Map(
      "transfers" -> ARR(IndexedSeq(
        CaseObj(
          CASETYPEREF("ScriptTransfer", el),
          Map(
            "recipient" -> CaseObj(CASETYPEREF("Address", el), Map("bytes" -> CONST_BYTESTR(address1))),
            "amount"    -> CONST_LONG(41),
            "asset"     -> CONST_BYTESTR(asset)
          )
        ),
        CaseObj(
          CASETYPEREF("ScriptTransfer", el),
          Map(
            "recipient" -> CaseObj(CASETYPEREF("Address", el), Map("bytes" -> CONST_BYTESTR(address2))),
            "amount"    -> CONST_LONG(42),
            "asset"     -> noAsset
          )
        )
      )))
  )

  val scriptResultObj = CaseObj(CASETYPEREF("ScriptResult", el), Map("data" -> writeSetObj, "transfers" -> transferSetObj))

  val writeResult    = List(DataItem.Lng("xxx", 42))
  val transferResult = List((Address(address1), 41L, Some(asset)), (Address(address2), 42L, None))

  property("ScriptResult from WriteSet") {
    ScriptResult.fromObj(writeSetObj) shouldBe Right(ScriptResult(writeResult, List.empty))
  }

  property("ScriptResult from TransferSet") {
    ScriptResult.fromObj(transferSetObj) shouldBe Right(ScriptResult(List.empty, transferResult))
  }

  property("ScriptResult from ScriptResult") {
    ScriptResult.fromObj(scriptResultObj) shouldBe
      Right(ScriptResult(writeResult, transferResult))
  }

  property("ScriptResult from bad object") {
    ScriptResult.fromObj(CaseObj(CASETYPEREF("Foo", el), Map.empty)) should produce("CallableFunction needs to return")
  }
}
