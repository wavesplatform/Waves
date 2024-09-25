package com.wavesplatform.lang.evaluator

import cats.Id
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.utils
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.{CASETYPEREF, FINAL}
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.FieldNames
import com.wavesplatform.lang.v1.evaluator.{ScriptResult, ScriptResultV3}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain.{AssetTransfer, DataItem}
import com.wavesplatform.test.*

class ScriptResultTest extends PropSpec {

  val pureEvalContext: EvaluationContext[Environment, Id] =
    PureContext.build(V3, useNewPowPrecision = true).withEnvironment[Environment].evaluationContext(utils.environment)

  val el       = List.empty[(String, FINAL)]
  val address1 = ByteStr.fromBytes(19: Byte)
  val address2 = ByteStr.fromBytes(20: Byte)
  val asset    = ByteStr.fromBytes(21: Byte)
  val noAsset  = com.wavesplatform.lang.v1.evaluator.ctx.impl.unit

  val writeSetObj = CaseObj(
    CASETYPEREF("WriteSet", el),
    Map(
      "data" -> ARR(
        IndexedSeq(
          CaseObj(
            CASETYPEREF("DataEntry", el),
            Map(
              "key"   -> CONST_STRING("xxx").explicitGet(),
              "value" -> CONST_LONG(42)
            )
          ): EVALUATED
        ),
        false
      ).explicitGet()
    )
  )

  val transferSetObj = CaseObj(
    CASETYPEREF("TransferSet", el),
    Map(
      "transfers" -> ARR(
        IndexedSeq(
          CaseObj(
            CASETYPEREF("ScriptTransfer", el),
            Map(
              "recipient" -> CaseObj(CASETYPEREF("Address", el), Map("bytes" -> CONST_BYTESTR(address1).explicitGet())),
              "amount"    -> CONST_LONG(41),
              "asset"     -> CONST_BYTESTR(asset).explicitGet()
            )
          ),
          CaseObj(
            CASETYPEREF("ScriptTransfer", el),
            Map(
              "recipient" -> CaseObj(CASETYPEREF("Address", el), Map("bytes" -> CONST_BYTESTR(address2).explicitGet())),
              "amount"    -> CONST_LONG(42),
              "asset"     -> noAsset
            )
          )
        ),
        false
      ).explicitGet()
    )
  )

  val scriptResultObj =
    CaseObj(CASETYPEREF("ScriptResult", el), Map(FieldNames.ScriptWriteSet -> writeSetObj, FieldNames.ScriptTransferSet -> transferSetObj))

  val writeResult = List(DataItem.Lng("xxx", 42))
  val transferResult =
    List(AssetTransfer(Address(address1), Address(address1), 41L, Some(asset)), AssetTransfer(Address(address2), Address(address2), 42L, None))

  property("ScriptResult from WriteSet") {
    ScriptResult.fromObj(pureEvalContext, asset, writeSetObj, V3, 0) shouldBe Right(ScriptResultV3(writeResult, List.empty, 0))
  }

  property("ScriptResult from TransferSet") {
    ScriptResult.fromObj(pureEvalContext, asset, transferSetObj, V3, 0) shouldBe Right(ScriptResultV3(List.empty, transferResult, 0))
  }

  property("ScriptResult from ScriptResult") {
    ScriptResult.fromObj(pureEvalContext, asset, scriptResultObj, V3, 0) shouldBe
      Right(ScriptResultV3(writeResult, transferResult, 0))
  }

  property("ScriptResult from bad object") {
    ScriptResult.fromObj(pureEvalContext, asset, CaseObj(CASETYPEREF("Foo", el), Map.empty), V3, 0) should produce("CallableFunction needs to return")
  }
}
