package com.wavesplatform.transaction

import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.TxNonNegativeAmount.reads
import play.api.libs.json.{JsError, JsSuccess, Json}

class TxNonNegativeAmountTest extends FreeSpec {
  "TxNonNegativeAmount JSON deserialization" - {
    "positive cases" in forAll(
      Table[String, Long](
        ("rawJson", "expected"),
        ("0", 0),
        ("1", 1),
        (Long.MaxValue.toString, Long.MaxValue),
        (s""""${Long.MaxValue}"""", Long.MaxValue) // As JsString
      )
    ) { (rawJson, expected) =>
      Json.parse(rawJson).validate[TxNonNegativeAmount] shouldBe JsSuccess(TxNonNegativeAmount.unsafeFrom(expected))
    }

    "negative cases" in forAll(
      Table(
        "rawJson",
        "-1",
        "1.1",
        "-1.1",
        s""""-1"""",
        s""""1.1"""",
        s""""x""""
      )
    ) { rawJson =>
      Json.parse(rawJson).validate[TxNonNegativeAmount] shouldBe a[JsError]
    }
  }
}
