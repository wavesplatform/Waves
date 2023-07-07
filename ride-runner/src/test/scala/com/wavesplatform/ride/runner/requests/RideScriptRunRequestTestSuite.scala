package com.wavesplatform.ride.runner.requests

import com.wavesplatform.{BaseTestSuite, HasTestAccounts}
import play.api.libs.json.{JsSuccess, Json}

class RideScriptRunRequestTestSuite extends BaseTestSuite with HasTestAccounts {
  private val expected = RideScriptRunRequest(
    address = aliceAddr,
    requestBody = Json.obj("foo" -> 1),
    trace = true,
    intAsString = true
  )

  "RideScriptRunRequest" - {
    "Reads" - {
      "Tuple format" in {
        withClue(expected.detailedLogPrefix) {
          Json.parse(s"""["$aliceAddr", { "foo": 1 }]""").validate[RideScriptRunRequest] shouldBe JsSuccess(
            expected.copy(
              trace = false,
              intAsString = false
            )
          )
        }
      }

      "Object format" - {
        "full" in {
          withClue(expected.detailedLogPrefix) {
            Json
              .parse(s"""{
                        | "address": "$aliceAddr",
                        | "requestBody": { "foo": 1 },
                        | "trace": true,
                        | "intAsString": true
                        |}""".stripMargin)
              .validate[RideScriptRunRequest] shouldBe JsSuccess(expected)
          }
        }

        "without trace" in {
          withClue(expected.detailedLogPrefix) {
            Json
              .parse(s"""{
                        | "address": "$aliceAddr",
                        | "requestBody": { "foo": 1 },
                        | "intAsString": true
                        |}""".stripMargin)
              .validate[RideScriptRunRequest] shouldBe JsSuccess(expected.copy(trace = false))
          }
        }

        "without intAsString" in {
          withClue(expected.detailedLogPrefix) {
            Json
              .parse(s"""{
                        | "address": "$aliceAddr",
                        | "requestBody": { "foo": 1 },
                        | "trace": true
                        |}""".stripMargin)
              .validate[RideScriptRunRequest] shouldBe JsSuccess(expected.copy(intAsString = false))
          }
        }
      }
    }
  }
}
