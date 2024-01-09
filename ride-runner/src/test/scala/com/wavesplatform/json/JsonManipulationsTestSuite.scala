package com.wavesplatform.json

import cats.syntax.either.*
import cats.syntax.option.*
import com.wavesplatform.BaseTestSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.libs.json.{JsArray, JsNumber, JsObject, Json}

class JsonManipulationsTestSuite extends BaseTestSuite with TableDrivenPropertyChecks {
  private val ten = JsNumber(10)
  private val json = Json.parse(
    """{
      |  "extra1": {},
      |  "foo": {
      |    "extra1": {},
      |    "bar": {
      |      "extra1": {},
      |      "baz": 10,
      |      "extra2": {}
      |    },
      |    "extra2": "foo12bar"
      |  },
      |  "extra2": 1
      |}""".stripMargin
  )

  "JsonManipulations" - {
    "regexReplace" - {
      val regexExampleJson = Json.parse(
        """{
          |  "foo": {
          |    "bar": " foo 12b ar",
          |    "baz": 10
          |  },
          |  "bar": "111"
          |}""".stripMargin
      )

      "on object" in {
        val table = Table(
          "path" -> "expectedJson",
          "foo.bar" -> Json.parse(
            """{
              |  "foo": {
              |    "bar": "12",
              |    "baz": 10
              |  },
              |  "bar": "111"
              |}""".stripMargin
          ),
          "bar.baz" -> regexExampleJson
        )

        forAll(table) { (path, expectedJson) =>
          JsonManipulations.regexReplace(regexExampleJson, path, """^.+foo.+?(\d+).+$""", "$1") shouldBe expectedJson.asRight
        }
      }

      "on value" in {
        val table = Table(
          ("json", "path", "expectedError"),
          (ten, "foo.bar", "expected an object"),
          (regexExampleJson, "foo.baz", "Expected a string at path"),
          (regexExampleJson, "", "Expected a non-empty path")
        )

        forAll(table) { (json, path, expectedError) =>
          JsonManipulations.regexReplace(json, path, ".+", "1") match {
            case Right(x) => fail(s"Expected an error, but got success: $x")
            case Left(e)  => e should include regex expectedError
          }
        }
      }
    }

    "pruneAll" in {
      val table = Table(
        "paths" -> "expectedJson",
        List("foo.bar.baz", "extra1") -> Json.parse(
          """{
            |  "extra1": {},
            |  "foo": {
            |    "bar": {
            |      "baz": 10
            |    }
            |  }
            |}""".stripMargin
        ),
        List("", "foo.bar.baz") -> Json.parse(
          """{
            |  "foo": {
            |    "bar": {
            |      "baz": 10
            |    }
            |  }
            |}""".stripMargin
        ),
        List("") -> JsObject.empty,
        Nil      -> JsObject.empty
      )

      forAll(table) { (paths, expectedJson) =>
        JsonManipulations.pruneAll(json, paths) shouldBe expectedJson
      }
    }

    "prune" - {
      "on object" in {
        val table = Table(
          "path" -> "expectedJson",
          "foo.bar.baz" -> Json.parse(
            """{
              |  "foo": {
              |    "bar": {
              |      "baz": 10
              |    }
              |  }
              |}""".stripMargin
          ),
          "foo.bar" -> Json.parse(
            """{
              |  "foo": {
              |    "bar": {
              |      "extra1": {},
              |      "baz": 10,
              |      "extra2": {}
              |    }
              |  }
              |}""".stripMargin
          ),
          ""        -> JsObject.empty,
          "bar.baz" -> JsObject.empty,
          "foo.baz" -> JsObject.empty
        )

        forAll(table) { (path, expectedJson) =>
          JsonManipulations.prune(json, path) shouldBe expectedJson
        }
      }

      "on value" - {
        "empty path" in { JsonManipulations.prune(ten, "") shouldBe JsObject.empty }
        "non-empty path" in { JsonManipulations.prune(ten, "foo") shouldBe JsObject.empty }
      }
    }

    "pickAll" - {
      "on object" in {
        val table = Table(
          "path"                        -> "expectedJson",
          List("foo.bar.baz")           -> JsArray(Seq(ten)),
          List("extra2", "foo.bar.baz") -> JsArray(Seq(JsNumber(1), ten)),
          List.empty                    -> JsArray.empty,
          List("")                      -> JsArray.empty,
          List("", "foo.bar.baz")       -> JsArray(Seq(ten))
        )

        forAll(table) { (paths, expectedJson) =>
          JsonManipulations.pickAll(json, paths) shouldBe expectedJson
        }
      }

      "on value" in {
        JsonManipulations.pickAll(ten, List("foo")) shouldBe JsArray.empty
        JsonManipulations.pickAll(ten, List.empty) shouldBe JsArray.empty
      }
    }

    "pick" - {
      "on object" in {
        val table = Table(
          "path"        -> "expectedJson",
          "foo.bar.baz" -> ten.some,
          "foo.bar" -> Json
            .parse(
              """{
                |  "extra1": {},
                |  "baz": 10,
                |  "extra2": {}
                |}""".stripMargin
            )
            .some,
          ""        -> none,
          "bar.baz" -> none,
          "foo.baz" -> none
        )

        forAll(table) { (path, expectedJson) =>
          JsonManipulations.pick(json, path) shouldBe expectedJson
        }
      }

      "on value" in {
        JsonManipulations.pick(ten, "foo") shouldBe None
      }
    }

    "mkTrunk" in {
      JsonManipulations.mkTrunk("foo.bar.baz", ten) shouldBe Json.parse(
        """{
          |  "foo": {
          |    "bar": {
          |      "baz": 10
          |    }
          |  }
          |}""".stripMargin
      )

      JsonManipulations.mkTrunk("", ten) shouldBe ten
    }
  }
}
