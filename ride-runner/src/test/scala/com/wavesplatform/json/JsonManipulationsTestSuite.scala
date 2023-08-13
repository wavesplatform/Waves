package com.wavesplatform.json

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
      |    "extra2": {}
      |  },
      |  "extra2": 1
      |}""".stripMargin
  )

  "JsonManipulations" - {
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
