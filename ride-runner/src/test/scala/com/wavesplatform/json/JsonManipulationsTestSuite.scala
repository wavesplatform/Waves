package com.wavesplatform.json

import cats.syntax.option.*
import com.wavesplatform.BaseTestSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.libs.json.{JsNumber, JsObject, Json}

class JsonManipulationsTestSuite extends BaseTestSuite with TableDrivenPropertyChecks {
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
    "pick" in {
      val table = Table(
        "path"        -> "expectedJson",
        "foo.bar.baz" -> JsNumber(10).some,
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
        JsonManipulations.pick(json, path.split('.').toList) shouldBe expectedJson
      }
    }

    "mkTrunk" in {
      JsonManipulations.mkTrunk("foo.bar.baz", JsNumber(10)) shouldBe Json.parse(
        """{
          |  "foo": {
          |    "bar": {
          |      "baz": 10
          |    }
          |  }
          |}""".stripMargin
      )

      JsonManipulations.mkTrunk("", JsNumber(10)) shouldBe JsNumber(10)
    }

    "prune" in {
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
        JsonManipulations.prune(json, path.split('.').toList) shouldBe expectedJson
      }
    }
  }
}
