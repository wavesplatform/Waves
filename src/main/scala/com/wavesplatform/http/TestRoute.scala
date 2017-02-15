package com.wavesplatform.http

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import PlayJsonSupport._
import play.api.libs.json.{Format, Json}

object TestRoute {
  case class Foo(bar: String, baz: Int)

  implicit val foo: Format[Foo] = Json.format

  val r: Route = path("test-route") { post {
    entity(as[Foo]) { f =>
      complete(f.bar)
    }
  } }
}
