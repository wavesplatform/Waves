package com.wavesplatform.api.http

import akka.http.scaladsl.server.{Directive1, MissingQueryParamRejection}
import play.api.libs.json.{JsArray, Json}

trait AutoParamsDirective { self: ApiRoute =>
  def paramList(name: String): Directive1[Vector[String]] = {
    import scala.concurrent.duration._

    val fromQuery = parameterSeq.map(_.filter(_._1 == name).map(_._2))
    val fromPost  = formFieldSeq.map(_.filter(_._1 == name).map(_._2))
    val fromJson = extractStrictEntity(5 seconds)
      .map(entity => Json.parse(entity.toString()).as[JsArray].value.map(_.as[String]))
      .recover(_ => provide(Seq.empty[String]))

    (for {
      q <- fromQuery
      p <- fromPost
      j <- fromJson
    } yield (q ++ p ++ j).toVector).filter(_.nonEmpty, MissingQueryParamRejection(name))
  }
}
