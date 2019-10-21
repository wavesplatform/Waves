package com.wavesplatform.api.http

import akka.http.scaladsl.server.{Directive1, MissingQueryParamRejection}
import play.api.libs.json.{JsArray, Json}

import scala.util.Try

trait AutoParamsDirective { self: ApiRoute =>
  def paramList(name: String): Directive1[Seq[String]] = {
    import scala.concurrent.duration._

    def fromParamSeq(parameterSeq: Directive1[collection.immutable.Seq[(String, String)]]) =
      parameterSeq
        .map(_.filter(_._1 == name).map(_._2))
        .recover(_ => provide(Seq.empty[String]))

    val fromQuery = fromParamSeq(parameterSeq)
    val fromPost  = fromParamSeq(formFieldSeq)
    val fromJson = extractStrictEntity(5 seconds)
      .map(entity => Try(Json.parse(entity.data.toArray).as[JsArray].value.map(_.as[String])).getOrElse(Nil))
      .recover(_ => provide(Seq.empty[String]))

    (for {
      q <- fromQuery
      p <- fromPost
      j <- fromJson
    } yield q ++ p ++ j).filter(_.nonEmpty, MissingQueryParamRejection(name))
  }
}
