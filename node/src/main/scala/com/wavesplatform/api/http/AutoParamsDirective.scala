package com.wavesplatform.api.http

import akka.http.scaladsl.server.{Directive1, MalformedRequestContentRejection, RequestEntityExpectedRejection}
import scalapb.json4s.JsonFormat
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

import scala.util.Try

trait AutoParamsDirective { self: ApiRoute =>
  def protobufEntity[T <: GeneratedMessage with Message[T]: GeneratedMessageCompanion]: Directive1[T] = {
    def paramsJson(params: Seq[(String, String)]): String = {
      import play.api.libs.json.{JsArray, JsObject, JsString, Json}
      val lists = params.groupBy(_._1).mapValues(_.map(_._2))
      val values = lists.collect { case (k, Seq(v)) => (k, v) }
      Json.stringify(JsObject(values.map(v => v._1 -> JsString(v._2)) ++ lists.map(v => s"${v._1}s" -> JsArray(v._2.map(JsString)))))
    }

    def tryFromJson(d: Directive1[String]): Directive1[T] =
      d.filter(v => !Set("", "[]", "{}").contains(v), RequestEntityExpectedRejection)
        .map(json => Try(JsonFormat.fromJsonString[T](json)))
        .filter(_.isSuccess, MalformedRequestContentRejection("Couldn't parse values", new IllegalArgumentException("Couldn't parse values")))
        .map(_.get)

    val fromQuery = tryFromJson(parameterSeq.map(paramsJson))
    val fromForm  = tryFromJson(formFieldSeq.map(paramsJson))
    val fromJson  = tryFromJson(entity(as[String]))

    fromQuery.recover(_ => fromForm).recover(_ => fromJson)
  }
}
