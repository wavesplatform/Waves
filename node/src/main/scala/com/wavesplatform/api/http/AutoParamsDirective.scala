package com.wavesplatform.api.http

import akka.http.scaladsl.server.{Directive1, MalformedRequestContentRejection, RequestEntityExpectedRejection}
import scalapb.json4s.JsonFormat
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

import scala.util.Try

trait AutoParamsDirective { self: ApiRoute =>
  def protobufEntity[T <: GeneratedMessage with Message[T]: GeneratedMessageCompanion]: Directive1[T] = {
    def paramsJson(params: Seq[(String, String)]): String = {
      import play.api.libs.json.{JsArray, JsObject, JsString, Json}
      val map   = collection.mutable.Map.empty[String, String]
      val lists = collection.mutable.Map.empty[String, Seq[String]]
      params.foreach {
        case (name, value) =>
          lists.get(name) match {
            case Some(list) =>
              lists(name) = list :+ value

            case None =>
              map.get(name) match {
                case Some(prev) =>
                  map -= name
                  lists(name) = Seq(prev, value)

                case None =>
                  map(name) = value
              }
          }
      }
      Json.stringify(JsObject(map.map(v => v._1 -> JsString(v._2)) ++ lists.map(v => s"${v._1}s" -> JsArray(v._2.map(JsString)))))
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
