package com.wavesplatform.lang.v1.repl.model

import io.circe.{Decoder, HCursor}

case class FunctionCall(function: String, args: List[CallArg])

sealed trait CallArg
case class Binary(value: String) extends CallArg
case class Bool(value: Boolean)  extends CallArg
case class Numeric(value: Long)  extends CallArg
case class Str(value: String)    extends CallArg

object CallArg {
  implicit val decoder: Decoder[CallArg] = (c: HCursor) => {
    val value = c.downField("value")
    for {
      t <- c.downField("type").as[String]
      r <- t match {
        case "integer" => value.as[Long].map(Numeric)
        case "boolean" => value.as[Boolean].map(Bool)
        case "binary"  => value.as[String].map(Binary)
        case "string"  => value.as[String].map(Str)
      }
    } yield r
  }
}
