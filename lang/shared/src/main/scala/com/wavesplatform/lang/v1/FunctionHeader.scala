package com.wavesplatform.lang.v1

sealed trait FunctionHeader
object FunctionHeader {
  case class Predef(name: Short) extends FunctionHeader
  case class User(name: String)  extends FunctionHeader
}
