package com.wavesplatform.lang.v1

sealed trait FunctionHeader
object FunctionHeader {
  case class Native(name: Short) extends FunctionHeader
  case class User(name: String)  extends FunctionHeader
}
