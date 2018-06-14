package com.wavesplatform.lang.v1

// TODO: Deny create Predef fh in User
sealed trait FunctionHeader
object FunctionHeader {
  case class Predef(name: Short) extends FunctionHeader
  case class User(name: String)  extends FunctionHeader
}
