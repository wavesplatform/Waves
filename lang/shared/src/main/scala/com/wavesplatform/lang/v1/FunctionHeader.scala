package com.wavesplatform.lang.v1

sealed abstract class FunctionHeader(val funcName: String)
object FunctionHeader {
  case class Native(name: Short) extends FunctionHeader(name.toString)
  case class User(internalName: String, name: String)  extends FunctionHeader(internalName)
  object User {
    def apply(internalName: String): User = User(internalName, internalName)
  }
}
