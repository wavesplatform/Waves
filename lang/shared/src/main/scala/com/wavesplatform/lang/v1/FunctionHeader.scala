package com.wavesplatform.lang.v1
import com.wavesplatform.lang.v1.evaluator.FunctionIds

sealed abstract class FunctionHeader(val funcName: String)
object FunctionHeader {
  case class Native(name: Short) extends FunctionHeader(name.toString)

  case class User(internalName: String, name: String) extends FunctionHeader(internalName) {
    override def hashCode(): Int = internalName.##
    override def equals(obj: Any): Boolean =
      obj match {
        case User(`internalName`, _) => true
        case _                       => false
      }

    override def toString: String = s"User($internalName)"
  }
  object User {
    def apply(internalName: String): User = User(internalName, internalName)
  }

  implicit class FunctionHeaderExt(f: FunctionHeader) {
    val isExternal: Boolean = f match {
      case Native(name) => FunctionIds.ExternalFunctionIds.contains(name)
      case User(_, _)   => false
    }
  }
}
