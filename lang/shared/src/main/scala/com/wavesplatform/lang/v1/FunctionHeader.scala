package com.wavesplatform.lang.v1

import scala.collection.mutable

sealed abstract class FunctionHeader(val funcName: String)
object FunctionHeader {
  case class Native private(name: Short) extends FunctionHeader(name.toString)
  object Native {
    private[this] val cache = mutable.Map.empty[Short, Native]
    def apply(name: Short): Native = cache.getOrElse(name, cache.synchronized {
      cache.getOrElseUpdate(name, new Native(name))
    })
  }

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
}
