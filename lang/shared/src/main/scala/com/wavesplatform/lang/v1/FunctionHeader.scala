package com.wavesplatform.lang.v1

sealed abstract class FunctionHeader(val funcName: String)
object FunctionHeader {
  case class Native(name: Short) extends FunctionHeader(name.toString) {
    // The case class hash, computed once: headers key the function maps every call looks up. Same value, so map order is unchanged.
    override val hashCode: Int = scala.util.hashing.MurmurHash3.caseClassHash(this)
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
