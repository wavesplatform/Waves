package com.wavesplatform.lang.v1

import com.wavesplatform.lang.v1.FunctionHeader.FunctionHeaderType
import com.wavesplatform.lang.v1.compiler.Terms

case class FunctionHeader(name: String, args: List[FunctionHeaderType])
object FunctionHeader {
  sealed trait FunctionHeaderType
  object FunctionHeaderType {
    def fromTypePlaceholder(t: Terms.TYPEPLACEHOLDER): FunctionHeaderType = t match {
      case Terms.TYPEPARAM(char)       => FunctionHeaderType.TYPEPARAM(char)
      case Terms.OPTIONTYPEPARAM(x)    => FunctionHeaderType.OPTIONTYPEPARAM(fromTypePlaceholder(x))
      case Terms.NOTHING               => FunctionHeaderType.NOTHING
      case compiler.Terms.UNIT         => FunctionHeaderType.UNIT
      case compiler.Terms.LONG         => FunctionHeaderType.LONG
      case compiler.Terms.BYTEVECTOR   => FunctionHeaderType.BYTEVECTOR
      case compiler.Terms.BOOLEAN      => FunctionHeaderType.BOOLEAN
      case compiler.Terms.STRING       => FunctionHeaderType.STRING
      case Terms.OPTION(x)             => FunctionHeaderType.OPTION(fromTypePlaceholder(x))
      case Terms.TYPEREF(name: String) => FunctionHeaderType.TYPEREF(name)
    }

    case class TYPEPARAM(char: Byte)                  extends FunctionHeaderType
    case class OPTIONTYPEPARAM(t: FunctionHeaderType) extends FunctionHeaderType
    case object NOTHING                               extends FunctionHeaderType
    case object UNIT                                  extends FunctionHeaderType
    case object LONG                                  extends FunctionHeaderType
    case object BYTEVECTOR                            extends FunctionHeaderType
    case object BOOLEAN                               extends FunctionHeaderType
    case object STRING                                extends FunctionHeaderType
    case class OPTION(t: FunctionHeaderType)          extends FunctionHeaderType
    case class TYPEREF(name: String)                  extends FunctionHeaderType
  }
}
