package com.wavesplatform.lang.v1

import com.wavesplatform.lang.v1
import com.wavesplatform.lang.v1.FunctionHeader.FunctionHeaderType

case class FunctionHeader(name: String, args: List[FunctionHeaderType])
object FunctionHeader {
  sealed trait FunctionHeaderType
  object FunctionHeaderType {
    def fromTypePlaceholder(t: Terms.TYPEPLACEHOLDER): FunctionHeaderType = t match {
      case Terms.TYPEPARAM(char)       => FunctionHeaderType.TYPEPARAM(char)
      case Terms.OPTIONTYPEPARAM(x)    => FunctionHeaderType.OPTIONTYPEPARAM(fromTypePlaceholder(x))
      case v1.Terms.NOTHING            => FunctionHeaderType.NOTHING
      case v1.Terms.UNIT               => FunctionHeaderType.UNIT
      case v1.Terms.LONG               => FunctionHeaderType.LONG
      case v1.Terms.BYTEVECTOR         => FunctionHeaderType.BYTEVECTOR
      case v1.Terms.BOOLEAN            => FunctionHeaderType.BOOLEAN
      case v1.Terms.STRING             => FunctionHeaderType.STRING
      case Terms.OPTION(x)             => FunctionHeaderType.OPTION(fromTypePlaceholder(x))
      case Terms.TYPEREF(name: String) => FunctionHeaderType.TYPEREF(name)
      case Terms.LIST(x)             => FunctionHeaderType.LIST(fromTypePlaceholder(x))
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
    case class LIST(t: FunctionHeaderType)          extends FunctionHeaderType
    case class TYPEREF(name: String)                  extends FunctionHeaderType
  }
}
