package com.wavesplatform.lang

import com.wavesplatform.lang.FunctionHeader.FunctionHeaderType

case class FunctionHeader(name: String, args: List[FunctionHeaderType])
object FunctionHeader {
  sealed trait FunctionHeaderType
  object FunctionHeaderType {
    def fromTypePlaceholder(t: Terms.TYPEPLACEHOLDER): FunctionHeaderType = t match {
      case com.wavesplatform.lang.Terms.TYPEPARAM(char)       => FunctionHeaderType.TYPEPARAM(char)
      case com.wavesplatform.lang.Terms.OPTIONTYPEPARAM(x)    => FunctionHeaderType.OPTIONTYPEPARAM(fromTypePlaceholder(x))
      case com.wavesplatform.lang.Terms.NOTHING               => FunctionHeaderType.NOTHING
      case com.wavesplatform.lang.Terms.UNIT                  => FunctionHeaderType.UNIT
      case com.wavesplatform.lang.Terms.LONG                  => FunctionHeaderType.LONG
      case com.wavesplatform.lang.Terms.BYTEVECTOR            => FunctionHeaderType.BYTEVECTOR
      case com.wavesplatform.lang.Terms.BOOLEAN               => FunctionHeaderType.BOOLEAN
      case com.wavesplatform.lang.Terms.STRING                => FunctionHeaderType.STRING
      case com.wavesplatform.lang.Terms.OPTION(x)             => FunctionHeaderType.OPTION(fromTypePlaceholder(x))
      case com.wavesplatform.lang.Terms.TYPEREF(name: String) => FunctionHeaderType.TYPEREF(name)
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
