package com.wavesplatform.lang
import com.wavesplatform.lang.ctx.Obj
import scodec.bits.ByteVector

object Terms {

  case class FUNCTION(args: List[TYPEPLACEHOLDER], result: TYPEPLACEHOLDER)

  sealed trait TYPEPLACEHOLDER
  case class TYPEPARAM(char: Char)               extends TYPEPLACEHOLDER
  case class OPTIONTYPEPARAM(t: TYPEPLACEHOLDER) extends TYPEPLACEHOLDER

  sealed trait TYPE                  extends TYPEPLACEHOLDER { type Underlying }
  case object NOTHING                extends TYPE            { type Underlying = Nothing }
  case object UNIT                   extends TYPE            { type Underlying = Unit }
  case object LONG                   extends TYPE            { type Underlying = Long }
  case object BYTEVECTOR             extends TYPE            { type Underlying = ByteVector }
  case object BOOLEAN                extends TYPE            { type Underlying = Boolean }
  case class OPTION(innerType: TYPE) extends TYPE            { type Underlying = Option[innerType.Underlying] }
  case class TYPEREF(name: String)   extends TYPE            { type Underlying = Obj }

  sealed trait BINARY_OP_KIND
  case object SUM_OP extends BINARY_OP_KIND
  case object AND_OP extends BINARY_OP_KIND
  case object OR_OP  extends BINARY_OP_KIND
  case object EQ_OP  extends BINARY_OP_KIND
  case object GT_OP  extends BINARY_OP_KIND
  case object GE_OP  extends BINARY_OP_KIND

  object Untyped {
    case class LET(name: String, value: EXPR)
    sealed trait EXPR
    case class CONST_LONG(t: Long)                                   extends EXPR
    case class GETTER(ref: EXPR, field: String)                      extends EXPR
    case class CONST_BYTEVECTOR(bs: ByteVector)                      extends EXPR
    case class BINARY_OP(a: EXPR, kind: BINARY_OP_KIND, b: EXPR)     extends EXPR
    case class BLOCK(let: Option[LET], body: EXPR)                   extends EXPR
    case class IF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR)           extends EXPR
    case class REF(key: String)                                      extends EXPR
    case object TRUE                                                 extends EXPR
    case object FALSE                                                extends EXPR
    case class FUNCTION_CALL(functionName: String, args: List[EXPR]) extends EXPR
  }

  object Typed {
    case class LET(name: String, value: EXPR)
    sealed abstract class EXPR(val tpe: TYPE)
    case class CONST_LONG(t: Long)                                                           extends EXPR(LONG)
    case class GETTER(ref: EXPR, field: String, override val tpe: TYPE)                      extends EXPR(tpe)
    case class CONST_BYTEVECTOR(bs: ByteVector)                                              extends EXPR(BYTEVECTOR)
    case class BINARY_OP(a: EXPR, kind: BINARY_OP_KIND, b: EXPR, override val tpe: TYPE)     extends EXPR(tpe)
    case class BLOCK(let: Option[LET], body: EXPR, override val tpe: TYPE)                   extends EXPR(tpe)
    case class IF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR, override val tpe: TYPE)           extends EXPR(tpe)
    case class REF(key: String, override val tpe: TYPE)                                      extends EXPR(tpe)
    case object TRUE                                                                         extends EXPR(BOOLEAN)
    case object FALSE                                                                        extends EXPR(BOOLEAN)
    case class FUNCTION_CALL(functionName: String, args: List[EXPR], override val tpe: TYPE) extends EXPR(tpe)
  }

  def findCommonType(t1: TYPE, t2: TYPE): Option[TYPE] = findCommonType(t1, t2, biDirectional = true)

  def matchType(required: TYPE, actual: TYPE): Option[TYPE] = findCommonType(required, actual, biDirectional = false)

  private def findCommonType(required: TYPE, actual: TYPE, biDirectional: Boolean): Option[TYPE] =
    if (actual == NOTHING) Some(required)
    else if (required == NOTHING && biDirectional) Some(actual)
    else if (required == actual) Some(required)
    else
      (required, actual) match {
        case (OPTION(it1), OPTION(it2)) => findCommonType(it1, it2, biDirectional).map(OPTION)
        case _                          => None
      }
}
