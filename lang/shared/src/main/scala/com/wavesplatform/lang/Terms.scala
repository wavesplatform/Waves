package com.wavesplatform.lang
import scodec.bits.ByteVector

object Terms {

  case class FUNCTION(args: List[TYPE], result: TYPE)

  sealed trait TYPE { type Underlying }
  case object NOTHING              extends TYPE { type Underlying = Nothing              }
  case object UNIT                 extends TYPE { type Underlying = Unit                 }
  case object INT                  extends TYPE { type Underlying = Int                  }
  case object BYTEVECTOR           extends TYPE { type Underlying = ByteVector           }
  case object BOOLEAN              extends TYPE { type Underlying = Boolean              }
  case class OPTION(t: TYPE)       extends TYPE { type Underlying = Option[t.Underlying] }
  case class TYPEREF(name: String) extends TYPE { type Underlying = Any                  }

  sealed trait BINARY_OP_KIND
  case object SUM_OP extends BINARY_OP_KIND
  case object AND_OP extends BINARY_OP_KIND
  case object OR_OP  extends BINARY_OP_KIND
  case object EQ_OP  extends BINARY_OP_KIND
  case object GT_OP  extends BINARY_OP_KIND
  case object GE_OP  extends BINARY_OP_KIND

  object Untyped {
    sealed trait EXPR
    case class CONST_INT(t: Int)                                     extends EXPR
    case class GETTER(ref: EXPR, field: String)                      extends EXPR
    case class CONST_BYTEVECTOR(bs: ByteVector)                      extends EXPR
    case class BINARY_OP(a: EXPR, kind: BINARY_OP_KIND, b: EXPR)     extends EXPR
    case class IS_DEFINED(opt: EXPR)                                 extends EXPR
    case class LET(name: String, value: EXPR)                        extends EXPR
    case class BLOCK(let: Option[LET], body: EXPR)                   extends EXPR
    case class IF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR)           extends EXPR
    case class REF(key: String)                                      extends EXPR
    case class GET(opt: EXPR)                                        extends EXPR
    case object TRUE                                                 extends EXPR
    case object FALSE                                                extends EXPR
    case object NONE                                                 extends EXPR
    case class SOME(t: EXPR)                                         extends EXPR
    case class FUNCTION_CALL(functionName: String, args: List[EXPR]) extends EXPR
  }

  object Typed {
    sealed abstract class EXPR(val tpe: TYPE)
    case class CONST_INT(t: Int)                                                             extends EXPR(INT)
    case class GETTER(ref: EXPR, field: String, override val tpe: TYPE)                      extends EXPR(tpe)
    case class CONST_BYTEVECTOR(bs: ByteVector)                                              extends EXPR(BYTEVECTOR)
    case class BINARY_OP(a: EXPR, kind: BINARY_OP_KIND, b: EXPR, override val tpe: TYPE)     extends EXPR(tpe)
    case class IS_DEFINED(opt: EXPR)                                                         extends EXPR(BOOLEAN)
    case class LET(name: String, value: EXPR)                                                extends EXPR(UNIT)
    case class BLOCK(let: Option[LET], body: EXPR, override val tpe: TYPE)                   extends EXPR(tpe)
    case class IF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR, override val tpe: TYPE)           extends EXPR(tpe)
    case class REF(key: String, override val tpe: TYPE)                                      extends EXPR(tpe)
    case class GET(opt: EXPR, override val tpe: TYPE)                                        extends EXPR(tpe)
    case object TRUE                                                                         extends EXPR(BOOLEAN)
    case object FALSE                                                                        extends EXPR(BOOLEAN)
    case object NONE                                                                         extends EXPR(OPTION(NOTHING))
    case class SOME(t: EXPR, override val tpe: TYPE)                                         extends EXPR(tpe)
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

  def inferTypeParams(actual: TYPE, expected: TYPE): List[(String, TYPE)] =
    (actual, expected) match {
      case (actualType, TYPEREF(name)) => List((name, actualType))
      case (OPTION(t1), OPTION(t2)) => inferTypeParams(t1, t2)
      case _ => List.empty
    }
}
