package com.wavesplatform.lang
import com.wavesplatform.lang.ctx.Obj
import scodec.bits.ByteVector

import scala.reflect.runtime.universe._

object Terms {

  case class FUNCTION(args: List[TYPEPLACEHOLDER], result: TYPEPLACEHOLDER)

  sealed trait TYPEPLACEHOLDER
  case class TYPEPARAM(char: Char)               extends TYPEPLACEHOLDER
  case class OPTIONTYPEPARAM(t: TYPEPLACEHOLDER) extends TYPEPLACEHOLDER

  sealed trait TYPE extends TYPEPLACEHOLDER {
    type Underlying
    def typetag: TypeTag[Underlying]
  }
  sealed abstract class AUTO_TAGGED_TYPE[T](implicit override val typetag: TypeTag[T]) extends TYPE {
    override type Underlying = T
  }
  case object NOTHING    extends AUTO_TAGGED_TYPE[Nothing]
  case object UNIT       extends AUTO_TAGGED_TYPE[Unit]
  case object LONG       extends AUTO_TAGGED_TYPE[Long]
  case object BYTEVECTOR extends AUTO_TAGGED_TYPE[ByteVector]
  case object BOOLEAN    extends AUTO_TAGGED_TYPE[Boolean]
  case object STRING     extends AUTO_TAGGED_TYPE[String]
  case class OPTION(innerType: TYPE) extends TYPE {
    type Underlying = Option[innerType.Underlying]
    override def typetag: TypeTag[Option[innerType.Underlying]] = typeTag[Underlying]
  }
  case class TYPEREF(name: String) extends AUTO_TAGGED_TYPE[Obj]

  sealed trait BINARY_OP_KIND { val symbol: String }
  case object SUM_OP extends BINARY_OP_KIND {
    override val symbol: String = "+"
  }
  case object AND_OP extends BINARY_OP_KIND {
    override val symbol: String = "&&"
  }
  case object OR_OP extends BINARY_OP_KIND {
    override val symbol: String = "||"
  }
  case object EQ_OP extends BINARY_OP_KIND {
    override val symbol: String = "=="
  }
  case object GT_OP extends BINARY_OP_KIND {
    override val symbol: String = ">"
  }
  case object GE_OP extends BINARY_OP_KIND {
    override val symbol: String = ">="
  }

  object Untyped {
    case class LET(name: String, value: EXPR)
    sealed trait EXPR
    case class CONST_LONG(value: Long)                               extends EXPR
    case class GETTER(ref: EXPR, field: String)                      extends EXPR
    case class CONST_BYTEVECTOR(value: ByteVector)                   extends EXPR
    case class CONST_STRING(value: String)                           extends EXPR
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
    case class CONST_STRING(s: String)                                                       extends EXPR(STRING)
    case class BINARY_OP(a: EXPR, kind: BINARY_OP_KIND, b: EXPR, override val tpe: TYPE)     extends EXPR(tpe)
    case class BLOCK(let: Option[LET], body: EXPR, override val tpe: TYPE)                   extends EXPR(tpe)
    case class IF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR, override val tpe: TYPE)           extends EXPR(tpe)
    case class REF(key: String, override val tpe: TYPE)                                      extends EXPR(tpe)
    case object TRUE                                                                         extends EXPR(BOOLEAN)
    case object FALSE                                                                        extends EXPR(BOOLEAN)
    case class FUNCTION_CALL(functionHeader: FunctionHeader, args: List[EXPR], override val tpe: TYPE) extends EXPR(tpe)
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
