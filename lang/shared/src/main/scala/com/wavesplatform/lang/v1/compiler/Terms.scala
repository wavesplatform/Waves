package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.TypeInfo
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.evaluator.ctx.{CaseObj, Obj}
import scodec.bits.ByteVector

object Terms {

  sealed trait TYPEPLACEHOLDER
  case class TYPEPARAM(char: Byte)               extends TYPEPLACEHOLDER
  case class OPTIONTYPEPARAM(t: TYPEPLACEHOLDER) extends TYPEPLACEHOLDER

  sealed trait TYPE extends TYPEPLACEHOLDER {
    type Underlying
    def typeInfo: TypeInfo[Underlying]
  }
  sealed abstract class AUTO_TAGGED_TYPE[T](implicit override val typeInfo: TypeInfo[T]) extends TYPE {
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
    override def typeInfo: TypeInfo[Option[innerType.Underlying]] = TypeInfo.optionTypeInfo(innerType.typeInfo)
  }
  case class TYPEREF(name: String)     extends AUTO_TAGGED_TYPE[Obj]
  case class CASETYPEREF(name: String) extends AUTO_TAGGED_TYPE[CaseObj]

  sealed abstract class EXPR(val tpe: TYPE)
  case class LET(name: String, value: EXPR)
  case class CONST_LONG(t: Long)                                                               extends EXPR(LONG)
  case class GETTER(ref: EXPR, field: String, override val tpe: TYPE)                          extends EXPR(tpe)
  case class CONST_BYTEVECTOR(bs: ByteVector)                                                  extends EXPR(BYTEVECTOR)
  case class CONST_STRING(s: String)                                                           extends EXPR(STRING)
  case class BLOCK(let: LET, body: EXPR, override val tpe: TYPE)                               extends EXPR(tpe)
  case class IF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR, override val tpe: TYPE)               extends EXPR(tpe)
  case class REF(key: String, override val tpe: TYPE)                                          extends EXPR(tpe)
  case object TRUE                                                                             extends EXPR(BOOLEAN)
  case object FALSE                                                                            extends EXPR(BOOLEAN)
  case class FUNCTION_CALL(function: FunctionHeader, args: List[EXPR], override val tpe: TYPE) extends EXPR(tpe)

}
