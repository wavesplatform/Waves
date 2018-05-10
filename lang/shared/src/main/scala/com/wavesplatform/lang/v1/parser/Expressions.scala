package com.wavesplatform.lang.v1.parser

import scodec.bits.ByteVector

object Expressions {

  case class LET(name: String, value: EXPR)
  sealed trait EXPR
  case class CONST_LONG(value: Long)                               extends EXPR
  case class GETTER(ref: EXPR, field: String)                      extends EXPR
  case class CONST_BYTEVECTOR(value: ByteVector)                   extends EXPR
  case class CONST_STRING(value: String)                           extends EXPR
  case class BINARY_OP(a: EXPR, kind: BinaryOperation, b: EXPR)    extends EXPR
  case class BLOCK(let: LET, body: EXPR)                           extends EXPR
  case class IF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR)           extends EXPR
  case class REF(key: String)                                      extends EXPR
  case object TRUE                                                 extends EXPR
  case object FALSE                                                extends EXPR
  case class FUNCTION_CALL(functionName: String, args: List[EXPR]) extends EXPR
  case class MATCH_CASE(types: Seq[String], expr: EXPR)
  case class MATCH(expr: EXPR, cases: Seq[MATCH_CASE]) extends EXPR

}
