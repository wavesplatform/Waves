package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF

import com.wavesplatform.lang.v1.FunctionHeader
import scodec.bits.ByteVector

object
Terms {
  sealed abstract class EXPR
  sealed abstract class DECLARATION
  sealed trait EVALUATED

  case class LET(name: String, value: EXPR)                     extends DECLARATION
  case class FUNC(name: String, args: List[String], body: EXPR) extends DECLARATION
  case class CONST_LONG(t: Long)                                extends EXPR with EVALUATED
  case class GETTER(expr: EXPR, field: String)                  extends EXPR
  case class CONST_BYTEVECTOR(bs: ByteVector)                   extends EXPR with EVALUATED
  case class CONST_STRING(s: String)                            extends EXPR with EVALUATED
  case class BLOCK(let: LET, body: EXPR)                        extends EXPR
  case class IF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR)        extends EXPR
  case class REF(key: String)                                   extends EXPR

  case class CONST_BOOLEAN(b: Boolean) extends EXPR with EVALUATED {
    override def toString(): String = if (b) "TRUE" else "FALSE"
  }
  val TRUE  = CONST_BOOLEAN(true)
  val FALSE = CONST_BOOLEAN(false)

  case class FUNCTION_CALL(function: FunctionHeader, args: List[EXPR]) extends EXPR

case class CaseObj(caseType: CASETYPEREF, fields: Map[String, EVALUATED]) extends EVALUATED{
  override def toString: String = {
    s"""
       |${caseType.name} {
       |  ${fields.map({ case (k, v) => s"$k -> $v" }).mkString(", ")}
       |}
     """.stripMargin
  }
}

case class ARR(xs: IndexedSeq[EVALUATED]) extends EVALUATED

}
