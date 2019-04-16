package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext

object Terms {
  val decompilerContext = PureContext.build(V3).decompilerContext

  sealed abstract class DECLARATION
  case class LET(name: String, value: EXPR)                     extends DECLARATION
  case class FUNC(name: String, args: List[String], body: EXPR) extends DECLARATION

  sealed abstract class EXPR
  case class GETTER(expr: EXPR, field: String)                         extends EXPR
  @Deprecated
  case class LET_BLOCK(let: LET, body: EXPR)                           extends EXPR
  case class BLOCK(dec: DECLARATION, body: EXPR)                       extends EXPR
  case class IF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR)               extends EXPR
  case class REF(key: String)                                          extends EXPR
  case class FUNCTION_CALL(function: FunctionHeader, args: List[EXPR]) extends EXPR

  sealed trait EVALUATED extends EXPR
  case class CONST_LONG(t: Long)        extends EVALUATED { override def toString: String = t.toString                 }
  case class CONST_BYTESTR(bs: ByteStr) extends EVALUATED { override def toString: String = bs.toString                }
  case class CONST_STRING(s: String)    extends EVALUATED { override def toString: String = s                          }
  case class CONST_BOOLEAN(b: Boolean)  extends EVALUATED { override def toString: String = if (b) "TRUE" else "FALSE" }

  lazy val TRUE  = CONST_BOOLEAN(true)
  lazy val FALSE = CONST_BOOLEAN(false)

  case class CaseObj(caseType: CASETYPEREF, fields: Map[String, EVALUATED]) extends EVALUATED {
    override def toString: String = prettyString()

    def prettyString(depth: Int = 0): String =
      if (fields.isEmpty) caseType.name
      else {
        def text(v: EVALUATED) = {
          v match {
            case co: CaseObj => co.prettyString(depth + 1)
            case a           => a.toString
          }
        }
        val parenthesisIndent = "\t" * depth
        val fieldsIndent      = "\t" * (depth + 1)

        val fieldsText = fields
          .map { case (name, value) => s"$fieldsIndent$name = ${text(value)}" }
          .mkString("(\n", "\n", s"\n$parenthesisIndent)")

        caseType.name + fieldsText
      }
  }

  case class ARR(xs: IndexedSeq[EVALUATED]) extends EVALUATED {
    override def toString: String = xs.mkString("[", ", ", "]")
  }
}
