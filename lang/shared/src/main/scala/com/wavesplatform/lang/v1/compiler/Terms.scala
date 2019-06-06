package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF

import monix.eval.Coeval
import cats.implicits._

object Terms {
  sealed abstract class DECLARATION {
    def toStr: Coeval[String]
    override def toString: String = toStr()
  }
  case class LET(name: String, value: EXPR)                     extends DECLARATION {
    def toStr: Coeval[String] = for {
      e <- value.toStr
    } yield "LET(" ++ name.toString ++ "," ++ e ++ ")"
  }
  case class FUNC(name: String, args: List[String], body: EXPR) extends DECLARATION {
    def toStr: Coeval[String] = for {
      e <- body.toStr
    } yield "FUNC(" ++ name.toString ++ "," ++ args.toString ++ "," ++ e ++ ")"
  }

  sealed abstract class EXPR {
    def toStr: Coeval[String]
    override def toString: String = toStr()
  }
  case class GETTER(expr: EXPR, field: String)                         extends EXPR {
    def toStr: Coeval[String] = for {
      e <- expr.toStr
    } yield "GETTER(" ++ e ++ "," ++ field.toString ++ ")"
  }
  @Deprecated
  case class LET_BLOCK(let: LET, body: EXPR)                           extends EXPR {
    def toStr: Coeval[String] = for {
      e <- let.toStr
      b <- body.toStr
    } yield "LET_BLOCK(" ++ e ++ "," ++ b ++ ")"
  }
  case class BLOCK(dec: DECLARATION, body: EXPR)                       extends EXPR {
    def toStr: Coeval[String] = for {
      e <- dec.toStr
      b <- body.toStr
    } yield "BLOCK(" ++ e ++ "," ++ b ++ ")"
  }
  case class IF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR)               extends EXPR {
    def toStr: Coeval[String] = for {
      c <- cond.toStr
      t <- ifTrue.toStr
      f <- ifFalse.toStr
    } yield "IF(" ++ c ++ "," ++ t ++ "," ++ f ++ ")"
  }
  case class REF(key: String)                                          extends EXPR {
    override def toString: String = "REF(" ++ key.toString ++ ")"
    def toStr: Coeval[String] = Coeval.now(toString)
  }
  case class FUNCTION_CALL(function: FunctionHeader, args: List[EXPR]) extends EXPR {
    def toStr: Coeval[String] = for {
      e <- args.map(_.toStr).sequence
    } yield "FUNCTION_CALL(" ++ function.toString ++ "," ++ e.toString ++ ")"
  }

  sealed trait EVALUATED extends EXPR {
    def prettyString(level: Int) : String = toString
    def toStr: Coeval[String] = Coeval.now(toString)
  }
  case class CONST_LONG(t: Long)        extends EVALUATED { override def toString: String = t.toString                 }
  case class CONST_BYTESTR(bs: ByteStr) extends EVALUATED {
    import com.wavesplatform.common.utils.{Base58, Base64}
    override def toString: String = bs.toString
    override def prettyString(level: Int) : String = {
      if(bs.size > 1024) {
        "base64'" ++ Base64.encode(bs) ++ "'"
      } else {
        "base58'" ++ Base58.encode(bs) ++ "'"
      }
    }

  }

  def escape(s: String): String = {
    // Simple and very naive implementation based on
    // https://github.com/linkedin/dustjs/blob/3fc12efd153433a21fd79ac81e8c5f5d6f273a1c/dist/dust-core.js#L1099

    // Note this might not be the most efficient since Scala.js compiles this to a bunch of .split and .join calls
    s.replace("\\", "\\\\")
     .replace("/", "\\/")
     .replace("'", "\\'")
     .replace("\"", "\\\"")
     .replace("\n", "\\n")
     .replace("\r", "\\r")
     .replace("\t", "\\t")
     .replace("\b", "\\b")
     .replace("\f", "\\f")
     .replace("\u2028", "\\u2028")
     .replace("\u2029", "\\u2029")
  }
  case class CONST_STRING(s: String)    extends EVALUATED {
    override def toString: String = s
    override def prettyString(level: Int) : String = "\"" ++ escape(s) ++ "\""
  }

  case class CONST_BOOLEAN(b: Boolean)  extends EVALUATED { override def toString: String = if (b) "TRUE" else "FALSE" }

  lazy val TRUE  = CONST_BOOLEAN(true)
  lazy val FALSE = CONST_BOOLEAN(false)

  case class CaseObj(caseType: CASETYPEREF, fields: Map[String, EVALUATED]) extends EVALUATED {
    override def toString: String = TermPrinter.string(this)

    override def prettyString(depth: Int): String = TermPrinter.indentObjString(this, depth)
  }

  case class ARR(xs: IndexedSeq[EVALUATED]) extends EVALUATED {
    override def toString: String = TermPrinter.string(this)
  }
}
