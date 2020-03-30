package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import monix.eval.Coeval
import cats.implicits._
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.ContractLimits._

object Terms {
  private val DATA_TX_BYTES_MAX = 150 * 1024 // should be the same as DataTransaction.MAX_BYTES

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
    def weight: Long
  }

  case class CONST_LONG(t: Long) extends EVALUATED {
    override def toString: String = t.toString
    override val weight: Long = 8l
  }

  case class CONST_BYTESTR private (bs: ByteStr) extends EVALUATED {
    override def toString: String = bs.toString
    override def prettyString(level: Int) : String = {
      if(bs.size > 1024) {
        "base64'" ++ Base64.encode(bs) ++ "'"
      } else {
        "base58'" ++ Base58.encode(bs) ++ "'"
      }
    }
    override val weight: Long = bs.size
  }
  object CONST_BYTESTR {
    def apply(bs: ByteStr): Either[ExecutionError, CONST_BYTESTR] =
      Either.cond(
        bs.size <= DATA_TX_BYTES_MAX,
        new CONST_BYTESTR(bs),
        s"ByteStr exceeds $DATA_TX_BYTES_MAX bytes"
      )
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
  case class CONST_STRING private (s: String)    extends EVALUATED {
    override def toString: String = s
    override def prettyString(level: Int) : String = "\"" ++ escape(s) ++ "\""
    override val weight: Long = s.getBytes.size
  }
  object CONST_STRING {
    def apply(s: String): Either[ExecutionError, CONST_STRING] =
      Either.cond(
        s.getBytes("UTF-8").length <= DATA_TX_BYTES_MAX,
        new CONST_STRING(s),
        s"String exceeds $DATA_TX_BYTES_MAX bytes"
      )
  }

  case class CONST_BOOLEAN(b: Boolean)  extends EVALUATED {
    override def toString: String = b.toString
    override val weight: Long = 1l
  }

  lazy val TRUE  = CONST_BOOLEAN(true)
  lazy val FALSE = CONST_BOOLEAN(false)

  case class CaseObj private (caseType: CASETYPEREF, fields: Map[String, EVALUATED]) extends EVALUATED {
    override def toString: String = TermPrinter.string(this)

    override def prettyString(depth: Int): String = TermPrinter.indentObjString(this, depth)

    override val weight: Long = OBJ_WEIGHT + FIELD_WEIGHT * fields.size + fields.map(_._2.weight).sum
  }

  object CaseObj {
    def apply(caseType: CASETYPEREF, fields: Map[String, EVALUATED]): CaseObj = {
      val obj = new CaseObj(caseType, fields)
      if (obj.weight > MaxWeight) {
        throw new Exception(s"the object ${caseType.name} is too heavy. Actual weight: ${obj.weight}, limit: ${MaxWeight}")
      } else {
        obj
      }
    }
  }

  abstract case class ARR(xs: IndexedSeq[EVALUATED]) extends EVALUATED {
    override def toString: String = TermPrinter.string(this)
  }

  object ARR {
    def apply(xs: IndexedSeq[EVALUATED], mweight: Long): Either[ExecutionError, ARR] =
      if (mweight > MaxWeight) {
        Left(s"the list is too heavy. Actual weight: ${mweight}, limit: ${MaxWeight}")
      } else {
        Right(new ARR(xs) { override val weight: Long = mweight })
      }
 
    def apply(xs: IndexedSeq[EVALUATED]): Either[ExecutionError, ARR] = {
      val weight =  EMPTYARR_WEIGHT + ELEM_WEIGHT * xs.size + xs.map(_.weight).sum
      ARR(xs, weight)
    }
  }
}
