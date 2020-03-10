package com.wavesplatform.lang.v1.compiler

import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.MaxListLengthV4
import monix.eval.Coeval

object Terms {
  private val DATA_TX_BYTES_MAX = 150 * 1024 // should be the same as DataTransaction.MAX_BYTES

  sealed abstract class DECLARATION {
    def name: String
    def toStr: Coeval[String]
    def deepCopy: DECLARATION
    override def toString: String = toStr()
    def isItFailed: Boolean = false
  }
  case class FAILED_DEC() extends DECLARATION {
    def name = "NO_NAME"
    def toStr: Coeval[String] = Coeval.now("Error")
    override def isItFailed: Boolean = true
    override def deepCopy: DECLARATION = this
  }
  case class LET(name: String, var value: EXPR)                     extends DECLARATION {
    def toStr: Coeval[String] = for {
      e <- value.toStr
    } yield "LET(" ++ name.toString ++ "," ++ e ++ ")"

    def deepCopy: LET =
      LET(name, value.deepCopy)
  }
  case class FUNC(name: String, args: List[String], var body: EXPR) extends DECLARATION {
    def toStr: Coeval[String] = for {
      e <- body.toStr
    } yield "FUNC(" ++ name.toString ++ "," ++ args.toString ++ "," ++ e ++ ")"

    def deepCopy: FUNC =
      FUNC(name, args, body.deepCopy)
  }

  sealed abstract class EXPR {
    def toStr: Coeval[String]
    def deepCopy: EXPR
    override def toString: String = toStr()
    def isItFailed: Boolean = false
  }

  case class FAILED_EXPR() extends EXPR {
    def toStr: Coeval[String] = Coeval.now("error")
    override def isItFailed: Boolean = true
    override def deepCopy: EXPR = this
  }

  case class GETTER(var expr: EXPR, field: String)                         extends EXPR {
    def toStr: Coeval[String] = for {
      e <- expr.toStr
    } yield "GETTER(" ++ e ++ "," ++ field.toString ++ ")"

    override def deepCopy: EXPR =
      GETTER(expr.deepCopy, field)
  }

  sealed trait BLOCK_DEF {
    val dec: DECLARATION
    var body: EXPR
  }

  @Deprecated
  case class LET_BLOCK(let: LET, var body: EXPR)                           extends EXPR with BLOCK_DEF {
    def toStr: Coeval[String] = for {
      e <- let.toStr
      b <- body.toStr
    } yield "LET_BLOCK(" ++ e ++ "," ++ b ++ ")"

    override val dec: DECLARATION = let

    override def deepCopy: EXPR =
      LET_BLOCK(let.deepCopy, body.deepCopy)
  }

  case class BLOCK(dec: DECLARATION, var body: EXPR)                       extends EXPR with BLOCK_DEF {
    def toStr: Coeval[String] = for {
      e <- dec.toStr
      b <- body.toStr
    } yield "BLOCK(" ++ e ++ "," ++ b ++ ")"

    override def deepCopy: EXPR =
      BLOCK(dec.deepCopy, body.deepCopy)
  }
  case class IF(var cond: EXPR, ifTrue: EXPR, ifFalse: EXPR)               extends EXPR {
    def toStr: Coeval[String] = for {
      c <- cond.toStr
      t <- ifTrue.toStr
      f <- ifFalse.toStr
    } yield "IF(" ++ c ++ "," ++ t ++ "," ++ f ++ ")"

    override def deepCopy: EXPR =
      IF(cond.deepCopy, ifTrue.deepCopy, ifFalse.deepCopy)
  }
  case class REF(key: String)                                          extends EXPR {
    override def toString: String = "REF(" ++ key.toString ++ ")"
    def toStr: Coeval[String] = Coeval.now(toString)

    override def deepCopy: EXPR =
      this
  }
  case class FUNCTION_CALL(function: FunctionHeader, var args: List[EXPR]) extends EXPR {
    def toStr: Coeval[String] = for {
      e <- args.map(_.toStr).sequence
    } yield "FUNCTION_CALL(" ++ function.toString ++ "," ++ e.toString ++ ")"

    override def deepCopy: EXPR =
      FUNCTION_CALL(function, args.map(_.deepCopy))
  }

  sealed trait EVALUATED extends EXPR {
    def prettyString(level: Int) : String = toString
    def toStr: Coeval[String] = Coeval.now(toString)

    override def deepCopy: EXPR =
      this
  }
  case class CONST_LONG(t: Long)        extends EVALUATED { override def toString: String = t.toString  }

  case class CONST_BYTESTR private (bs: ByteStr) extends EVALUATED {
    override def toString: String = bs.toString
    override def prettyString(level: Int) : String = {
      if(bs.size > 1024) {
        "base64'" ++ Base64.encode(bs) ++ "'"
      } else {
        "base58'" ++ Base58.encode(bs) ++ "'"
      }
    }

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
  }
  object CONST_STRING {
    def apply(s: String): Either[ExecutionError, CONST_STRING] =
      Either.cond(
        s.getBytes("UTF-8").length <= DATA_TX_BYTES_MAX,
        new CONST_STRING(s),
        s"String exceeds $DATA_TX_BYTES_MAX bytes"
      )
  }

  case class CONST_BOOLEAN(b: Boolean)  extends EVALUATED { override def toString: String = b.toString }

  lazy val TRUE  = CONST_BOOLEAN(true)
  lazy val FALSE = CONST_BOOLEAN(false)

  case class CaseObj(caseType: CASETYPEREF, fields: Map[String, EVALUATED]) extends EVALUATED {
    override def toString: String = TermPrinter.string(this)

    override def prettyString(depth: Int): String = TermPrinter.indentObjString(this, depth)
  }

  case class ARR private (xs: IndexedSeq[EVALUATED]) extends EVALUATED {
    override def toString: String = TermPrinter.string(this)
  }
  object ARR {
    def apply(xs: IndexedSeq[EVALUATED], limited: Boolean = false): Either[ExecutionError, ARR] =
      if (limited && xs.size > MaxListLengthV4)
        Left(s"List size should not exceed $MaxListLengthV4")
      else
        Right(new ARR(xs))
  }
}
