package com.wavesplatform.lang.v1.compiler

import cats.Eval
import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.ContractLimits._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.MaxListLengthV4
import monix.eval.Coeval

object Terms {
  val DATA_TX_BYTES_MAX: Int = 150 * 1024 // should be the same as DataTransaction.MAX_BYTES

  sealed abstract class DECLARATION {
    def name: String
    def toStr: Coeval[String]
    def deepCopy: Eval[DECLARATION]
    override def toString: String = toStr()
    def isItFailed: Boolean       = false
  }
  case class FAILED_DEC() extends DECLARATION {
    def name                         = "NO_NAME"
    def toStr: Coeval[String]        = Coeval.now("Error")
    override def isItFailed: Boolean = true

    override def deepCopy: Eval[DECLARATION] =
      Eval.now(this)
  }
  case class LET(name: String, var value: EXPR) extends DECLARATION {
    def toStr: Coeval[String] =
      for {
        e <- value.toStr
      } yield "LET(" ++ name ++ "," ++ e ++ ")"

    override def deepCopy: Eval[LET] =
      value.deepCopy.map(LET(name, _))
  }
  case class FUNC(name: String, args: List[String], body: EXPR) extends DECLARATION {
    def toStr: Coeval[String] =
      for {
        e <- body.toStr
      } yield "FUNC(" ++ name ++ "," ++ args.toString ++ "," ++ e ++ ")"

    def deepCopy: Eval[FUNC] =
      body.deepCopy.map(FUNC(name, args, _))
  }

  sealed abstract class EXPR {
    def toStr: Coeval[String]
    def deepCopy: Eval[EXPR]
    override def toString: String = toStr()
    def isItFailed: Boolean       = false
  }

  case class FAILED_EXPR() extends EXPR {
    def toStr: Coeval[String]        = Coeval.now("error")
    override def isItFailed: Boolean = true

    override def deepCopy: Eval[EXPR] =
      Eval.now(this)
  }

  case class GETTER(var expr: EXPR, field: String) extends EXPR {
    def toStr: Coeval[String] =
      for {
        e <- expr.toStr
      } yield "GETTER(" ++ e ++ "," ++ field ++ ")"

    override def deepCopy: Eval[EXPR] =
      expr.deepCopy.map(GETTER(_, field))
  }

  sealed trait BLOCK_DEF {
    val dec: DECLARATION
    var body: EXPR
  }

  @Deprecated
  case class LET_BLOCK(let: LET, var body: EXPR) extends EXPR with BLOCK_DEF {
    def toStr: Coeval[String] =
      for {
        e <- let.toStr
        b <- body.toStr
      } yield "LET_BLOCK(" ++ e ++ "," ++ b ++ ")"

    override val dec: DECLARATION = let

    override def deepCopy: Eval[EXPR] =
      for {
        l <- let.deepCopy
        b <- body.deepCopy
      } yield LET_BLOCK(l, b)
  }

  case class BLOCK(dec: DECLARATION, var body: EXPR) extends EXPR with BLOCK_DEF {
    def toStr: Coeval[String] =
      for {
        e <- dec.toStr
        b <- body.toStr
      } yield "BLOCK(" ++ e ++ "," ++ b ++ ")"

    override def deepCopy: Eval[EXPR] =
      for {
        l <- dec.deepCopy
        b <- body.deepCopy
      } yield BLOCK(l, b)
  }

  case class IF(var cond: EXPR, ifTrue: EXPR, ifFalse: EXPR) extends EXPR {
    def toStr: Coeval[String] =
      for {
        c <- cond.toStr
        t <- ifTrue.toStr
        f <- ifFalse.toStr
      } yield "IF(" ++ c ++ "," ++ t ++ "," ++ f ++ ")"

    override def deepCopy: Eval[EXPR] =
      for {
        c <- cond.deepCopy
        t <- ifTrue.deepCopy
        f <- ifFalse.deepCopy
      } yield IF(c, t, f)
  }
  case class REF(key: String) extends EXPR {
    override def toString: String = "REF(" ++ key ++ ")"
    def toStr: Coeval[String]     = Coeval.now(toString)

    override def deepCopy: Eval[EXPR] =
      Eval.now(this)
  }
  case class FUNCTION_CALL(function: FunctionHeader, var args: List[EXPR]) extends EXPR {
    def toStr: Coeval[String] =
      for {
        e <- args.map(_.toStr).sequence
      } yield "FUNCTION_CALL(" ++ function.toString ++ "," ++ e.toString ++ ")"

    override def deepCopy: Eval[EXPR] =
      Eval.defer(args.traverse(_.deepCopy)).map(FUNCTION_CALL(function, _))
  }

  sealed trait EVALUATED extends EXPR {
    def prettyString(level: Int): String = toString
    def toStr: Coeval[String]            = Coeval.now(toString)
    def weight: Long
    var wasLogged: Boolean = false

    override def deepCopy: Eval[EXPR] =
      Eval.now(this)
  }
  case class CONST_LONG(t: Long) extends EVALUATED {
    override def toString: String = t.toString
    override val weight: Long     = 8L
  }

  case class CONST_BYTESTR private (bs: ByteStr) extends EVALUATED {
    override def toString: String = bs.toString
    override def prettyString(level: Int): String = {
      if (bs.size > 1024) {
        "base64'" ++ Base64.encode(bs.arr) ++ "'"
      } else {
        "base58'" ++ Base58.encode(bs.arr) ++ "'"
      }
    }
    override val weight: Long = bs.size
  }
  object CONST_BYTESTR {
    def apply(bs: ByteStr): Either[ExecutionError, EVALUATED] =
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
  case class CONST_STRING private (s: String) extends EVALUATED {
    override def toString: String                 = s
    override def prettyString(level: Int): String = "\"" ++ escape(s) ++ "\""
    override val weight: Long                     = s.getBytes.length
  }
  object CONST_STRING {
    def apply(s: String): Either[ExecutionError, EVALUATED] =
      Either.cond(
        s.getBytes("UTF-8").length <= DATA_TX_BYTES_MAX,
        new CONST_STRING(s),
        s"String exceeds $DATA_TX_BYTES_MAX bytes"
      )
  }

  case class CONST_BOOLEAN(b: Boolean) extends EVALUATED {
    override def toString: String = b.toString
    override val weight: Long     = 1L
  }

  lazy val TRUE: CONST_BOOLEAN  = CONST_BOOLEAN(true)
  lazy val FALSE: CONST_BOOLEAN = CONST_BOOLEAN(false)

  case class CaseObj private (caseType: CASETYPEREF, fields: Map[String, EVALUATED]) extends EVALUATED {
    override def toString: String = TermPrinter.string(this)

    override def prettyString(depth: Int): String = TermPrinter.indentObjString(this, depth)

    override val weight: Long = OBJ_WEIGHT + FIELD_WEIGHT * fields.size + fields.map(_._2.weight).sum
  }

  object CaseObj {
    def apply(caseType: CASETYPEREF, fields: Map[String, EVALUATED]): CaseObj = {
      val obj = new CaseObj(caseType, fields)
      if (obj.weight > MaxWeight) {
        throw new Exception(s"the object ${caseType.name} is too heavy. Actual weight: ${obj.weight}, limit: $MaxWeight")
      } else {
        obj
      }
    }
  }

  abstract case class ARR private (xs: IndexedSeq[EVALUATED]) extends EVALUATED {
    override def toString: String = TermPrinter.string(this)

    val elementsWeightSum: Long =
      weight - EMPTYARR_WEIGHT - ELEM_WEIGHT * xs.size
  }

  object ARR {
    def apply(xs: IndexedSeq[EVALUATED], mweight: Long, limited: Boolean): Either[ExecutionError, ARR] =
      if (limited && xs.size > MaxListLengthV4) {
        Left(s"List size should not exceed $MaxListLengthV4")
      } else {
        Either.cond(
          mweight <= MaxWeight,
          new { override val weight: Long = mweight } with ARR(xs),
          s"the list is too heavy. Actual weight: ${mweight}, limit: ${MaxWeight}"
        )
      }

    def apply(xs: IndexedSeq[EVALUATED], limited: Boolean): Either[ExecutionError, ARR] = {
      val weight = EMPTYARR_WEIGHT + ELEM_WEIGHT * xs.size + xs.map(_.weight).sum
      ARR(xs, weight, limited)
    }
  }
}
