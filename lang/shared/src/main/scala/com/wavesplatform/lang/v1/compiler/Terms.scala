package com.wavesplatform.lang.v1.compiler

import java.nio.charset.StandardCharsets

import cats.Eval
import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.ContractLimits._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.MaxListLengthV4
import monix.eval.Coeval

object Terms {
  val DataTxMaxBytes: Int       = 150 * 1024     // should be the same as DataTransaction.MaxBytes
  val DataTxMaxProtoBytes: Int  = 165947         // depends from DataTransaction.MaxProtoBytes
  val DataEntryValueMax: Int    = Short.MaxValue // should be the same as DataEntry.MaxValueSize

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
    val getType: REAL // used for _isInstanceOf and therefore for match

    override def deepCopy: Eval[EXPR] =
      Eval.now(this)
  }

  case class CONST_LONG(t: Long) extends EVALUATED {
    override def toString: String = t.toString
    override val weight: Long     = 8L
    override val getType: REAL = LONG
  }
  case class CONST_BIGINT(t: BigInt) extends EVALUATED {
    override def toString: String = t.toString
    override val weight: Long     = 64L
    override val getType: REAL = BIGINT
  }

  class CONST_BYTESTR private (val bs: ByteStr) extends EVALUATED {
    override def toString: String = bs.toString
    override def prettyString(level: Int): String = {
      if (bs.size > 1024) {
        "base64'" ++ Base64.encode(bs.arr) ++ "'"
      } else {
        "base58'" ++ Base58.encode(bs.arr) ++ "'"
      }
    }
    override val weight: Long = bs.size

    override val getType: REAL = BYTESTR

    override def equals(obj: Any): Boolean =
      obj match {
        case CONST_BYTESTR(`bs`) => true
        case _                   => false
      }

    override def hashCode(): Int = bs.hashCode
  }

  object CONST_BYTESTR {
    sealed abstract class Limit(val value: Int)
    case object DataEntrySize   extends Limit(DataEntryValueMax)
    case object DataTxSize      extends Limit(DataTxMaxBytes)
    case object NoLimit         extends Limit(Int.MaxValue)

    def apply(bs: ByteStr, limit: Limit = DataEntrySize): Either[ExecutionError, EVALUATED] =
      Either.cond(
        bs.size <= limit.value,
        new CONST_BYTESTR(bs),
        s"ByteStr size=${bs.size} exceeds ${limit.value} bytes"
      )

    def unapply(arg: CONST_BYTESTR): Option[ByteStr] =
      Some(arg.bs)
  }

  class CONST_STRING private (val s: String) extends EVALUATED {
    override def toString: String                 = s
    override def prettyString(level: Int): String = "\"" ++ escape(s) ++ "\""
    override val weight: Long                     = s.getBytes.length

    override  val getType: REAL = STRING

    override def equals(obj: Any): Boolean =
      obj match {
        case CONST_STRING(`s`) => true
        case _                 => false
      }

    override def hashCode(): Int = s.hashCode
  }
  object CONST_STRING {
    def apply(s: String, reduceLimit: Boolean = true): Either[ExecutionError, CONST_STRING] = {
      val limit =
        if (reduceLimit) DataEntryValueMax
        else DataTxMaxBytes

      val actualSize = s.getBytes(StandardCharsets.UTF_8).length

      Either.cond(
        actualSize <= limit,
        new CONST_STRING(s),
        s"String size=$actualSize exceeds $limit bytes"
      )
    }

    def unapply(arg: CONST_STRING): Option[String] =
      Some(arg.s)
  }

  private def escape(s: String): String = {
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

  case class CONST_BOOLEAN(b: Boolean) extends EVALUATED {
    override def toString: String = b.toString
    override val weight: Long     = 1L

    override val getType: REAL = BOOLEAN
  }

  lazy val TRUE: CONST_BOOLEAN  = CONST_BOOLEAN(true)
  lazy val FALSE: CONST_BOOLEAN = CONST_BOOLEAN(false)

  case class CaseObj private (caseType: CASETYPEREF, fields: Map[String, EVALUATED]) extends EVALUATED {
    override def toString: String = TermPrinter.string(this)

    override def prettyString(depth: Int): String = TermPrinter.indentObjString(this, depth)

    override val weight: Long = OBJ_WEIGHT + FIELD_WEIGHT * fields.size + fields.map(_._2.weight).sum

    override val getType: REAL =
      if (caseType.name == runtimeTupleType.name)
        TUPLE(fields.toSeq.sortBy(_._1).map(_._2.getType).toList)
      else
        caseType
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

    lazy val elementsWeightSum: Long =
      weight - EMPTYARR_WEIGHT - ELEM_WEIGHT * xs.size

    override val getType: REAL = LIST(ANY)
  }

  object ARR {
    def apply(xs: IndexedSeq[EVALUATED], mweight: Long, limited: Boolean): Either[ExecutionError, ARR] =
      if (limited && xs.size > MaxListLengthV4) {
        Left(s"List size should not exceed $MaxListLengthV4")
      } else {
        Either.cond(
          mweight <= MaxWeight,
          new ARR(xs) { override val weight: Long = mweight },
          s"The list is too heavy. Actual weight: $mweight, limit: $MaxWeight"
        )
      }

    def apply(xs: IndexedSeq[EVALUATED], limited: Boolean): Either[ExecutionError, ARR] = {
      val weight = EMPTYARR_WEIGHT + ELEM_WEIGHT * xs.size + xs.map(_.weight).sum
      ARR(xs, weight, limited)
    }
  }

  case class FAIL(reason: String) extends EVALUATED {
    override def toString: String = "Evaluation failed: " ++ reason
    def weight: Long = 0
    override val getType: REAL = NOTHING
  }

  val runtimeTupleType: CASETYPEREF = CASETYPEREF("Tuple", Nil)

  implicit val orderingConstLong: Ordering[CONST_LONG] =
    (a, b) => a.t compare b.t

  implicit val orderingConstBigInt: Ordering[CONST_BIGINT] =
    (a, b) => a.t compare b.t
}
