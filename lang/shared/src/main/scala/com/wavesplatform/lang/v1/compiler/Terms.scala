package com.wavesplatform.lang.v1.compiler

import cats.Eval
import cats.instances.list.*
import cats.syntax.traverse.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.ContractLimits.*
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Types.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.MaxListLengthV4
import com.wavesplatform.lang.*
import monix.eval.Coeval

import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3

object Terms {
  val DataTxMaxBytes: Int      = 150 * 1024     // should be the same as DataTransaction.MaxBytes
  val DataTxMaxProtoBytes: Int = 165947         // depends from DataTransaction.MaxProtoBytes
  val DataEntryValueMax: Int   = Short.MaxValue // should be the same as DataEntry.MaxValueSize

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
    // Set by copyForEvaluation when the value is still the original's; the evaluator copies the value before it first evaluates it. Not part
    // of equality.
    private[lang] var valueShared: Boolean = false

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

    override def hashCode(): Int = EXPR.hashCode(List(this), 7207)

    override def equals(obj: Any): Boolean = obj match {
      case e: EXPR => EXPR.equals(List(this), List(e))
      case _       => false
    }
  }

  object EXPR {
    @tailrec
    final def hashCode(stack: List[EXPR], prevHashCode: Int): Int =
      if (stack.isEmpty) prevHashCode
      else
        stack.head match {
          case FAILED_EXPR() =>
            hashCode(stack.tail, prevHashCode * 31 + 7001)
          case GETTER(expr, field) =>
            hashCode(expr :: stack.tail, (prevHashCode * 31 + field.hashCode) * 31 + 7013)
          case LET_BLOCK(let, body) =>
            hashCode(let.value :: body :: stack.tail, (prevHashCode * 31 + let.name.hashCode) * 31 + 7019)
          case BLOCK(dec, body) =>
            dec match {
              case FAILED_DEC() =>
                hashCode(body :: stack.tail, prevHashCode * 31 + 7027)
              case LET(name, value) =>
                hashCode(value :: stack.tail, (prevHashCode * 31 + name.hashCode) * 31 + 7027)
              case FUNC(name, args, body) =>
                hashCode(body :: stack.tail, (prevHashCode * 31 + MurmurHash3.listHash(name :: args, 7027)) * 31 + 7027)
            }
          case IF(cond, ifTrue, ifFalse) =>
            hashCode(cond :: ifTrue :: ifFalse :: stack.tail, prevHashCode * 31 + 7039)
          case REF(key) =>
            hashCode(stack.tail, (prevHashCode * 31 + key.hashCode) * 31 + 7043)
          case FUNCTION_CALL(function, args) =>
            hashCode(args ++ stack.tail, (prevHashCode * 31 + function.hashCode()) * 31 + 7057)
          case evaluated: EVALUATED =>
            hashCode(stack.tail, (prevHashCode * 31 + evaluated.hashCode()) * 31 + 7069)
        }

    @tailrec
    final def equals(e1: List[EXPR], e2: List[EXPR]): Boolean =
      if (e1.isEmpty && e2.isEmpty) true
      else
        (e1.head, e2.head) match {
          case (FAILED_EXPR(), FAILED_EXPR()) =>
            equals(e1.tail, e2.tail)
          case (g1: GETTER, g2: GETTER) =>
            if (g1.field != g2.field) false
            else equals(g1.expr :: e1.tail, g2.expr :: e2.tail)
          case (lb1: LET_BLOCK, lb2: LET_BLOCK) =>
            if (lb1.let.name != lb2.let.name) false
            else equals(lb1.let.value :: lb1.body :: e1.tail, lb2.let.value :: lb2.body :: e2.tail)
          case (b1: BLOCK, b2: BLOCK) =>
            (b1.dec, b2.dec) match {
              case (FAILED_DEC(), FAILED_DEC()) =>
                equals(b1.body :: e1.tail, b2.body :: e2.tail)
              case (l1: LET, l2: LET) =>
                if (l1.name != l2.name) false
                else equals(l1.value :: b1.body :: e1.tail, l2.value :: b2.body :: e2.tail)
              case (f1: FUNC, f2: FUNC) =>
                if (f1.name != f2.name || f1.args != f2.args) false
                else equals(f1.body :: b1.body :: e1.tail, f2.body :: b2.body :: e2.tail)
              case _ => false
            }
          case (if1: IF, if2: IF) =>
            equals(
              if1.cond :: if1.ifFalse :: if1.ifTrue :: e1.tail,
              if2.cond :: if2.ifFalse :: if2.ifTrue :: e2.tail
            )
          case (REF(key1), REF(key2)) =>
            if (key1 != key2) false
            else equals(e1.tail, e2.tail)
          case (fc1: FUNCTION_CALL, fc2: FUNCTION_CALL) =>
            if (fc1.function != fc2.function) false
            else equals(fc1.args ++ e1.tail, fc2.args ++ e2.tail)
          case (ev1: EVALUATED, ev2: EVALUATED) =>
            if (ev1 != ev2) false
            else equals(e1.tail, e2.tail)
          case _ => false
        }
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

  @deprecated("use BLOCK instead", "1.0")
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
        e <- args.map(_.toStr).sequence[Coeval, String]
      } yield "FUNCTION_CALL(" ++ function.toString ++ "," ++ e.toString ++ ")"

    override def deepCopy: Eval[EXPR] =
      args
        .foldLeft(Eval.now(List.empty[EXPR])) { case (prev, cur) =>
          prev.flatMap(p => cur.deepCopy.map(_ :: p))
        }
        .map(reversedArgs => FUNCTION_CALL(function, reversedArgs.reverse))
  }

  sealed trait EVALUATED extends EXPR {
    def toStr: Coeval[String] = Coeval.now(toString)
    def weight: Long
    val getType: REAL // used for _isInstanceOf and therefore for match

    override def deepCopy: Eval[EXPR] =
      Eval.now(this)
  }

  case class CONST_LONG(t: Long) extends EVALUATED {
    override def toString: String = t.toString
    override val weight: Long     = 8L
    override val getType: REAL    = LONG
    override def hashCode(): Int  = t.hashCode()
    override def equals(obj: Any): Boolean = obj match {
      case CONST_LONG(`t`) => true
      case _               => false
    }
  }
  case class CONST_BIGINT(t: BigInt) extends EVALUATED {
    override def toString: String = t.toString
    override val weight: Long     = 64L
    override val getType: REAL    = BIGINT
    override def hashCode(): Int  = t.hashCode()
    override def equals(obj: Any): Boolean = obj match {
      case CONST_BIGINT(`t`) => true
      case _                 => false
    }
  }

  class CONST_BYTESTR private (val bs: ByteStr) extends EVALUATED {
    override def toString: String = bs.toString

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
    case object DataEntrySize extends Limit(DataEntryValueMax)
    case object DataTxSize    extends Limit(DataTxMaxBytes)
    case object NoLimit       extends Limit(Int.MaxValue)

    def apply(bs: ByteStr, limit: Limit = DataEntrySize): Either[CommonError, EVALUATED] =
      Either.cond(
        bs.size <= limit.value,
        new CONST_BYTESTR(bs),
        s"ByteStr size=${bs.size} exceeds ${limit.value} bytes"
      )

    def unapply(arg: CONST_BYTESTR): Option[ByteStr] =
      Some(arg.bs)
  }

  class CONST_STRING private (val s: String, bytesLength: Int) extends EVALUATED {
    override def toString: String  = s
    override lazy val weight: Long = bytesLength

    override val getType: REAL = STRING

    override def equals(obj: Any): Boolean =
      obj match {
        case CONST_STRING(`s`) => true
        case _                 => false
      }

    override def hashCode(): Int = s.hashCode
  }

  object CONST_STRING {
    def apply(s: String, reduceLimit: Boolean = true, bytesLength: Option[Int] = None): Either[CommonError, CONST_STRING] = {
      val limit =
        if (reduceLimit) DataEntryValueMax
        else DataTxMaxBytes

      val actualSize = bytesLength.getOrElse(s.getBytes(StandardCharsets.UTF_8).length)

      Either.cond(
        actualSize <= limit,
        new CONST_STRING(s, actualSize),
        s"String size=$actualSize exceeds $limit bytes"
      )
    }

    def unapply(arg: CONST_STRING): Option[String] =
      Some(arg.s)
  }

  case class CONST_BOOLEAN(b: Boolean) extends EVALUATED {
    override def toString: String = b.toString
    override val weight: Long     = 1L

    override val getType: REAL = BOOLEAN

    override def hashCode(): Int = b.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case CONST_BOOLEAN(`b`) => true
      case _                  => false
    }
  }

  lazy val TRUE: CONST_BOOLEAN  = CONST_BOOLEAN(true)
  lazy val FALSE: CONST_BOOLEAN = CONST_BOOLEAN(false)

  case class CaseObj private (caseType: CASETYPEREF, fields: Map[String, EVALUATED]) extends EVALUATED {
    // must be with fixArrIndentation = false, because of makeString behavior before RideV6 (NODE-2370)
    override def toString: String = TermPrinter().string(this)

    override val weight: Long = OBJ_WEIGHT + FIELD_WEIGHT * fields.size + fields.map(_._2.weight).sum

    override val getType: REAL =
      if (caseType.name == runtimeTupleType.name)
        TUPLE(fields.toSeq.sortBy(_._1).map(_._2.getType).toList)
      else
        caseType

    override def hashCode(): Int = MurmurHash3.caseClassHash(this)

    override def equals(obj: Any): Boolean = obj match {
      case CaseObj(`caseType`, `fields`) => true
      case _                             => false
    }
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
    // must be with fixArrIndentation = false, because of makeString behavior before RideV6 (NODE-2370)
    override def toString: String = TermPrinter().string(this)

    lazy val elementsWeightSum: Long =
      weight - EMPTYARR_WEIGHT - ELEM_WEIGHT * xs.size

    override val getType: REAL = LIST(ANY)

    override def hashCode(): Int = MurmurHash3.indexedSeqHash(xs, 4517)

    override def equals(obj: Any): Boolean = obj match {
      case ARR(`xs`) => true
      case _         => false
    }
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
      var measuredWeight = EMPTYARR_WEIGHT + ELEM_WEIGHT * xs.size
      xs.foreach { ev => measuredWeight += ev.weight }
      ARR(xs, measuredWeight, limited)
    }
  }

  case class FAIL(reason: String) extends EVALUATED {
    override def toString: String = "Evaluation failed: " ++ reason
    def weight: Long              = 0
    override val getType: REAL    = NOTHING

    override def hashCode(): Int = reason.hashCode * 31 + 7129

    override def equals(obj: Any): Boolean = obj match {
      case FAIL(`reason`) => true
      case _              => false
    }
  }

  val runtimeTupleType: CASETYPEREF = CASETYPEREF("Tuple", Nil)

  implicit val orderingConstLong: Ordering[CONST_LONG] =
    (a, b) => a.t compare b.t

  implicit val orderingConstBigInt: Ordering[CONST_BIGINT] =
    (a, b) => a.t compare b.t

  // Marks the node below it on the work stack as one whose children are all copied.
  private object Rebuild

  /** Copies a tree for the evaluator, which writes into it. A loop over explicit stacks: going through `Eval` ([[EXPR.deepCopy]]) allocated
    * several objects per node and took a third of block appending on dApp-heavy blocks.
    *
    * Only what the evaluator writes into before it is reached gets copied. Leaves are immutable and shared. Function declarations are shared:
    * the evaluator copies a body at every call and never writes into it in place. A LET is copied, but its value is shared and marked with
    * `valueShared`; the evaluator copies the value when it first evaluates it, so values that are never referenced are never copied.
    */
  def copyForEvaluation(root: EXPR): EXPR = {
    val work   = new java.util.ArrayDeque[AnyRef]()
    val copies = new java.util.ArrayDeque[AnyRef]()

    def copy[A]: A = copies.pop().asInstanceOf[A]

    def isShared(e: AnyRef): Boolean = e match {
      case _: REF | _: FAILED_EXPR | _: EVALUATED | _: FAILED_DEC | _: FUNC => true
      case _                                                                => false
    }

    def copyLet(l: LET): LET = {
      val copied = LET(l.name, l.value)
      copied.valueShared = !isShared(l.value)
      copied
    }

    work.push(root)
    while (!work.isEmpty) work.pop() match {
      case Rebuild =>
        val copied = work.pop() match {
          case g: GETTER     => GETTER(copy, g.field)
          case lb: LET_BLOCK => LET_BLOCK(copyLet(lb.let), copy)
          case _: BLOCK =>
            val body = copy[EXPR]
            BLOCK(copy, body)
          case _: IF =>
            val ifFalse = copy[EXPR]
            val ifTrue  = copy[EXPR]
            IF(copy, ifTrue, ifFalse)
          case fc: FUNCTION_CALL =>
            var args = List.empty[EXPR]
            fc.args.foreach(_ => args = copy[EXPR] :: args)
            FUNCTION_CALL(fc.function, args)
          case other => throw new IllegalStateException(s"Unexpected node $other")
        }
        copies.push(copied)
      case shared if isShared(shared) =>
        copies.push(shared)
      case l: LET =>
        copies.push(copyLet(l))
      case node =>
        work.push(node)
        work.push(Rebuild)
        // Children go on in reverse, so they are copied first to last and come off the copies stack last to first.
        node match {
          case g: GETTER         => work.push(g.expr)
          case lb: LET_BLOCK     => work.push(lb.body)
          case b: BLOCK          => work.push(b.body); work.push(b.dec)
          case i: IF             => work.push(i.ifFalse); work.push(i.ifTrue); work.push(i.cond)
          case fc: FUNCTION_CALL => fc.args.reverseIterator.foreach(work.push)
          case other             => throw new IllegalStateException(s"Unexpected node $other")
        }
    }
    copies.pop().asInstanceOf[EXPR]
  }
}
