package com.wavesplatform.lang

import monix.eval.Coeval
import scodec.bits.ByteVector

object Terms {

  sealed trait TYPE { type Underlying }
  case object NOTHING              extends TYPE { type Underlying = Nothing              }
  case object UNIT                 extends TYPE { type Underlying = Unit                 }
  case object INT                  extends TYPE { type Underlying = Int                  }
  case object BYTEVECTOR           extends TYPE { type Underlying = ByteVector           }
  case object BOOLEAN              extends TYPE { type Underlying = Boolean              }
  case class OPTION(t: TYPE)       extends TYPE { type Underlying = Option[t.Underlying] }
  case class TYPEREF(name: String) extends TYPE { type Underlying = AnyRef               }

  case class CUSTOMTYPE(name: String, fields: List[(String, TYPE)])

  sealed trait LazyVal {
    val tpe: TYPE
    val value: Coeval[tpe.Underlying]
  }

  object LazyVal {
    private case class LazyValImpl(tpe: TYPE, v: Coeval[Any]) extends LazyVal {
      override val value: Coeval[tpe.Underlying] = v.map(_.asInstanceOf[tpe.Underlying])
    }

    def apply(t: TYPE)(v: Coeval[t.Underlying]): LazyVal = LazyValImpl(t, v)
  }

  case class OBJECT(fields: Map[String, LazyVal])

  def findCommonType(t1: TYPE, t2: TYPE): Option[TYPE] =
    if (t1 == NOTHING) Some(t2)
    else if (t2 == NOTHING) Some(t1)
    else if (t1 == t2) Some(t1)
    else
      (t1, t2) match {
        case (OPTION(it1), OPTION(it2)) => findCommonType(it1, it2).map(OPTION)
        case _                          => None
      }

  object Untyped {
    sealed trait EXPR
    case class CONST_INT(t: Int)                                              extends EXPR
    case class GETTER(ref: BLOCK, field: String)                              extends EXPR
    case class CONST_BYTEVECTOR(bs: ByteVector)                               extends EXPR
    case class SUM(a: BLOCK, b: BLOCK)                                        extends EXPR
    case class AND(a: BLOCK, b: BLOCK)                                        extends EXPR
    case class OR(a: BLOCK, b: BLOCK)                                         extends EXPR
    case class EQ(a: BLOCK, b: BLOCK)                                         extends EXPR
    case class GT(a: BLOCK, b: BLOCK)                                         extends EXPR
    case class GE(a: BLOCK, b: BLOCK)                                         extends EXPR
    case class SIG_VERIFY(message: BLOCK, signature: BLOCK, publicKey: BLOCK) extends EXPR
    case class IS_DEFINED(opt: BLOCK)                                         extends EXPR
    case class LET(name: String, value: BLOCK)                                extends EXPR
    case class BLOCK(let: Option[LET], body: EXPR)                            extends EXPR
    case class IF(cond: BLOCK, ifTrue: BLOCK, ifFalse: BLOCK)                 extends EXPR
    case class REF(key: String)                                               extends EXPR
    case class GET(opt: BLOCK)                                                extends EXPR
    case object TRUE                                                          extends EXPR
    case object FALSE                                                         extends EXPR
    case object NONE                                                          extends EXPR
    case class SOME(t: BLOCK)                                                 extends EXPR
  }

  object Typed {
    sealed abstract class EXPR(val tpe: TYPE)
    case class CONST_INT(t: Int)                                                   extends EXPR(INT)
    case class GETTER(ref: EXPR, field: String, override val tpe: TYPE)            extends EXPR(tpe)
    case class CONST_BYTEVECTOR(bs: ByteVector)                                    extends EXPR(BYTEVECTOR)
    case class SUM(a: EXPR, b: EXPR)                                               extends EXPR(INT)
    case class AND(a: EXPR, b: EXPR)                                               extends EXPR(BOOLEAN)
    case class OR(a: EXPR, b: EXPR)                                                extends EXPR(BOOLEAN)
    case class EQ(a: EXPR, b: EXPR)                                                extends EXPR(BOOLEAN)
    case class GT(a: EXPR, b: EXPR)                                                extends EXPR(BOOLEAN)
    case class GE(a: EXPR, b: EXPR)                                                extends EXPR(BOOLEAN)
    case class SIG_VERIFY(message: EXPR, signature: EXPR, publicKey: EXPR)         extends EXPR(BOOLEAN)
    case class IS_DEFINED(opt: EXPR)                                               extends EXPR(BOOLEAN)
    case class LET(name: String, value: EXPR)                                      extends EXPR(UNIT)
    case class BLOCK(let: Option[LET], body: EXPR, override val tpe: TYPE)         extends EXPR(tpe)
    case class IF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR, override val tpe: TYPE) extends EXPR(tpe)
    case class REF(key: String, override val tpe: TYPE)                            extends EXPR(tpe)
    case class GET(opt: EXPR, override val tpe: TYPE)                              extends EXPR(tpe)
    case object TRUE                                                               extends EXPR(BOOLEAN)
    case object FALSE                                                              extends EXPR(BOOLEAN)
    case object NONE                                                               extends EXPR(OPTION(NOTHING))
    case class SOME(t: EXPR, override val tpe: TYPE)                               extends EXPR(tpe)
  }

  object Implicits {
    implicit def exprToBlock(t: Untyped.EXPR): Untyped.BLOCK = Untyped.BLOCK(None, t)
  }
}
