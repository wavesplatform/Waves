package com.wavesplatform.lang

import monix.eval.Coeval
import scodec.bits.ByteVector

object Terms {

  sealed trait Type { type Underlying }
  case object NOTHING              extends Type { type Underlying = Nothing              }
  case object UNIT                 extends Type { type Underlying = Unit                 }
  case object INT                  extends Type { type Underlying = Int                  }
  case object BYTEVECTOR           extends Type { type Underlying = ByteVector           }
  case object BOOLEAN              extends Type { type Underlying = Boolean              }
  case class OPTION(t: Type)       extends Type { type Underlying = Option[t.Underlying] }
  case class TYPEREF(name: String) extends Type { type Underlying = AnyRef               }

  case class CUSTOMTYPE(name: String, fields: List[(String, Type)])

  sealed trait LazyVal {
    val tpe: Type
    val value: Coeval[tpe.Underlying]
  }

  object LazyVal {
    private case class LazyValImpl(tpe: Type, v: Coeval[Any]) extends LazyVal {
      override val value: Coeval[tpe.Underlying] = v.map(_.asInstanceOf[tpe.Underlying])
    }

    def apply(t: Type)(v: Coeval[t.Underlying]): LazyVal = LazyValImpl(t, v)
  }

  case class OBJECT(fields: Map[String, LazyVal])

  def findCommonType(t1: Type, t2: Type): Option[Type] =
    if (t1 == NOTHING) Some(t2)
    else if (t2 == NOTHING) Some(t1)
    else if (t1 == t2) Some(t1)
    else
      (t1, t2) match {
        case (OPTION(it1), OPTION(it2)) => findCommonType(it1, it2).map(OPTION)
        case _                          => None
      }

  object Untyped {
    sealed trait Expr
    case class CONST_INT(t: Int)                                              extends Expr
    case class GETTER(ref: Block, field: String)                              extends Expr
    case class CONST_BYTEVECTOR(bs: ByteVector)                               extends Expr
    case class SUM(i1: Block, i2: Block)                                      extends Expr
    case class AND(t1: Block, t2: Block)                                      extends Expr
    case class OR(t1: Block, t2: Block)                                       extends Expr
    case class EQ(t1: Block, t2: Block)                                       extends Expr
    case class GT(t1: Block, t2: Block)                                       extends Expr
    case class GE(t1: Block, t2: Block)                                       extends Expr
    case class SIG_VERIFY(message: Block, signature: Block, publicKey: Block) extends Expr
    case class IS_DEFINED(t: Block)                                           extends Expr
    case class LET(name: String, value: Block)                                extends Expr
    case class Block(let: Option[LET], t: Expr)                               extends Expr
    case class IF(cond: Block, ifTrue: Block, ifFalse: Block)                 extends Expr
    case class REF(key: String)                                               extends Expr
    case class GET(t: Block)                                                  extends Expr
    case object TRUE                                                          extends Expr
    case object FALSE                                                         extends Expr
    case object NONE                                                          extends Expr
    case class SOME(t: Block)                                                 extends Expr
  }

  object Typed {
    sealed trait Expr { val exprType: Type }
    case class CONST_INT(t: Int)                                              extends Expr { val exprType: Type = INT }
    case class GETTER(ref: Block, field: String, exprType: Type)              extends Expr
    case class CONST_BYTEVECTOR(bs: ByteVector)                               extends Expr { val exprType: Type = BYTEVECTOR }
    case class SUM(i1: Block, i2: Block)                                      extends Expr { val exprType: Type = INT }
    case class AND(t1: Block, t2: Block)                                      extends Expr { val exprType: Type = BOOLEAN }
    case class OR(t1: Block, t2: Block)                                       extends Expr { val exprType: Type = BOOLEAN }
    case class EQ(t1: Block, t2: Block)                                       extends Expr { val exprType: Type = BOOLEAN }
    case class GT(t1: Block, t2: Block)                                       extends Expr { val exprType: Type = BOOLEAN }
    case class GE(t1: Block, t2: Block)                                       extends Expr { val exprType: Type = BOOLEAN }
    case class SIG_VERIFY(message: Block, signature: Block, publicKey: Block) extends Expr { val exprType: Type = BOOLEAN }
    case class IS_DEFINED(t: Block)                                           extends Expr { val exprType: Type = BOOLEAN }
    case class LET(name: String, value: Block)                                extends Expr { val exprType: Type = UNIT } // subtype of Expr mostly for serde
    case class Block(let: Option[LET], t: Expr, exprType: Type)               extends Expr
    case class IF(cond: Block, ifTrue: Block, ifFalse: Block, exprType: Type) extends Expr
    case class REF(key: String, exprType: Type)                               extends Expr
    case class GET(t: Block, exprType: Type)                                  extends Expr
    case object TRUE                                                          extends Expr { val exprType: Type = BOOLEAN }
    case object FALSE                                                         extends Expr { val exprType: Type = BOOLEAN }
    case object NONE                                                          extends Expr { val exprType: Type = OPTION(NOTHING) }
    case class SOME(t: Block, exprType: Type)                                 extends Expr
  }

  object Implicits {
    implicit def exprToBlock(t: Untyped.Expr): Untyped.Block = Untyped.Block(None, t)
  }
}
