package com.wavesplatform.lang
import monix.eval.Coeval
import scodec.bits.ByteVector
import scala.language.implicitConversions

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

  def eqType(t1: Type, t2: Type): Option[Type] =
    if (t1 == NOTHING) Some(t2)
    else if (t2 == NOTHING) Some(t1)
    else if (t1 == t2) Some(t1)
    else
      (t1, t2) match {
        case (OPTION(it1), OPTION(it2)) => eqType(it1, it2).map(OPTION)
        case _                          => None
      }

  sealed trait Expr { val predefinedType: Option[Type] }
  case class CONST_INT(t: Int)                                              extends Expr { val predefinedType: Option[Type] = Some(INT)             }
  case class GETTER(i: Block, field: String)                                extends Expr { val predefinedType: Option[Type] = None                  }
  case class CONST_BYTEVECTOR(bs: ByteVector)                               extends Expr { val predefinedType: Option[Type] = Some(BYTEVECTOR)      }
  case class SUM(i1: Block, i2: Block)                                      extends Expr { val predefinedType: Option[Type] = Some(INT)             }
  case class AND(t1: Block, t2: Block)                                      extends Expr { val predefinedType: Option[Type] = Some(BOOLEAN)         }
  case class OR(t1: Block, t2: Block)                                       extends Expr { val predefinedType: Option[Type] = Some(BOOLEAN)         }
  case class EQ(t1: Expr, t2: Block)                                        extends Expr { val predefinedType: Option[Type] = Some(BOOLEAN)         }
  case class GT(t1: Block, t2: Block)                                       extends Expr { val predefinedType: Option[Type] = Some(BOOLEAN)         }
  case class GE(t1: Block, t2: Block)                                       extends Expr { val predefinedType: Option[Type] = Some(BOOLEAN)         }
  case class SIG_VERIFY(message: Block, signature: Block, publicKey: Block) extends Expr { val predefinedType: Option[Type] = Some(BOOLEAN)         }
  case class IS_DEFINED(t: Block)                                           extends Expr { val predefinedType: Option[Type] = Some(BOOLEAN)         }
  case class LET(name: String, value: Block)                                extends Expr { val predefinedType: Option[Type] = Some(UNIT)            } // subtype of Expr mostly for serde
  case class Block(let: Option[LET], t: Expr)                               extends Expr { val predefinedType: Option[Type] = None                  }
  case class IF(cond: Block, ifTrue: Block, ifFalse: Block)                 extends Expr { val predefinedType: Option[Type] = None                  }
  case class REF(key: String)                                               extends Expr { val predefinedType: Option[Type] = None                  }
  case class GET(t: Block)                                                  extends Expr { val predefinedType: Option[Type] = None                  }
  case object TRUE                                                          extends Expr { val predefinedType               = Some(BOOLEAN)         }
  case object FALSE                                                         extends Expr { val predefinedType               = Some(BOOLEAN)         }
  case object NONE                                                          extends Expr { val predefinedType: Option[Type] = Some(OPTION(NOTHING)) }
  case class SOME(t: Block)                                                 extends Expr { val predefinedType: Option[Type] = None                  }
  implicit def exprToBlock(t: Expr): Block = Block(None, t)
}
