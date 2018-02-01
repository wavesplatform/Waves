package com.wavesplatform.lang
import scodec.bits.ByteVector

object Terms {

  sealed trait Type { type Underlying }
  case object NOTHING        extends Type { type Underlying = Nothing              }
  case object UNIT           extends Type { type Underlying = Unit                 }
  case object INT            extends Type { type Underlying = Int                  }
  case object BYTEVECTOR     extends Type { type Underlying = ByteVector           }
  case object BOOLEAN        extends Type { type Underlying = Boolean              }
  case class OPTION(t: Type) extends Type { type Underlying = Option[t.Underlying] }

  def eqType(t1: Type, t2: Type): Option[Type] =
    if (t1 == NOTHING) Some(t2)
    else if (t2 == NOTHING) Some(t1)
    else if (t1 == t2) Some(t1)
    else
      (t1, t2) match {
        case (OPTION(it1), OPTION(it2)) => eqType(it1, it2)
        case _                          => None
      }

  sealed trait Field { val tpe: Type }
  case object __satisfy_shapeless_0 extends Field { val tpe: Type = BOOLEAN            }
  case object Id                    extends Field { val tpe: Type = BYTEVECTOR         }
  case object Type                  extends Field { val tpe: Type = INT                }
  case object SenderPk              extends Field { val tpe: Type = BYTEVECTOR         }
  case object AssetId               extends Field { val tpe: Type = OPTION(BYTEVECTOR) }
  case object BodyBytes             extends Field { val tpe: Type = BYTEVECTOR         }
  case class Proof(i: Int)          extends Field { val tpe: Type = BYTEVECTOR         }

  sealed trait Expr { val predefinedType: Option[Type] }

  case class CONST_INT(t: Int)                                              extends Expr { val predefinedType: Option[Type] = Some(INT)             }
  case class CONST_BYTEVECTOR(bs: ByteVector)                               extends Expr { val predefinedType: Option[Type] = Some(BYTEVECTOR)      }
  case class SUM(i1: Block, i2: Block)                                      extends Expr { val predefinedType: Option[Type] = Some(INT)             }
  case class AND(t1: Block, t2: Block)                                      extends Expr { val predefinedType: Option[Type] = Some(BOOLEAN)         }
  case class OR(t1: Block, t2: Block)                                       extends Expr { val predefinedType: Option[Type] = Some(BOOLEAN)         }
  case class EQ(t1: Expr, t2: Block)                                        extends Expr { val predefinedType: Option[Type] = Some(BOOLEAN)         }
  case class GT(t1: Block, t2: Block)                                       extends Expr { val predefinedType: Option[Type] = Some(BOOLEAN)         }
  case class GE(t1: Block, t2: Block)                                       extends Expr { val predefinedType: Option[Type] = Some(BOOLEAN)         }
  case class SIG_VERIFY(message: Block, signature: Block, publicKey: Block) extends Expr { val predefinedType: Option[Type] = Some(BOOLEAN)         }
  case object HEIGHT                                                        extends Expr { val predefinedType: Option[Type] = Some(INT)             }
  case class TX_FIELD(field: Field)                                         extends Expr { val predefinedType: Option[Type] = Some(field.tpe)       }
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
