package scorex.transaction.smart.lang

import scodec.bits.ByteVector

object Terms {

  sealed trait Type {
    type Underlying
  }
  case object UNIT               extends Type { type Underlying = Unit       }
  case object INT                extends Type { type Underlying = Int        }
  case object BYTEVECTOR         extends Type { type Underlying = ByteVector }
  case object BOOLEAN            extends Type { type Underlying = Boolean    }
  case class OPTION[T <: Type]() extends Type { type Underlying = Option[T]  }

  sealed trait Field {
    val tpe: Type
  }
  case object __satisfy_shapeless_0 extends Field { val tpe: Type = BOOLEAN    }
  case object Id                    extends Field { val tpe: Type = BYTEVECTOR }
  case object Type                  extends Field { val tpe: Type = INT        }
  case object SenderPk              extends Field { val tpe: Type = BYTEVECTOR }
  case object BodyBytes             extends Field { val tpe: Type = BYTEVECTOR }
  case object Proof_0               extends Field { val tpe: Type = BYTEVECTOR }
  case object Proof_1               extends Field { val tpe: Type = BYTEVECTOR }
  case object Proof_2               extends Field { val tpe: Type = BYTEVECTOR }

  sealed trait Term {
    val knownType: Option[Type]
  }

  case class COMPOSITE(let: Option[LET], t: Term)                                       extends Term { val knownType: Option[Type] = t.knownType      }
  case class LET(name: String, t: COMPOSITE)                                            extends Term { val knownType: Option[Type] = Some(UNIT)       }
  case class REF(key: String)                                                           extends Term { val knownType: Option[Type] = None             }
  case class CONST_INT(t: Int)                                                          extends Term { val knownType: Option[Type] = Some(INT)        }
  case class CONST_BYTEVECTOR(bs: ByteVector)                                           extends Term { val knownType: Option[Type] = Some(BYTEVECTOR) }
  case class SUM(i1: COMPOSITE, i2: COMPOSITE)                                          extends Term { val knownType: Option[Type] = Some(INT)        }
  case class IF(cond: COMPOSITE, ifTrue: COMPOSITE, ifFalse: COMPOSITE)                 extends Term { val knownType: Option[Type] = ifTrue.knownType }
  case class AND(t1: COMPOSITE, t2: COMPOSITE)                                          extends Term { val knownType: Option[Type] = Some(BOOLEAN)    }
  case class OR(t1: COMPOSITE, t2: COMPOSITE)                                           extends Term { val knownType: Option[Type] = Some(BOOLEAN)    }
  case class EQ_INT(t1: Term, t2: COMPOSITE)                                            extends Term { val knownType: Option[Type] = Some(BOOLEAN)    }
  case class GT(t1: COMPOSITE, t2: COMPOSITE)                                           extends Term { val knownType: Option[Type] = Some(BOOLEAN)    }
  case class GE(t1: COMPOSITE, t2: COMPOSITE)                                           extends Term { val knownType: Option[Type] = Some(BOOLEAN)    }
  case class SIG_VERIFY(message: COMPOSITE, signature: COMPOSITE, publicKey: COMPOSITE) extends Term { val knownType: Option[Type] = Some(BOOLEAN)    }
  case object HEIGHT                                                                    extends Term { val knownType: Option[Type] = Some(INT)        }
  case class TX_FIELD(field: Field)                                                     extends Term { val knownType: Option[Type] = Some(field.tpe)  }

  implicit def term2compoiste(t: Term): COMPOSITE = COMPOSITE(None, t)

}
