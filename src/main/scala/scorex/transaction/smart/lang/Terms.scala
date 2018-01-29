package scorex.transaction.smart.lang

import scodec.bits.ByteVector

object Terms {

  sealed trait Type
  case object INT                extends Type
  case object BYTEVECTOR         extends Type
  case object BOOLEAN            extends Type
  case class OPTION[T <: Type]() extends Type

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
    val tpe: Type
  }

  case class CONST_INT(t: Int)                                           extends Term { val tpe: Type = INT        }
  case class CONST_BYTEVECTOR(bs: ByteVector)                            extends Term { val tpe: Type = BYTEVECTOR }
  case class SUM(i1: Term, i2: Term)                                     extends Term { val tpe: Type = INT        }
  case class IF(cond: Term, ifTrue: Term, ifFalse: Term)                 extends Term { val tpe: Type = ifTrue.tpe }
  case class AND(t1: Term, t2: Term)                                     extends Term { val tpe: Type = BOOLEAN    }
  case class OR(t1: Term, t2: Term)                                      extends Term { val tpe: Type = BOOLEAN    }
  case class EQ_INT(t1: Term, t2: Term)                                  extends Term { val tpe: Type = BOOLEAN    }
  case class GT(t1: Term, t2: Term)                                      extends Term { val tpe: Type = BOOLEAN    }
  case class GE(t1: Term, t2: Term)                                      extends Term { val tpe: Type = BOOLEAN    }
  case class SIG_VERIFY(message: Term, signature: Term, publicKey: Term) extends Term { val tpe: Type = BOOLEAN    }
  case object HEIGHT                                                     extends Term { val tpe: Type = INT        }
  case class TX_FIELD(field: Field)                                      extends Term { val tpe: Type = field.tpe  }

}
