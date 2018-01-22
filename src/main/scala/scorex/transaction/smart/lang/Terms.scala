package scorex.transaction.smart.lang

import scodec.bits.ByteVector

object Terms {

  sealed trait Field[T]
  case object __satisfy_shapeless_0 extends Field[Boolean]
  case object Id                    extends Field[ByteVector]
  case object Type                  extends Field[Int]
  case object SenderPk              extends Field[ByteVector]
  case object BodyBytes             extends Field[ByteVector]
  case object Proof_0               extends Field[ByteVector]
  case object Proof_1               extends Field[ByteVector]
  case object Proof_2               extends Field[ByteVector]

  sealed trait Term[T]
  case class CONST_INT(t: Int)                                                                extends INT
  case class CONST_BYTEVECTOR(bs: ByteVector)                                                 extends BYTE_VECTOR
  case class SUM(i1: INT, i2: INT)                                                            extends INT
  case class IF[T](cond: BOOL, ifTrue: Term[T], ifFalse: Term[T])                             extends Term[T]
  case class AND(t1: BOOL, t2: BOOL)                                                          extends BOOL
  case class OR(t1: BOOL, t2: BOOL)                                                           extends BOOL
  case class EQ_INT(t1: INT, t2: INT)                                                         extends BOOL
  case class GT(t1: INT, t2: INT)                                                             extends BOOL
  case class GE(t1: INT, t2: INT)                                                             extends BOOL
  case class SIG_VERIFY(message: BYTE_VECTOR, signature: BYTE_VECTOR, publicKey: BYTE_VECTOR) extends BOOL
  case object HEIGHT                                                                          extends INT
  case class TX_FIELD[T](f: Field[T])                                                            extends Term[T]

  type BOOL        = Term[Boolean]
  type INT         = Term[Int]
  type BYTE_VECTOR = Term[ByteVector]

}
