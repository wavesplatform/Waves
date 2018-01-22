package scorex.transaction.smart.lang

import com.wavesplatform.state2.ByteStr

object Terms {

  sealed trait Term[T]

  type BOOL = Term[Boolean]
  type INT = Term[Int]
  type BYTEARRAY = Term[ByteStr]

  case class CONST_INT[T](t: T) extends Term[T]

  case class SUM(i1: INT, i2: INT) extends INT
  case class IF[T](cond: BOOL, ifTrue: Term[T], ifFalse: Term[T])
      extends Term[T]
  case class AND(t1: BOOL, t2: BOOL) extends BOOL
  case class OR(t1: BOOL, t2: BOOL) extends BOOL
  case class EQ_INT(t1: INT, t2: INT) extends BOOL
  case class SIG_VERIFY(message: BYTEARRAY,
                        signature: BYTEARRAY,
                        publicKey: BYTEARRAY)
      extends BOOL
  case object HEIGHT extends INT
  object Transaction
  case object TX extends Term[Transaction.type]

  sealed trait Field[T]
  case object Id extends Field[ByteStr]
  case object Type extends Field[Int]
  case object SenderPk extends Field[ByteStr]
  case object BodyBytes extends Field[ByteStr]
  case object Proof_0 extends Field[ByteStr]
  case object Proof_1 extends Field[ByteStr]
  case object Proof_2 extends Field[ByteStr]
  case object Proof_3 extends Field[ByteStr]
  case object Proof_4 extends Field[ByteStr]
  case object Proof_5 extends Field[ByteStr]
  case object Proof_6 extends Field[ByteStr]
  case object Proof_7 extends Field[ByteStr]
  case class Accessor[T](tx: TX.type, f: Field[T]) extends Term[T]

}
