package scorex.transaction.smart.lang

import scodec._
import scodec.bits._
import codecs._
import scorex.transaction.smart.lang.Terms._

object Serde {

  import codecs.implicits._

  implicit def f[A]                   = Discriminated[Field, Int](uint8)
  implicit def fId                    = f[ByteVector].bind[Id.type](0)
  implicit def fType                  = f[Int].bind[Type.type](1)
  implicit def fSenderPk              = f[ByteVector].bind[SenderPk.type](2)
  implicit def fBodyBytes             = f[ByteVector].bind[BodyBytes.type](3)
  implicit def fProof_0               = f[ByteVector].bind[Proof_0.type](4)
  implicit def fProof_1               = f[ByteVector].bind[Proof_1.type](5)
  implicit def fProof_2               = f[ByteVector].bind[Proof_2.type](6)
  implicit def f__satisfy_shapeless_0 = f[Boolean].bind[__satisfy_shapeless_0.type](999)

  implicit def d[A]             = Discriminated[Term, Int](uint8)
  implicit def dConstInt        = d[Int].bind[CONST_INT](0)
  implicit def dConstByteVector = d[ByteVector].bind[CONST_BYTEVECTOR](1)
  implicit def dSum             = d[Int].bind[SUM](2)
  implicit def dAnd             = d[Boolean].bind[AND](3)
  implicit def dIf[A]           = d[A].bind[IF](4)
  implicit def dOr              = d[Boolean].bind[OR](5)
  implicit def dEqInt           = d[Boolean].bind[EQ_INT](6)
  implicit def dGe              = d[Boolean].bind[GE](7)
  implicit def dGt              = d[Boolean].bind[GT](8)
  implicit def dSigVerify       = d[Boolean].bind[SIG_VERIFY](9)
  implicit def dHeight          = d[Int].bind[HEIGHT.type](10)
  implicit def dField[A]        = d[A].bind[TX_FIELD](11)

  implicit def dLet[A]       = d[A].bind[LET](12)
  implicit def dComposite[A] = d[A].bind[COMPOSITE](13)
  implicit def dRef    = d.bind[REF](13)

  val codec = Codec[Term]
}
