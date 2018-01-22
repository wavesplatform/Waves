package scorex.transaction.smart.lang


import scodec._
import scodec.bits._
import codecs._
import com.wavesplatform.state2.ByteStr
import scorex.transaction.smart.lang.Terms._

object Serde {

  import codecs.implicits._

  implicit def f[A]       = Discriminated[Field[A], Int](uint2)
  implicit def fId        = f[ByteStr].bind[Id.type](0)
  implicit def fType      = f[Int].bind[Type.type](1)
  implicit def fSenderPk  = f[ByteStr].bind[SenderPk.type](2)
  implicit def fBodyBytes = f[ByteStr].bind[BodyBytes.type](3)
  implicit def fProof_0   = f[ByteStr].bind[Proof_0.type](4)
  implicit def fProof_1   = f[ByteStr].bind[Proof_1.type](5)
  implicit def fProof_2   = f[ByteStr].bind[Proof_2.type](6)
  implicit def f_ss_0   = f[Boolean].bind[__satisfy_shapeless_0.type](999)

  implicit def d[A]         = Discriminated[Term[A], Int](uint2)
  implicit def dConst       = d[Int].bind[CONST_INT](0)
  implicit def dSum         = d[Int].bind[SUM](1)
  implicit def dAnd         = d[Boolean].bind[AND](2)
  implicit def dIf[A]       = d[A].bind[IF[A]](3)
  implicit def dOr          = d[Boolean].bind[OR](4)
  implicit def dEqInt       = d[Boolean].bind[EQ_INT](5)
  implicit def dSigVerify   = d[Boolean].bind[SIG_VERIFY](6)
  implicit def dHeight      = d[Int].bind[HEIGHT.type](7)
  implicit def dAccessor[A] = d[A].bind[Accessor[A]](8)

  val codec = Codec[BOOL]
}
