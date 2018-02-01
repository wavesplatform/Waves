package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._
import scodec._
import scodec.bits._
import scodec.codecs._

object Serde {

  import codecs.implicits._

  implicit def f[A]                   = Discriminated[Field, Int](uint8)
  implicit def fId                    = f[ByteVector].bind[Id.type](0)
  implicit def fType                  = f[Int].bind[Type.type](1)
  implicit def fSenderPk              = f[ByteVector].bind[SenderPk.type](2)
  implicit def fBodyBytes             = f[ByteVector].bind[BodyBytes.type](3)
  implicit def fProof                 = f[ByteVector].bind[Proof](4)
  implicit def assetId                = f[Option[ByteVector]].bind[AssetId.type](5)
  implicit def f__satisfy_shapeless_0 = f[Boolean].bind[__satisfy_shapeless_0.type](999)

  implicit def d[A]             = Discriminated[Expr, Int](uint8)
  implicit def dConstInt        = d[Int].bind[CONST_INT](0)
  implicit def dConstByteVector = d[ByteVector].bind[CONST_BYTEVECTOR](1)
  implicit def dSum             = d[Int].bind[SUM](2)
  implicit def dAnd             = d[Boolean].bind[AND](3)
  implicit def dIf              = d.bind[IF](4)
  implicit def dOr              = d[Boolean].bind[OR](5)
  implicit def dEqInt           = d[Boolean].bind[EQ_INT](6)
  implicit def dGe              = d[Boolean].bind[GE](7)
  implicit def dGt              = d[Boolean].bind[GT](8)
  implicit def dSigVerify       = d[Boolean].bind[SIG_VERIFY](9)
  implicit def dHeight          = d[Int].bind[HEIGHT.type](10)
  implicit def dField           = d.bind[TX_FIELD](11)
  implicit def dLet             = d.bind[LET](12)
  implicit def dComposite       = d.bind[Block](13)
  implicit def dRef             = d.bind[REF](14)
  implicit def dGet             = d.bind[GET](15)
  implicit def dIsDefined       = d.bind[IS_DEFINED](16)
  implicit def dTrue            = d.bind[TRUE.type](17)
  implicit def dFalse           = d.bind[FALSE.type](18)

  val codec = Codec[Expr]
}
