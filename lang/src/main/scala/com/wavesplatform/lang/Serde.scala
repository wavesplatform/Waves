package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._
import scodec._
import scodec.bits._
import scodec.codecs._

object Serde {

  import codecs.implicits._

  implicit def d[A]             = Discriminated[Expr, Int](uint8)
  implicit def dConstInt        = d[Int].bind[CONST_INT](0)
  implicit def dConstByteVector = d[ByteVector].bind[CONST_BYTEVECTOR](1)
  implicit def dSum             = d[Int].bind[SUM](2)
  implicit def dAnd             = d[Boolean].bind[AND](3)
  implicit def dIf              = d.bind[IF](4)
  implicit def dOr              = d[Boolean].bind[OR](5)
  implicit def dEqInt           = d[Boolean].bind[EQ](6)
  implicit def dGe              = d[Boolean].bind[GE](7)
  implicit def dGt              = d[Boolean].bind[GT](8)
  implicit def dSigVerify       = d[Boolean].bind[SIG_VERIFY](9)
  implicit def dHeight          = d[Int].bind[HEIGHT.type](10)
  implicit def dLet             = d.bind[LET](12)
  implicit def dComposite       = d.bind[Block](13)
  implicit def dRef             = d.bind[REF](14)
  implicit def dGet             = d.bind[GET](15)
  implicit def dIsDefined       = d.bind[IS_DEFINED](16)
  implicit def dTrue            = d.bind[TRUE.type](17)
  implicit def dFalse           = d.bind[FALSE.type](18)
  implicit def dSome            = d.bind[SOME](19)
  implicit def dNone            = d.bind[NONE.type](20)
  implicit def dGetter          = d.bind[GETTER](21)

  val codec = Codec[Expr]
}
