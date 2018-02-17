package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._
import scodec._
import scodec.bits._
import scodec.codecs._

object Serde {

  import codecs.implicits._

  implicit def d[A]             = Discriminated[Typed.EXPR, Int](uint8)
  implicit def dConstInt        = d[Int].bind[Typed.CONST_INT](0)
  implicit def dConstByteVector = d[ByteVector].bind[Typed.CONST_BYTEVECTOR](1)
  implicit def dSum             = d[Int].bind[Typed.SUM](2)
  implicit def dAnd             = d[Boolean].bind[Typed.AND](3)
  implicit def dIf              = d.bind[Typed.IF](4)
  implicit def dOr              = d[Boolean].bind[Typed.OR](5)
  implicit def dEqInt           = d[Boolean].bind[Typed.EQ](6)
  implicit def dGe              = d[Boolean].bind[Typed.GE](7)
  implicit def dGt              = d[Boolean].bind[Typed.GT](8)
  implicit def dSigVerify       = d[Boolean].bind[Typed.SIG_VERIFY](9)
  implicit def dLet             = d.bind[Typed.LET](12)
  implicit def dComposite       = d.bind[Typed.BLOCK](13)
  implicit def dRef             = d.bind[Typed.REF](14)
  implicit def dGet             = d.bind[Typed.GET](15)
  implicit def dIsDefined       = d.bind[Typed.IS_DEFINED](16)
  implicit def dTrue            = d.bind[Typed.TRUE.type](17)
  implicit def dFalse           = d.bind[Typed.FALSE.type](18)
  implicit def dSome            = d.bind[Typed.SOME](19)
  implicit def dNone            = d.bind[Typed.NONE.type](20)
  implicit def dGetter          = d.bind[Typed.GETTER](21)

  implicit def td[A]            = Discriminated[TYPE, Int](uint8)
  implicit def tdNothing        = td.bind[NOTHING.type](0)
  implicit def tdUnit           = td.bind[UNIT.type](1)
  implicit def tdInt            = td.bind[INT.type](2)
  implicit def tdByteVector     = td.bind[BYTEVECTOR.type](3)
  implicit def tdBoolean        = td.bind[BOOLEAN.type](4)
  implicit def tdOption         = td.bind[OPTION](5)
  implicit def tdTypeRef        = td.bind[TYPEREF](6)

  val codec = Codec[Typed.EXPR]
}
