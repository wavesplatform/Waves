package com.wavesplatform.lang

import com.wavesplatform.lang.Terms.{TYPEPLACEHOLDER, _}
import scodec._
import scodec.codecs._

object Serde {

  import codecs.implicits._

  implicit def d                = Discriminated[Typed.EXPR, Int](uint8)
  implicit def dConstInt        = d.bind[Typed.CONST_LONG](0)
  implicit def dConstByteVector = d.bind[Typed.CONST_BYTEVECTOR](1)
  implicit def dConstString     = d.bind[Typed.CONST_STRING](1)
  implicit def dBinaryOp        = d.bind[Typed.BINARY_OP](2)
  implicit def dIf              = d.bind[Typed.IF](3)
  implicit def dBlock           = d.bind[Typed.BLOCK](4)
  implicit def dRef             = d.bind[Typed.REF](5)
  implicit def dTrue            = d.bind[Typed.TRUE.type](6)
  implicit def dFalse           = d.bind[Typed.FALSE.type](7)
  implicit def dGetter          = d.bind[Typed.GETTER](8)
  implicit def dFunctionCall    = d.bind[Typed.FUNCTION_CALL](9)

  implicit def bkD    = Discriminated[BINARY_OP_KIND, Int](uint8)
  implicit def bkDSum = bkD.bind[SUM_OP.type](0)
  implicit def bkDAnd = bkD.bind[AND_OP.type](1)
  implicit def bkDOr  = bkD.bind[OR_OP.type](2)
  implicit def bkDEq  = bkD.bind[EQ_OP.type](3)
  implicit def bkDGt  = bkD.bind[GT_OP.type](4)
  implicit def bkDGe  = bkD.bind[GE_OP.type](5)

  val codec: Codec[Typed.EXPR] =  Codec[Typed.EXPR]

}
