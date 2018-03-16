package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._
import scodec._
import scodec.codecs._

object Serde {

  import codecs.implicits._

  implicit def d                = Discriminated[Typed.EXPR, Int](uint8)
  implicit def dConstInt        = d.bind[Typed.CONST_LONG](0)
  implicit def dConstByteVector = d.bind[Typed.CONST_BYTEVECTOR](1)
  implicit def dBinaryOp        = d.bind[Typed.BINARY_OP](2)
  implicit def dIf              = d.bind[Typed.IF](3)
  implicit def dComposite       = d.bind[Typed.BLOCK](6)
  implicit def dRef             = d.bind[Typed.REF](7)
  implicit def dTrue            = d.bind[Typed.TRUE.type](8)
  implicit def dFalse           = d.bind[Typed.FALSE.type](9)
  implicit def dGetter          = d.bind[Typed.GETTER](12)
  implicit def dFunctionCall    = d.bind[Typed.FUNCTION_CALL](13)

  implicit def bkD    = Discriminated[BINARY_OP_KIND, Int](uint8)
  implicit def bkDSum = bkD.bind[SUM_OP.type](0)
  implicit def bkDAnd = bkD.bind[AND_OP.type](1)
  implicit def bkDOr  = bkD.bind[OR_OP.type](2)
  implicit def bkDEq  = bkD.bind[EQ_OP.type](3)
  implicit def bkDGt  = bkD.bind[GT_OP.type](4)
  implicit def bkDGe  = bkD.bind[GE_OP.type](5)

  implicit def tD           = Discriminated[TYPE, Int](uint8)
  implicit def tDNothing    = tD.bind[NOTHING.type](0)
  implicit def tDUnit       = tD.bind[UNIT.type](1)
  implicit def tDInt        = tD.bind[LONG.type](2)
  implicit def tDByteVector = tD.bind[BYTEVECTOR.type](3)
  implicit def tDBoolean    = tD.bind[BOOLEAN.type](4)
  implicit def tDOption     = tD.bind[OPTION](5)
  implicit def tDTypeRef    = tD.bind[TYPEREF](6)

  val codec: Codec[Typed.EXPR] = Codec[Typed.EXPR]

}
