package com.wavesplatform.lang.v1

import com.wavesplatform.lang.v1.compiler.Terms._
import scodec._
import scodec.codecs._

object Serde {

  import codecs.implicits._

  implicit def d                = Discriminated[EXPR, Int](uint8)
  implicit def dConstInt        = d.bind[CONST_LONG](0)
  implicit def dConstByteVector = d.bind[CONST_BYTEVECTOR](1)
  implicit def dConstString     = d.bind[CONST_STRING](2)
  implicit def dIf              = d.bind[IF](3)
  implicit def dComposite       = d.bind[BLOCK](4)
  implicit def dRef             = d.bind[REF](5)
  implicit def dTrue            = d.bind[TRUE.type](6)
  implicit def dFalse           = d.bind[FALSE.type](7)
  implicit def dGetter          = d.bind[GETTER](8)
  implicit def dFunctionCall    = d.bind[FUNCTION_CALL](9)

  implicit def tD            = Discriminated[TYPE, Int](uint8)
  implicit def tDNothing     = tD.bind[NOTHING.type](0)
  implicit def tDUnit        = tD.bind[UNIT.type](1)
  implicit def tDInt         = tD.bind[LONG.type](2)
  implicit def tDByteVector  = tD.bind[BYTEVECTOR.type](3)
  implicit def tDBoolean     = tD.bind[BOOLEAN.type](4)
  implicit def tDOption      = tD.bind[OPTION](5)
  implicit def tDString      = tD.bind[STRING.type](6)
  implicit def tDCaseTypeRef = tD.bind[CASETYPEREF](7)
  implicit def tDUnion       = tD.bind[UNION](8)
  implicit def tDList        = tD.bind[LIST](9)

  val codec: Codec[EXPR] = Codec[EXPR]

}
