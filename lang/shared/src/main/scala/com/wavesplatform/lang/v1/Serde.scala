package com.wavesplatform.lang.v1

import com.wavesplatform.lang.v1.FunctionHeader.FunctionHeaderType
import com.wavesplatform.lang.v1.compiler.Terms._
import scodec._
import scodec.codecs._

object Serde {

  import codecs.implicits._

  implicit def d0            = Discriminated[FunctionHeaderType, Int](uint8)
  implicit def dTypeParam    = d0.bind[FunctionHeaderType.TYPEPARAM](0)
  implicit def dOptTypeParam = d0.bind[FunctionHeaderType.OPTIONTYPEPARAM](1)
  implicit def d0Nothing     = d0.bind[FunctionHeaderType.NOTHING.type](2)
  implicit def d0Unit        = d0.bind[FunctionHeaderType.UNIT.type](3)
  implicit def d0Long        = d0.bind[FunctionHeaderType.LONG.type](4)
  implicit def d0ByteVector  = d0.bind[FunctionHeaderType.BYTEVECTOR.type](5)
  implicit def d0Boolean     = d0.bind[FunctionHeaderType.BOOLEAN.type](6)
  implicit def d0Option      = d0.bind[FunctionHeaderType.OPTION](7)
  implicit def d0TypeRef     = d0.bind[FunctionHeaderType.TYPEREF](8)
  implicit def d0String      = d0.bind[FunctionHeaderType.STRING.type](9)
  implicit def d0List      = d0.bind[FunctionHeaderType.LIST](10)
  implicit def dListTypeParam = d0.bind[FunctionHeaderType.LISTTYPEPARAM](11)

  implicit def d                = Discriminated[EXPR, Int](uint8)
  implicit def dConstInt        = d.bind[CONST_LONG](0)
  implicit def dConstByteVector = d.bind[CONST_BYTEVECTOR](1)
  implicit def dConstString     = d.bind[CONST_STRING](2)
  implicit def dIf              = d.bind[IF](3)
  implicit def dComposite       = d.bind[BLOCK](6)
  implicit def dRef             = d.bind[REF](7)
  implicit def dTrue            = d.bind[TRUE.type](8)
  implicit def dFalse           = d.bind[FALSE.type](9)
  implicit def dGetter          = d.bind[GETTER](12)
  implicit def dFunctionCall    = d.bind[FUNCTION_CALL](13)

  implicit def tD           = Discriminated[TYPE, Int](uint8)
  implicit def tDNothing    = tD.bind[NOTHING.type](0)
  implicit def tDUnit       = tD.bind[UNIT.type](1)
  implicit def tDInt        = tD.bind[LONG.type](2)
  implicit def tDByteVector = tD.bind[BYTEVECTOR.type](3)
  implicit def tDBoolean    = tD.bind[BOOLEAN.type](4)
  implicit def tDOption     = tD.bind[OPTION](5)
  implicit def tDTypeRef    = tD.bind[TYPEREF](6)
  implicit def tDString     = tD.bind[STRING.type](7)
  implicit def tDList     = tD.bind[LIST](8)

  val codec: Codec[EXPR] = Codec[EXPR]

}
