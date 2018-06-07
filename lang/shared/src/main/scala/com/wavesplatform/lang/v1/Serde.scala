package com.wavesplatform.lang.v1

import com.wavesplatform.lang.v1.compiler.Terms._
import scodec._
import scodec.codecs._

object Serde {

  import codecs.implicits._

  implicit val dFunctionHeader       = Discriminated[FunctionHeader, Boolean](bool)
  implicit val dFunctionHeaderSystem = dFunctionHeader.bind[FunctionHeader.Predef](false)
  implicit val dFunctionHeaderUser   = dFunctionHeader.bind[FunctionHeader.User](true)

  implicit val d                = Discriminated[EXPR, Int](uint8)
  implicit val dConstInt        = d.bind[CONST_LONG](0)
  implicit val dConstByteVector = d.bind[CONST_BYTEVECTOR](1)
  implicit val dConstString     = d.bind[CONST_STRING](2)
  implicit val dIf              = d.bind[IF](3)
  implicit val dComposite       = d.bind[BLOCK](4)
  implicit val dRef             = d.bind[REF](5)
  implicit val dTrue            = d.bind[TRUE.type](6)
  implicit val dFalse           = d.bind[FALSE.type](7)
  implicit val dGetter          = d.bind[GETTER](8)
  implicit val dFunctionCall    = d.bind[FUNCTION_CALL](9)

  val codec: Codec[EXPR] = Codec[EXPR]

}
