package com.wavesplatform.lang.v1.traits.domain

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.{StdLibVersion, V4}
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_BYTESTR, CONST_LONG, CONST_STRING, EVALUATED}

sealed trait TransferAttachment {
  val evaluated: EVALUATED =
    this match {
      case BytesTransferAttachment(bytes)                 => CONST_BYTESTR(bytes).explicitGet()
      case GenericTransferAttachment(IntValue(value))     => CONST_LONG(value)
      case GenericTransferAttachment(BooleanValue(value)) => CONST_BOOLEAN(value)
      case GenericTransferAttachment(StringValue(value))  => CONST_STRING(value).explicitGet()
      case GenericTransferAttachment(ByteStrValue(value)) => CONST_BYTESTR(value).explicitGet()
    }
}
case class BytesTransferAttachment(bytes: ByteStr)           extends TransferAttachment
case class GenericTransferAttachment(value: AttachmentValue) extends TransferAttachment

object TransferAttachment {
  def bytesExact(bytes: ByteStr, version: StdLibVersion): TransferAttachment =
    if (version >= V4) GenericTransferAttachment(ByteStrValue(bytes))
    else BytesTransferAttachment(bytes)
}

sealed trait AttachmentValue
case class IntValue(value: Int)         extends AttachmentValue
case class BooleanValue(value: Boolean) extends AttachmentValue
case class StringValue(value: String)   extends AttachmentValue
case class ByteStrValue(value: ByteStr) extends AttachmentValue