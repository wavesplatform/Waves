package com.wavesplatform.lang.v1.traits.domain

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl

sealed trait TransferAttachment {
  val evaluated: EVALUATED =
    this match {
      case IntValue(value)     => CONST_LONG(value)
      case BooleanValue(value) => CONST_BOOLEAN(value)
      case StringValue(value)  => CONST_STRING(value).explicitGet()
      case ByteStrValue(value) => CONST_BYTESTR(value).explicitGet()
      case EmptyAttachment     => impl.unit
    }
}
case class IntValue(value: Int)         extends TransferAttachment
case class BooleanValue(value: Boolean) extends TransferAttachment
case class StringValue(value: String)   extends TransferAttachment
case class ByteStrValue(value: ByteStr) extends TransferAttachment
case object EmptyAttachment             extends TransferAttachment