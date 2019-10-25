package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V4}

object FieldNames {
  val WriteSet       = "WriteSet"
  val ScriptTransfer = "ScriptTransfer"
  val TransferSet    = "TransferSet"
  val ScriptResult   = "ScriptResult"
  val Transfers      = "transfers"
  val ScriptTransferSet    = "transferSet"
  val Data           = "data"
  val ScriptWriteSet       = "writeSet"
  val DataEntry      = "DataEntry"
  val Recipient      = "recipient"
  val Amount         = "amount"
  val Asset          = "asset"
  val Key            = "key"
  val Value          = "value"

  val DataEntryStr = s"$DataEntry($Key: String, $Value: Int|String|Boolean|ByteVector)"
  val TransferStr  = s"$ScriptTransfer($Recipient: Address, $Amount: Int, $Asset: ByteBector|Unit)"

  private val CallableV3ResultStr =
    s"$WriteSet($Data: List[$DataEntryStr])" +
    s" or " +
    s"$TransferSet($Transfers: List[$TransferStr])" +
    s" or " +
    s"$ScriptResult($ScriptWriteSet: $WriteSet, $ScriptTransferSet: $TransferSet)"

  private val CallableV4ResultStr =
    s"List[ " +
    s"$DataEntryStr..., " +
    s"$TransferStr..., "  +
    s"]"

  def callableResultError(v: StdLibVersion): String = {
    val resultStr =
      v match {
        case V3 => CallableV3ResultStr
        case V4 => CallableV4ResultStr
        case _  => ???
      }
    s"CallableFunction needs to return $resultStr"
  }
}
