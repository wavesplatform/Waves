package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

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

  lazy val ExpectedScriptResult = s"$WriteSet($Data: List[DataEntry($Key: String, $Value: Int|String|Boolean|ByteVector)]" +
    s" or " +
    s"$TransferSet($Transfers: List[$ScriptTransfer($Recipient: Address, $Amount: Int, $Asset: ByteBector|Unit)]" +
    s" or " +
    s"$ScriptResult($ScriptWriteSet: $WriteSet, $ScriptTransferSet: $TransferSet)"
  lazy val Error = s"CallableFunction needs to return $ExpectedScriptResult or it super type"

}
