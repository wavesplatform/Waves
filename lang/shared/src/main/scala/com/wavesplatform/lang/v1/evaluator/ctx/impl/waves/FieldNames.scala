package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

object FieldNames {
  val WriteSet         = "WriteSet"
  val ContractTransfer = "ContractTransfer"
  val TransferSet      = "TransferSet"
  val ContractResult   = "ContractResult"
  val Transfers        = "transfers"
  val Data             = "data"
  val DataEntry        = "DataEntry"
  val Recipient        = "recipient"
  val Amount           = "amount"
  val Asset            = "asset"
  val Key              = "key"
  val Value            = "value"

  lazy val ExpectedContractResult = s"$WriteSet($Data: List[DataEntry($Key: String, $Value: Int|String|Boolean|ByteVector)]" +
    s" or " +
    s"$TransferSet($Transfers: List[$ContractTransfer($Recipient: Address, $Amount: Int, $Asset: ByteBector|Unit)]" +
    s" or " +
    s"$ContractResult($Data: $WriteSet, $Transfers: $TransferSet)"
  lazy val Error = s"CallableFunction needs to return $ExpectedContractResult or it super type"

}
