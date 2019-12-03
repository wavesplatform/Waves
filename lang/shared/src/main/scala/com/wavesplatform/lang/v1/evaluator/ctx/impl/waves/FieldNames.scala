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

  val DataEntry    = "DataEntry"
  val BooleanEntry = "BooleanEntry"
  val StringEntry  = "StringEntry"
  val BinaryEntry  = "BinaryEntry"
  val IntEntry     = "IntEntry"
  val DeleteEntry  = "DeleteEntry"

  val Recipient      = "recipient"
  val Amount         = "amount"
  val Asset          = "asset"
  val Key            = "key"
  val Value          = "value"

  val IssueScript = "Script"
  val Issue = "Issue"
  val IssueName         = "name"
  val IssueQuantity     = "quantity"
  val IssueDecimals     = "decimals"
  val IssueDescription  = "description"
  val IssueScriptField  = "compiledScript"
  val IssueIsReissuable = "isReissuable"

  val Reissue = "Reissue"
  val ReissueAssetId      = "assetId"
  val ReissueQuantity     = "quantity"
  val ReissueIsReissuable = "isReissuable"

  val Burn = "Burn"
  val BurnAssetId  = "assetId"
  val BurnQuantity = "quantity"
}
