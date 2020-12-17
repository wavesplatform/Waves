package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

object FieldNames {
  val WriteSet          = "WriteSet"
  val ScriptTransfer    = "ScriptTransfer"
  val TransferSet       = "TransferSet"
  val ScriptResult      = "ScriptResult"
  val Transfers         = "transfers"
  val ScriptTransferSet = "transferSet"
  val Data              = "data"
  val ScriptWriteSet    = "writeSet"

  val DataEntry    = "DataEntry"
  val BooleanEntry = "BooleanEntry"
  val StringEntry  = "StringEntry"
  val BinaryEntry  = "BinaryEntry"
  val IntegerEntry = "IntegerEntry"
  val DeleteEntry  = "DeleteEntry"

  val Recipient = "recipient"
  val Amount    = "amount"
  val Asset     = "asset"
  val Key       = "key"
  val Value     = "value"

  val IssueScript       = "Script"
  val Issue             = "Issue"
  val IssueName         = "name"
  val IssueQuantity     = "quantity"
  val IssueDecimals     = "decimals"
  val IssueDescription  = "description"
  val IssueScriptField  = "compiledScript"
  val IssueIsReissuable = "isReissuable"
  val IssueNonce        = "nonce"

  val Reissue             = "Reissue"
  val ReissueAssetId      = "assetId"
  val ReissueQuantity     = "quantity"
  val ReissueIsReissuable = "isReissuable"

  val Burn         = "Burn"
  val BurnAssetId  = "assetId"
  val BurnQuantity = "quantity"

  val SponsorFee        = "SponsorFee"
  val SponsorFeeAssetId = "assetId"
  val SponsorFeeMinFee  = "minSponsoredAssetFee"

  val Lease          = "Lease"
  val LeaseRecipient = "recipient"
  val LeaseAmount    = "amount"
  val LeaseNonce     = "nonce"

  val LeaseCancel = "LeaseCancel"
  val LeaseId     = "leaseId"
}
