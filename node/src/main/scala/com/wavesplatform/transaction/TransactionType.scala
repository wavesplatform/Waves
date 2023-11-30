package com.wavesplatform.transaction

object TransactionType extends Enumeration(1) {
  type TransactionType = Value
  val Genesis, Payment, Issue, Transfer, Reissue, Burn, Exchange, Lease, LeaseCancel, CreateAlias, MassTransfer, Data, SetScript, SponsorFee,
      SetAssetScript, InvokeScript, UpdateAssetInfo, Ethereum, InvokeExpression = Value

  implicit class ValueExt(val tpe: TransactionType) extends AnyVal {
    def transactionName: String = s"${tpe}Transaction"
  }
}
