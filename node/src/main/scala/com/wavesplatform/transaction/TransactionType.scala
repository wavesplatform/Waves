package com.wavesplatform.transaction

enum TransactionType {
  def id: Int                 = ordinal + 1
  def transactionName: String = s"${this.toString}Transaction"

  case Genesis, Payment, Issue, Transfer, Reissue, Burn, Exchange, Lease, LeaseCancel, CreateAlias, MassTransfer, Data, SetScript, SponsorFee,
    SetAssetScript, InvokeScript, UpdateAssetInfo, Ethereum, CommitToGeneration, InvokeExpression

}

object TransactionType {
  def fromId(id: Byte): TransactionType = TransactionType.fromOrdinal(id - 1)
}
