package com.wavesplatform.events.blockchainUpdateTests.fixtures

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.protobuf.block.{Block, MicroBlock}
import com.wavesplatform.protobuf.transaction.{
  CreateAliasTransactionData,
  IssueTransactionData,
  ReissueTransactionData,
  Transaction,
  TransferTransactionData
}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, TransactionBase}
import org.scalatest.matchers.should.Matchers

case class WavesTxChecks(append: Append, index: Int = 0) extends Matchers {
  val blockInfo: Block           = append.getBlock.getBlock
  val microBlockInfo: MicroBlock = append.getMicroBlock.getMicroBlock.getMicroBlock
  val wavesTransaction: Transaction =
    if (blockInfo.serializedSize.==(index)) microBlockInfo.transactions.apply(index).transaction.wavesTransaction.get
    else blockInfo.transactions.apply(index).transaction.wavesTransaction.get

  val txFeeAsset: Asset =
    if (wavesTransaction.getFee.assetId.isEmpty) Waves
    else IssuedAsset.apply(ByteStr(wavesTransaction.getFee.assetId.toByteArray))

  def txId(index: Int): Array[Byte] = append.transactionIds.apply(index).toByteArray

  def txFeeAmount: Long = wavesTransaction.getFee.amount

  def issueTxData: IssueTransactionData = wavesTransaction.getIssue

  def transferTxData: TransferTransactionData = wavesTransaction.getTransfer

  def reissueTxData: ReissueTransactionData = wavesTransaction.getReissue

  def aliasTxData: CreateAliasTransactionData = wavesTransaction.getCreateAlias

  def baseTxCheckers(transaction: TransactionBase, txIndex: Int): Unit = {
    txId(txIndex) shouldBe transaction.id.apply().arr
    txFeeAmount shouldEqual transaction.fee
    txFeeAsset shouldBe transaction.feeAssetId
  }

  def checkAliasTx(alias: CreateAliasTransaction): Unit = aliasTxData.alias shouldBe alias.alias.name

  def checkTransferTx(amount: Long, recipientAddress: Address): Unit = {
    transferTxData.amount.get.amount shouldBe amount
    transferTxData.recipient.get.recipient.publicKeyHash.get.toByteArray shouldBe recipientAddress.publicKeyHash
  }
}
