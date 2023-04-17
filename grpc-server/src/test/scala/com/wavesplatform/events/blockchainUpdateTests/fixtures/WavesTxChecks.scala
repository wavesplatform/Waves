package com.wavesplatform.events.blockchainUpdateTests.fixtures

import com.wavesplatform.account.Address
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.protobuf.block.{Block, MicroBlock}
import com.wavesplatform.protobuf.transaction.{CreateAliasTransactionData, IssueTransactionData, ReissueTransactionData, Transaction, TransferTransactionData}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.{CreateAliasTransaction, TransactionBase}
import org.scalatest.matchers.should.Matchers

class WavesTxChecks(append: Append, index: Int = 0) extends BlockInfo with Matchers {
  override var blockInfo: Block           = append.getBlock.getBlock
  override var microBlockInfo: MicroBlock = append.getMicroBlock.getMicroBlock.getMicroBlock
  var wavesTransaction: Transaction       = _

  if (blockInfo.serializedSize.==(index)) {
    wavesTransaction = microBlockInfo.transactions.apply(index).transaction.wavesTransaction.get
  } else {
    wavesTransaction = blockInfo.transactions.apply(index).transaction.wavesTransaction.get
  }

  def txId(index: Int): Array[Byte] = append.transactionIds.apply(index).toByteArray

  def txFeeAmount: Long = wavesTransaction.getFee.amount

  def txFeeAsset: String = {
    if (wavesTransaction.getFee.assetId.isEmpty) Waves.toString else wavesTransaction.getFee.assetId.toString
  }

  def issueTxData: IssueTransactionData = wavesTransaction.getIssue

  def transferTxData: TransferTransactionData = wavesTransaction.getTransfer

  def reissueTxData: ReissueTransactionData = wavesTransaction.getReissue

  def aliasTxData: CreateAliasTransactionData = wavesTransaction.getCreateAlias

  def baseTxCheckers(transaction: TransactionBase, txIndex: Int): Unit = {
    txId(txIndex) shouldBe transaction.id.apply().arr
    txFeeAmount shouldEqual transaction.fee
    txFeeAsset shouldBe transaction.feeAssetId.toString
  }

  def checkAliasTx(alias: CreateAliasTransaction): Unit = aliasTxData.alias shouldBe alias.alias.name

  def checkTransferTx(amount: Long, recipientAddress: Address): Unit = {
    transferTxData.amount.get.amount shouldBe amount
    transferTxData.recipient.get.recipient.publicKeyHash.get.toByteArray shouldBe recipientAddress.publicKeyHash
  }
}
