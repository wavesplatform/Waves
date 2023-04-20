package com.wavesplatform.events.blockchainUpdateTests.fixtures

import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.events.protobuf.StateUpdate.BalanceUpdate
import com.wavesplatform.protobuf.block.{Block, MicroBlock}
import com.wavesplatform.protobuf.transaction.*
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, TransactionBase}
import org.scalactic.source.Position
import org.scalatest.OptionValues
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

object WavesTxChecks extends Matchers with OptionValues {
  import PBAmounts.*

  def checkBaseTx(actualId: ByteString, actual: SignedTransaction, expected: TransactionBase)(implicit pos: Position): Unit = {
    ByteStr(actualId.toByteArray) shouldEqual expected.id()
    actual.transaction match {
      case SignedTransaction.Transaction.WavesTransaction(value) =>
        value.timestamp shouldEqual expected.timestamp
      case _ =>
    }
  }

  def checkCreateAlias(actualId: ByteString, actual: SignedTransaction, expected: CreateAliasTransaction)(implicit pos: Position): Unit = {
    checkBaseTx(actualId, actual, expected)
    actual.transaction.wavesTransaction.value.data match {
      case Data.CreateAlias(value) =>
        value.alias shouldEqual expected.alias.name
      case _ => fail("not a create alias transaction")
    }
  }

  def checkBalances(actual: Seq[BalanceUpdate], expected: Map[Address, (Asset, Long, Long)])(implicit pos: Position): Unit = {
    actual.map { bu =>
      Address.fromBytes(bu.address.toByteArray).explicitGet() -> (toVanillaAssetId(bu.amountAfter.value.assetId),
      bu.amountBefore,
      bu.amountAfter.value.amount)
    }.toMap shouldEqual expected
  }
}
