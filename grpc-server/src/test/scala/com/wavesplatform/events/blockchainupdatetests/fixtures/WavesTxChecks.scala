package com.wavesplatform.events.blockchainupdatetests.fixtures

import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.events.protobuf.StateUpdate.BalanceUpdate
import com.wavesplatform.protobuf.block.{Block, MicroBlock}
import com.wavesplatform.protobuf.transaction.*
import com.wavesplatform.protobuf.transaction.Recipient.Recipient.PublicKeyHash
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, TransactionBase}
import org.scalactic.source.Position
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers

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

  def checkTransfer(actualId: ByteString, actual: SignedTransaction, expected: TransferTransaction, publicKeyHash: Array[Byte])(implicit
      pos: Position
  ): Unit = {
    checkBaseTx(actualId, actual, expected)
    actual.transaction.wavesTransaction.value.data match {
      case Data.Transfer(value) =>
        value.amount.get.amount shouldEqual expected.amount.value
        value.recipient.get.recipient.publicKeyHash.get.toByteArray shouldBe publicKeyHash
      case _ => fail("not a transfer transaction")
    }
  }

  def checkBalances(actual: Seq[BalanceUpdate], expected: Map[(Address, Asset), (Long, Long)])(implicit pos: Position): Unit = {
    actual.map { bu =>
      (
        (Address.fromBytes(bu.address.toByteArray).explicitGet(), toVanillaAssetId(bu.amountAfter.value.assetId)),
        (bu.amountBefore, bu.amountAfter.value.amount)
      )
    }.toMap shouldEqual expected
  }
}
