package com.wavesplatform.events.blockchainupdatetests.fixtures

import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.events.protobuf.StateUpdate.BalanceUpdate
import com.wavesplatform.events.protobuf.StateUpdate.AssetStateUpdate
import com.wavesplatform.protobuf.transaction.*
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.IssueTransaction
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
        val assetId = if (value.getFee.assetId.isEmpty) Waves else value.getFee.assetId
        value.timestamp shouldEqual expected.timestamp
        assetId shouldEqual expected.assetFee._1
        value.fee.value.amount shouldEqual expected.assetFee._2
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

  def checkIssue(actualId: ByteString, actual: SignedTransaction, expected: IssueTransaction)(implicit
      pos: Position
  ): Unit = {
    checkBaseTx(actualId, actual, expected)
    actual.transaction.wavesTransaction.value.data match {
      case Data.Issue(value) =>
        value.amount shouldEqual expected.quantity.value
        value.decimals shouldBe expected.decimals.value
        value.name shouldBe expected.name.toStringUtf8
        value.description shouldBe expected.description.toStringUtf8
        if (!value.script.isEmpty) value.script.toByteArray shouldBe expected.script.head.bytes.apply().arr

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

  def checkAssetsAfter(actual: Seq[AssetStateUpdate], expected: IssueTransaction, isNft: Boolean): Unit = {
    val after = actual.head.after.get

    after.assetId.toByteArray shouldBe expected.asset.id.arr
    after.issuer.toByteArray shouldBe expected.sender.arr
    after.name shouldBe expected.name.toStringUtf8
    after.description shouldBe expected.description.toStringUtf8
    after.nft shouldBe isNft
    after.volume shouldBe expected.quantity.value
    after.decimals shouldBe expected.decimals.value
    after.scriptInfo.get.script.toByteArray shouldBe expected.script.get.bytes.value().arr
    after.reissuable shouldBe expected.reissuable
  }

  def checkAssetsBefore(actual: Seq[AssetStateUpdate], expected: IssueTransaction, isNft: Boolean): Unit = {
    val before = actual.head.before
      if (before.isDefined) {
        before.get.assetId.toByteArray shouldBe expected.asset.id.arr
        before.get.issuer.toByteArray shouldBe expected.sender.arr
        before.get.name shouldBe expected.name.toStringUtf8
        before.get.description shouldBe expected.description.toStringUtf8
        before.get.nft shouldBe isNft
        before.get.volume shouldBe expected.quantity.value
        before.get.decimals shouldBe expected.decimals.value
        before.get.scriptInfo.get.script.toByteArray shouldBe expected.script.get.bytes.value().arr
        before.get.reissuable shouldBe expected.reissuable
      }
  }
}
