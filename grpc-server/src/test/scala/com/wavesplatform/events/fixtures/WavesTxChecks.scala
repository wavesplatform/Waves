package com.wavesplatform.events.fixtures

import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.events.protobuf.StateUpdate.LeaseUpdate
import com.wavesplatform.events.protobuf.StateUpdate.{AssetStateUpdate, BalanceUpdate, LeasingUpdate}
import com.wavesplatform.protobuf.transaction.*
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
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
      case _ => fail("not a Transfer transaction")
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

      case _ => fail("not a Issue transaction")
    }
  }

  def checkReissue(actualId: ByteString, actual: SignedTransaction, expected: ReissueTransaction)(implicit
      pos: Position
  ): Unit = {
    checkBaseTx(actualId, actual, expected)
    actual.transaction.wavesTransaction.value.data match {
      case Data.Reissue(value) =>
        value.assetAmount.get.assetId.toByteArray shouldEqual expected.asset.id.arr
        value.assetAmount.get.amount shouldEqual expected.quantity.value
        value.reissuable shouldBe expected.reissuable
      case _ => fail("not a Reissue transaction")
    }
  }

  def checkBurn(actualId: ByteString, actual: SignedTransaction, expected: BurnTransaction)(implicit
      pos: Position
  ): Unit = {
    checkBaseTx(actualId, actual, expected)
    actual.transaction.wavesTransaction.value.data match {
      case Data.Burn(value) =>
        value.assetAmount.get.assetId.toByteArray shouldEqual expected.asset.id.arr
        value.assetAmount.get.amount shouldEqual expected.quantity.value
      case _ => fail("not a Burn transaction")
    }
  }

  def checkExchange(actualId: ByteString, actual: SignedTransaction, expected: ExchangeTransaction)(implicit pos: Position): Unit = {
    checkBaseTx(actualId, actual, expected)
    actual.transaction.wavesTransaction.value.data match {
      case Data.Exchange(value) =>
        value.amount shouldEqual expected.amount.value
        value.price shouldEqual expected.price.value
        value.buyMatcherFee shouldEqual expected.buyMatcherFee
        value.sellMatcherFee shouldEqual expected.sellMatcherFee

        // order1
        value.orders.head.chainId shouldEqual expected.chainId
        value.orders.head.sender.senderPublicKey.get.toByteArray shouldBe expected.order1.sender.arr
        value.orders.head.matcherPublicKey.toByteArray shouldBe expected.order1.matcherPublicKey.arr
        value.orders.head.assetPair.get.amountAssetId.toByteArray shouldEqual expected.order1.assetPair.amountAsset.compatId.get.arr
        value.orders.head.assetPair.get.priceAssetId.toByteArray shouldEqual expected.order1.assetPair.priceAsset.compatId.get.arr
        value.orders.head.orderSide.toString() equalsIgnoreCase expected.order1.orderType.toString
        value.orders.head.amount shouldEqual expected.order1.amount.value
        value.orders.head.price shouldEqual expected.order1.price.value
        value.orders.head.timestamp shouldEqual expected.order1.timestamp
        value.orders.head.expiration shouldEqual expected.order1.expiration
        value.orders.head.matcherFee.get.amount shouldEqual expected.order1.matcherFee.value
        toVanillaAssetId(value.orders.head.matcherFee.get.assetId) shouldBe expected.order1.matcherFeeAssetId
        value.orders.head.version shouldEqual expected.order1.version

        // order2
        value.orders.last.chainId shouldEqual expected.chainId
        value.orders.last.sender.senderPublicKey.get.toByteArray shouldBe expected.order2.sender.arr
        value.orders.last.matcherPublicKey.toByteArray shouldBe expected.order2.matcherPublicKey.arr
        value.orders.last.assetPair.get.amountAssetId.toByteArray shouldEqual expected.order2.assetPair.amountAsset.compatId.get.arr
        value.orders.last.assetPair.get.priceAssetId.toByteArray shouldEqual expected.order2.assetPair.priceAsset.compatId.get.arr
        value.orders.last.orderSide.toString() equalsIgnoreCase expected.order2.orderType.toString
        value.orders.last.amount shouldEqual expected.order2.amount.value
        value.orders.last.price shouldEqual expected.order2.price.value
        value.orders.last.timestamp shouldEqual expected.order2.timestamp
        value.orders.last.expiration shouldEqual expected.order2.expiration
        value.orders.last.matcherFee.get.amount shouldEqual expected.order2.matcherFee.value
        toVanillaAssetId(value.orders.last.matcherFee.get.assetId) shouldBe expected.order2.matcherFeeAssetId
        value.orders.last.version shouldEqual expected.order2.version
      case _ => fail("not a Exchange transaction")
    }
  }

  def checkLease(actualId: ByteString, actual: SignedTransaction, expected: LeaseTransaction, publicKeyHash: Array[Byte])(implicit
      pos: Position
  ): Unit = {
    checkBaseTx(actualId, actual, expected)
    actual.transaction.wavesTransaction.value.data match {
      case Data.Lease(value) =>
        value.recipient.get.recipient.publicKeyHash.get.toByteArray shouldBe publicKeyHash
        value.amount shouldBe expected.amount.value
      case _ => fail("not a Lease transaction")
    }
  }

  def checkLeaseCancel(actualId: ByteString, actual: SignedTransaction, expected: LeaseCancelTransaction)(implicit
      pos: Position
  ): Unit = {
    checkBaseTx(actualId, actual, expected)
    actual.transaction.wavesTransaction.value.data match {
      case Data.LeaseCancel(value) =>
        value.leaseId.toByteArray shouldBe expected.leaseId.arr
      case _ => fail("not a LeaseCancel transaction")
    }
  }

  def checkMassTransfer(actualId: ByteString, actual: SignedTransaction, expected: MassTransferTransaction, pkHashes: Seq[Array[Byte]])(implicit
      pos: Position
  ): Unit = {
    checkBaseTx(actualId, actual, expected)
    actual.transaction.wavesTransaction.value.data match {
      case Data.MassTransfer(value) =>
        value.assetId.toByteArray shouldBe expected.assetId.compatId.get.arr
        value.transfers.foreach(actualTransfer =>
          expected.transfers.foreach(expectedTransfer => actualTransfer.amount shouldBe expectedTransfer.amount.value)
        )
        value.transfers.zip(pkHashes).foreach(item => item._1.getRecipient.getPublicKeyHash.toByteArray shouldBe item._2)
      case _ => fail("not a MassTransfer transaction")
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

  def checkMassTransferBalances(actual: Seq[BalanceUpdate], expected: Map[(Address, Asset), (Long, Long)])(implicit pos: Position): Unit = {
    val actualBalances = actual.map { bu =>
      ((Address.fromBytes(bu.address.toByteArray).explicitGet(), toVanillaAssetId(bu.amountAfter.value.assetId)), (bu.amountBefore, bu.amountAfter.value.amount))
    }.toMap
    val matchingKeys = actualBalances.keySet.intersect(expected.keySet)
    matchingKeys.foreach { key =>
      actualBalances(key) shouldEqual expected(key)
    }
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
    after.reissuable shouldBe expected.reissuable
    if (after.scriptInfo.isDefined) after.scriptInfo.get.script.toByteArray shouldBe expected.script.get.bytes.value().arr
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
      before.get.reissuable shouldBe expected.reissuable
      if (before.get.scriptInfo.isDefined) before.get.scriptInfo.get.script.toByteArray shouldBe expected.script.get.bytes.value().arr
    }
  }

  def checkLeasingForAddress(actual: Seq[LeasingUpdate], expected: Map[(Address, Long, Long), (Long, Long)]): Unit = {
    actual.map { bu =>
      (
        (Address.fromBytes(bu.address.toByteArray).explicitGet(), bu.inAfter, bu.outAfter),
        (bu.inBefore, bu.outBefore)
      )
    }.toMap shouldBe expected
  }

  def checkIndividualLeases(actual: Seq[LeaseUpdate], expected: Map[(String, Long), (Array[Byte], Array[Byte], Array[Byte], Array[Byte])]): Unit = {
    actual.map { bu =>
      (
        (bu.statusAfter.toString().toLowerCase(), bu.amount),
        (
          bu.leaseId.toByteArray,
          bu.sender.toByteArray,
          bu.recipient.toByteArray,
          bu.originTransactionId.toByteArray
        )
      )
    }.toMap equals expected
  }
}
