package com.wavesplatform.events.fixtures

import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.fixtures.PrepareInvokeTestData.dataMap
import com.wavesplatform.events.protobuf.StateUpdate.AssetDetails.AssetScriptInfo
import com.wavesplatform.events.protobuf.StateUpdate.{AssetDetails, BalanceUpdate, DataEntryUpdate, LeaseUpdate, LeasingUpdate, ScriptUpdate}
import com.wavesplatform.events.protobuf.TransactionMetadata
import com.wavesplatform.protobuf.order.Order
import com.wavesplatform.protobuf.transaction.*
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.state.DataEntry
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, DataTransaction, EthereumTransaction, TransactionBase}
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
        checkOrders(value.orders.head, expected.order1)
        checkOrders(value.orders.last, expected.order2)
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

  def checkDataTransaction(actualId: ByteString, actual: SignedTransaction, expected: DataTransaction)(implicit pos: Position): Unit = {
    checkBaseTx(actualId, actual, expected)
    actual.transaction.wavesTransaction.value.data match {
      case Data.DataTransaction(value) =>
        val actualData   = value.data.map(entry => (entry.key, entry.value.value)).toMap
        val expectedData = expected.data.map(entry => (entry.key, entry.value)).toMap

        actualData.keySet shouldBe expectedData.keySet

        for ((key, actualValue) <- actualData) {
          val expectedValue = expectedData(key)

          if (key == "Binary") {
            actualValue shouldBe ByteString.copyFrom(Base58.decode(expectedValue.toString))
          } else {
            actualValue shouldBe expectedValue
          }
        }
      case _ => fail("not a create alias transaction")
    }
  }

  def checkSetScriptTransaction(actualId: ByteString, actual: SignedTransaction, expected: SetScriptTransaction)(implicit pos: Position): Unit = {
    checkBaseTx(actualId, actual, expected)
    actual.transaction.wavesTransaction.value.data match {
      case Data.SetScript(value) =>
        value.script.toByteArray shouldBe expected.script.get.bytes.value().arr
    }
  }

  def checkSetAssetScriptTransaction(actualId: ByteString, actual: SignedTransaction, expected: SetAssetScriptTransaction)(implicit
      pos: Position
  ): Unit = {
    checkBaseTx(actualId, actual, expected)
    actual.transaction.wavesTransaction.value.data match {
      case Data.SetAssetScript(value) =>
        value.assetId.toByteArray shouldBe expected.asset.id.arr
        value.script.toByteArray shouldBe expected.script.get.bytes.value().arr
    }
  }

  def checkUpdateAssetInfoTransaction(actualId: ByteString, actual: SignedTransaction, expected: UpdateAssetInfoTransaction)(implicit
      pos: Position
  ): Unit = {
    checkBaseTx(actualId, actual, expected)
    actual.transaction.wavesTransaction.value.data match {
      case Data.UpdateAssetInfo(value) =>
        value.assetId.toByteArray shouldBe expected.assetId.id.arr
        value.name shouldBe expected.name
    }
  }

  def checkSponsorFeeTransaction(actualId: ByteString, actual: SignedTransaction, expected: SponsorFeeTransaction)(implicit pos: Position): Unit = {
    checkBaseTx(actualId, actual, expected)
    val expectedAmount = expected.minSponsoredAssetFee.map(_.toString.toLong).getOrElse(0L)
    actual.transaction.wavesTransaction.value.data match {
      case Data.SponsorFee(value) =>
        value.minFee.value.assetId.toByteArray shouldBe expected.asset.id.arr
        value.minFee.value.amount shouldBe expectedAmount
    }
  }

  def checkInvokeTransaction(actualId: ByteString, actual: SignedTransaction, expected: InvokeScriptTransaction, publicKeyHash: Array[Byte])(implicit
      pos: Position
  ): Unit = {
    checkBaseTx(actualId, actual, expected)
    actual.transaction.wavesTransaction.value.data match {
      case Data.InvokeScript(value) =>
        value.dApp.get.getPublicKeyHash.toByteArray shouldBe publicKeyHash
    }
  }

  def checkInvokeBaseTransactionMetadata(actual: Seq[TransactionMetadata], expected: InvokeScriptTransaction, assetId: ByteStr, address: Array[Byte])(
      implicit pos: Position
  ): Unit = {
    val invokeScript = actual.head.getInvokeScript
    val arguments    = invokeScript.arguments

    actual.head.senderAddress.toByteArray shouldBe expected.senderAddress.bytes
    invokeScript.dAppAddress.toByteArray shouldBe expected.dApp.bytes
    invokeScript.functionName shouldBe expected.funcCallOpt.get.function.funcName
    arguments.head.value.binaryValue.get.toByteArray shouldBe assetId.arr
    arguments.apply(1).value.binaryValue.get.toByteArray shouldBe address
  }

  def checkInvokeResultTransactionMetadata(result: Option[InvokeScriptResult], dataMap: Map[String, Any], assetId: ByteStr, address: Address)(implicit
      pos: Position
  ): Unit = {
    val data         = result.get.data
    val transfers    = result.get.transfers
    val issues       = result.get.issues
    val reissues     = result.get.reissues
    val burns        = result.get.burns
    val sponsorFees  = result.get.sponsorFees
    val leases       = result.get.leases
    val leasesCancel = result.get.leaseCancels

    data.head.key shouldBe "int"
    data.head.value.intValue.get shouldBe dataMap.apply("intVal")
    data.apply(1).key shouldBe "byte"
    data.apply(1).value.binaryValue.value.toByteArray shouldBe assetId.arr
    data.apply(2).key shouldBe "bool"
    data.apply(2).value.boolValue.value.toString shouldBe dataMap.apply("booleanVal")
    data.apply(3).key shouldBe "str"
    data.apply(3).value.stringValue.value shouldBe dataMap.apply("stringVal")
    data.apply(4).key shouldBe "int"
    data.apply(4).value.isEmpty shouldBe true

    transfers.head.address.toByteArray shouldBe address.bytes
    transfers.head.amount.value.amount shouldBe dataMap.apply("scriptTransferAssetInt")
    transfers.head.amount.value.assetId.toByteArray shouldBe assetId.arr
    transfers.apply(1).address.toByteArray shouldBe address.bytes
    transfers.apply(1).amount.value.amount shouldBe dataMap.apply("scriptTransferIssueAssetInt")
    transfers.apply(2).address.toByteArray shouldBe address.bytes
    transfers.apply(2).amount.value.amount shouldBe dataMap.apply("scriptTransferUnitInt")

    issues.head.name shouldBe dataMap.apply("issueAssetName")
    issues.head.description shouldBe dataMap.apply("issueAssetDescription")
    issues.head.amount shouldBe dataMap.apply("issueAssetAmount")
    issues.head.decimals shouldBe dataMap.apply("issueAssetDecimals")
    issues.head.nonce shouldBe dataMap.apply("issueAssetNonce")

    reissues.head.assetId.toByteArray shouldBe assetId.arr
    reissues.head.amount shouldBe dataMap.apply("reissueInt")
    reissues.head.isReissuable shouldBe true

    burns.head.assetId.toByteArray shouldBe assetId.arr
    burns.head.amount shouldBe dataMap.apply("burnInt")

    sponsorFees.head.minFee.value.assetId.toByteArray shouldBe assetId.arr
    sponsorFees.head.minFee.value.amount shouldBe dataMap.apply("sponsorFeeAssetInt")
    sponsorFees.apply(1).minFee.value.amount shouldBe dataMap.apply("sponsorFeeIssueAssetInt")

    leases.head.recipient.get.getPublicKeyHash.toByteArray shouldBe address.publicKeyHash
    leases.head.amount shouldBe dataMap.apply("leaseInt")

    leasesCancel.head.leaseId shouldBe leases.head.leaseId
  }

  def checkEthereumTransaction(actualId: ByteString, actual: SignedTransaction, expected: EthereumTransaction)(implicit pos: Position): Unit = {
    checkBaseTx(actualId, actual, expected)
    actual.transaction.ethereumTransaction.value.toByteArray shouldBe expected.bytes.value()
  }

  def checkBalances(actual: Seq[BalanceUpdate], expected: Map[(Address, Asset), (Long, Long)])(implicit pos: Position): Unit = {
    actual.map { bu =>
      (
        (Address.fromBytes(bu.address.toByteArray).explicitGet(), toVanillaAssetId(bu.amountAfter.value.assetId)),
        (bu.amountBefore, bu.amountAfter.value.amount)
      )
    }.toMap shouldBe expected
  }

  def checkMassTransferBalances(actual: Seq[BalanceUpdate], expected: Map[(Address, Asset), (Long, Long)]): Unit = {
    val actualBalances = actual.map { bu =>
      (
        (Address.fromBytes(bu.address.toByteArray).explicitGet(), toVanillaAssetId(bu.amountAfter.value.assetId)),
        (bu.amountBefore, bu.amountAfter.value.amount)
      )
    }.toMap
    val matchingKeys = actualBalances.keySet.intersect(expected.keySet)
    matchingKeys.foreach { key =>
      actualBalances(key) shouldBe expected(key)
    }
  }

  def checkDataEntriesStateUpdate(actual: Seq[DataEntryUpdate], expectedData: Seq[DataEntry[?]], expectAddress: Array[Byte]): Unit = {
    actual.zip(expectedData).foreach { case (actualUpdate, expectedEntry) =>
      val actualEntry   = actualUpdate.dataEntry.get
      val actualKey     = actualEntry.key
      val actualValue   = if (actualEntry.value.isDefined) actualEntry.value.value else actualEntry.value
      val expectedKey   = expectedEntry.key
      val expectedValue = expectedEntry.value

      actualUpdate.address.toByteArray shouldBe expectAddress
      actualKey shouldBe expectedKey

      if (actualEntry.value.isDefined && actualEntry.value.isBinaryValue) {
        actualValue shouldBe ByteString.copyFrom(Base58.decode(expectedValue.toString))
      } else if (actualEntry.value.isDefined) {
        actualValue shouldBe expectedValue
      }
    }
  }

  def checkAssetsStateUpdates(actual: Option[AssetDetails], expected: IssueTransaction, isNft: Boolean, quantityValue: Long): Unit = {
      actual.get.assetId.toByteArray shouldBe expected.asset.id.arr
      actual.get.issuer.toByteArray shouldBe expected.sender.arr
      actual.get.name shouldBe expected.name.toStringUtf8
      actual.get.description shouldBe expected.description.toStringUtf8
      actual.get.nft shouldBe isNft
      actual.get.volume shouldBe quantityValue
      actual.get.decimals shouldBe expected.decimals.value
      actual.get.reissuable shouldBe expected.reissuable
  }

  def checkAssetsStateUpdates(actual: Option[AssetDetails], expected: BurnTransaction, isNft: Boolean, quantityAfterReissue: Long): Unit = {
    actual.get.assetId.toByteArray shouldBe expected.asset.id.arr
    actual.get.issuer.toByteArray shouldBe expected.sender.arr
    actual.get.nft shouldBe isNft
    actual.get.volume shouldBe quantityAfterReissue
  }

  def checkAssetsStateUpdates(actual: Option[AssetDetails], expected: ReissueTransaction, isNft: Boolean, quantityAfterReissue: Long): Unit = {
    actual.get.assetId.toByteArray shouldBe expected.asset.id.arr
    actual.get.issuer.toByteArray shouldBe expected.sender.arr
    actual.get.nft shouldBe isNft
    actual.get.volume shouldBe quantityAfterReissue
    actual.get.reissuable shouldBe expected.reissuable
  }

  def checkAssetsStateUpdates(actual: Option[AssetDetails], expected:  Map[String, Any], assetId: Asset, issuer: Array[Byte]): Unit = {
      toVanillaAssetId(actual.get.assetId) shouldBe assetId
      actual.get.issuer.toByteArray shouldBe issuer
      actual.get.name shouldBe expected.apply("issueAssetName")
      actual.get.description shouldBe expected.apply("issueAssetDescription")
      actual.get.volume shouldBe expected.apply("issueAssetAmount")
      actual.get.decimals shouldBe expected.apply("issueAssetDecimals")
  }

  def checkAssetUpdatesStateUpdates(actual: Option[AssetDetails], expected: UpdateAssetInfoTransaction): Unit = {
    actual.get.name shouldBe expected.name
    actual.get.description shouldBe expected.description
  }

  def checkAssetsScriptStateUpdates(actual: Option[AssetScriptInfo], expected: Array[Byte]): Unit = {
    actual.get.script.toByteArray shouldBe expected
  }

  def checkLeasingForAddress(actual: Seq[LeasingUpdate], expected: Map[(Address, Long, Long), (Long, Long)]): Unit = {
    actual.map { bu =>
      (
        (Address.fromBytes(bu.address.toByteArray).explicitGet(), bu.inAfter, bu.outAfter),
        (bu.inBefore, bu.outBefore)
      )
    }.toMap shouldBe expected
  }

  def checkIndividualLeases(actual: Seq[LeaseUpdate], expected: Map[(LeaseStatus, Long), (Array[Byte], Array[Byte], Array[Byte], Array[Byte])]): Unit = {
    actual.size shouldBe expected.size

    actual.zip(expected).foreach { case (lease, ((status, amount), (leaseId, sender, recipient, originTransactionId))) =>
      lease.statusAfter.toString() shouldBe status.toString.toUpperCase
      lease.amount shouldBe amount
      lease.leaseId.toByteArray shouldBe leaseId
      lease.sender.toByteArray shouldBe sender
      lease.recipient.toByteArray shouldBe recipient
      lease.originTransactionId.toByteArray shouldBe originTransactionId
    }
  }

  def checkSetScriptStateUpdate(actual: ScriptUpdate, expected: SetScriptTransaction)(implicit pos: Position): Unit = {
    actual.address.toByteArray shouldBe expected.sender.toAddress.bytes
    actual.after.toByteArray shouldBe expected.script.get.bytes.value().arr
    actual.before.toByteArray.isEmpty shouldBe true
  }

  private def checkOrders(order: Order, expected: exchange.Order): Unit = {
    order.sender.senderPublicKey.get.toByteArray shouldBe expected.sender.arr
    order.matcherPublicKey.toByteArray shouldBe expected.matcherPublicKey.arr
    order.assetPair.get.amountAssetId.toByteArray shouldBe expected.assetPair.amountAsset.compatId.get.arr
    order.assetPair.get.priceAssetId.toByteArray shouldBe expected.assetPair.priceAsset.compatId.get.arr
    order.orderSide.toString() equalsIgnoreCase expected.orderType.toString
    order.amount shouldBe expected.amount.value
    order.price shouldBe expected.price.value
    order.timestamp shouldBe expected.timestamp
    order.expiration shouldBe expected.expiration
    order.matcherFee.get.amount shouldBe expected.matcherFee.value
    toVanillaAssetId(order.matcherFee.get.assetId) shouldBe expected.matcherFeeAssetId
    order.version shouldBe expected.version
  }
}