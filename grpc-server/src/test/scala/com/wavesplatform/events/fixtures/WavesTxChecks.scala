package com.wavesplatform.events.fixtures

import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.protobuf.StateUpdate.AssetDetails.AssetScriptInfo
import com.wavesplatform.events.protobuf.StateUpdate.{AssetDetails, BalanceUpdate, DataEntryUpdate, LeaseUpdate, LeasingUpdate, ScriptUpdate}
import com.wavesplatform.events.protobuf.TransactionMetadata
import com.wavesplatform.protobuf.Amount
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
import org.scalatest.matchers.{MatchResult, Matcher}

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
      case _ => fail("not a Data transaction")
    }
  }

  def checkSetScriptTransaction(actualId: ByteString, actual: SignedTransaction, expected: SetScriptTransaction)(implicit pos: Position): Unit = {
    checkBaseTx(actualId, actual, expected)
    actual.transaction.wavesTransaction.value.data match {
      case Data.SetScript(value) =>
        value.script.toByteArray shouldBe expected.script.get.bytes.value().arr
      case _ => fail("not a SetScript transaction")
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
      case _ => fail("not a SetAssetScript transaction")
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
      case _ => fail("not a UpdateAssetInfo transaction")
    }
  }

  def checkSponsorFeeTransaction(actualId: ByteString, actual: SignedTransaction, expected: SponsorFeeTransaction)(implicit pos: Position): Unit = {
    checkBaseTx(actualId, actual, expected)
    val expectedAmount = expected.minSponsoredAssetFee.map(_.toString.toLong).getOrElse(0L)
    actual.transaction.wavesTransaction.value.data match {
      case Data.SponsorFee(value) =>
        value.minFee.value.assetId.toByteArray shouldBe expected.asset.id.arr
        value.minFee.value.amount shouldBe expectedAmount
      case _ => fail("not a SponsorFee transaction")
    }
  }

  def checkInvokeTransaction(actualId: ByteString, actual: SignedTransaction, expected: TransactionBase, publicKeyHash: Array[Byte])(implicit
      pos: Position
  ): Unit = {
    checkBaseTx(actualId, actual, expected)
    actual.transaction.wavesTransaction.value.data match {
      case Data.InvokeScript(value) =>
        value.dApp.get.getPublicKeyHash.toByteArray shouldBe publicKeyHash
      case _ => fail("not a InvokeScript transaction")
    }
  }

  def checkEthereumInvokeTransaction(actualId: ByteString, actual: SignedTransaction, expected: EthereumTransaction, publicKeyHash: Array[Byte])(
      implicit pos: Position
  ): Unit = {
    checkBaseTx(actualId, actual, expected)
    actual.transaction.wavesTransaction.value.data match {
      case Data.InvokeScript(value) =>
        value.dApp.get.getPublicKeyHash.toByteArray shouldBe publicKeyHash
      case _ => fail("not a InvokeScript transaction")
    }
  }

  def checkInvokeBaseTransactionMetadata(transactionMetadata: TransactionMetadata, expected: InvokeScriptTransaction)(implicit
      pos: Position
  ): Unit = {
    val invokeScript = transactionMetadata.getInvokeScript

    transactionMetadata.senderAddress.toByteArray shouldBe expected.senderAddress.bytes
    invokeScript.dAppAddress.toByteArray shouldBe expected.dApp.bytes
    invokeScript.functionName shouldBe expected.funcCallOpt.get.function.funcName
  }

  def checkEthereumInvokeBaseTransactionMetadata(
      transactionMetadata: TransactionMetadata,
      expected: EthereumTransaction,
      funcName: String,
      dAppAddress: Address
  )(implicit
      pos: Position
  ): Unit = {
    val ethereumMetadata = transactionMetadata.getEthereum

    transactionMetadata.senderAddress.toByteArray shouldBe expected.senderAddress.value().bytes
    ethereumMetadata.getInvoke.dAppAddress.toByteArray shouldBe dAppAddress.bytes
    ethereumMetadata.getInvoke.functionName shouldBe funcName
    ethereumMetadata.fee shouldBe expected.fee
    ethereumMetadata.timestamp shouldBe expected.timestamp
    ethereumMetadata.senderPublicKey.toByteArray shouldBe expected.sender.arr
  }

  def checkArguments(expectedValues: List[Any], actualArguments: List[Any]): Unit = {
    if (expectedValues.size != actualArguments.size) {
      throw new IllegalArgumentException("Number of expected values does not match the number of actual arguments.")
    }
    expectedValues.zip(actualArguments).foreach(item => item._1 shouldBe item._2)
  }

  def checkInvokeScriptResultData(result: Seq[DataTransactionData.DataEntry], actualData: Seq[(String, Any)])(implicit pos: Position): Unit = {
    if (result.length != actualData.length) {
      throw new IllegalArgumentException("Number of expected values does not match the number of actual data arguments.")
    }

    result.zip(actualData).foreach { case (entry, (key, expectedValue)) =>
      if (entry.key != key) {
        throw new IllegalArgumentException(s"Key mismatch: expected '$key', but got '${entry.key}'.")
      }

      val actualValue = if (entry.value.isDefined) entry.value.value match {
        case byteStr: ByteString => Some(byteStr.toByteArray)
        case otherValue          => Some(otherValue)
      }
      else None

      actualValue.getOrElse(None) shouldBe expectedValue
    }
  }

  def checkInvokeScriptResultIssues(actualIssue: InvokeScriptResult.Issue, expectedIssues: Map[String, Any])(implicit
      pos: Position
  ): Unit = {
    expectedIssues.foreach { case (fieldName, expectedValue) =>
      val actualValue = fieldName match {
        case "name"        => actualIssue.name
        case "description" => actualIssue.description
        case "amount"      => actualIssue.amount
        case "decimals"    => actualIssue.decimals
        case "nonce"       => actualIssue.nonce
        case _             => throw new IllegalArgumentException(s"Unknown issue field: $fieldName")
      }
      actualValue shouldBe expectedValue
    }
  }

  def checkInvokeScriptBaseInvokes(invoke: InvokeScriptResult.Invocation, address: Address, funcName: String): Unit = {
    val actualAddress = Address.fromBytes(invoke.dApp.toByteArray).explicitGet()
    actualAddress shouldBe address
    invoke.call.get.function shouldBe funcName
  }

  def checkInvokeScriptInvokesArgs(actualArgument: InvokeScriptResult.Call.Argument, expectedValue: Any): Unit = {
    val actualEntry = actualArgument.value

    if (actualEntry.isDefined && actualEntry.isBinaryValue) {
      actualEntry.binaryValue.get.toByteArray shouldBe expectedValue
    } else if (actualEntry.isDefined) {
      actualEntry.value shouldBe expectedValue
    }
  }

  def checkInvokeScriptInvokesPayments(actualPayment: Amount, asset: Asset, amount: Long): Unit = {
    val actualAssetId = toVanillaAssetId(actualPayment.assetId)
    actualAssetId shouldBe asset
    actualPayment.amount shouldBe amount
  }

  def checkInvokeScriptResultTransfers(transfer: InvokeScriptResult.Payment, address: Address, amount: Long, assetId: Asset)(implicit
      pos: Position
  ): Unit = {
    val actualAddress = Address.fromBytes(transfer.address.toByteArray).explicitGet()
    val actualAssetId = toVanillaAssetId(transfer.amount.value.assetId)

    actualAddress shouldBe address
    transfer.amount.value.amount shouldBe amount
    actualAssetId shouldBe assetId
  }

  def checkInvokeScriptResultReissue(reissue: InvokeScriptResult.Reissue, assetId: Asset, amount: Long, reissuable: Boolean)(implicit
      pos: Position
  ): Unit = {
    val actualAssetId = toVanillaAssetId(reissue.assetId)
    actualAssetId shouldBe assetId
    reissue.amount shouldBe amount
    reissue.isReissuable shouldBe reissuable
  }

  def checkInvokeScriptResultBurn(burn: InvokeScriptResult.Burn, assetId: Asset, amount: Long)(implicit
      pos: Position
  ): Unit = {
    val actualAssetId = toVanillaAssetId(burn.assetId)

    actualAssetId shouldBe assetId
    burn.amount shouldBe amount
  }

  def checkInvokeScriptResultSponsorFee(sponsorFee: InvokeScriptResult.SponsorFee, assetId: Asset, amount: Long)(implicit
      pos: Position
  ): Unit = {
    val actualAssetId = toVanillaAssetId(sponsorFee.minFee.value.assetId)
    actualAssetId shouldBe assetId
    sponsorFee.minFee.value.amount shouldBe amount
  }

  def checkInvokeScriptResultLease(leases: InvokeScriptResult.Lease, publicKeyHash: Array[Byte], amount: Long): Unit = {
    leases.recipient.value.getPublicKeyHash.toByteArray shouldBe publicKeyHash
    leases.amount shouldBe amount
  }

  def checkInvokeScriptResultLeaseCancel(leasesCancel: InvokeScriptResult.LeaseCancel, leaseId: Array[Byte]): Unit = {
    leasesCancel.leaseId.toByteArray shouldBe leaseId
  }

  def checkEthereumTransaction(actualId: ByteString, actual: SignedTransaction, expected: EthereumTransaction)(implicit pos: Position): Unit = {
    checkBaseTx(actualId, actual, expected)
    withClue("ethereum transaction bytes") {
      actual.transaction.ethereumTransaction.value.toByteArray shouldBe expected.bytes()
    }
  }

  class BalanceUpdateMatcher(expected: Map[(Address, Asset), (Long, Long)]) extends Matcher[Seq[BalanceUpdate]] {
    override def apply(actualBalances: Seq[BalanceUpdate]): MatchResult = {
      val mismatchedBalancesB = Seq.newBuilder[(Address, Asset, (Long, Long), (Long, Long))]
      val unexpectedBalancesB = Seq.newBuilder[BalanceUpdate]
      val unmetExpectations = actualBalances.foldLeft(expected) { case (prevExpected, b) =>
        val addr  = Address.fromBytes(b.address.toByteArray).explicitGet()
        val asset = toVanillaAssetId(b.amountAfter.value.assetId)
        prevExpected.get(addr -> asset) match {
          case Some((expectedBefore, expectedAfter)) =>
            if (expectedBefore != b.amountBefore || expectedAfter != b.amountAfter.value.amount) {
              mismatchedBalancesB += ((addr, asset, (expectedBefore, expectedAfter), (b.amountBefore, b.amountAfter.value.amount)))
            }
            prevExpected.removed(addr -> asset)
          case None =>
            unexpectedBalancesB += b
            prevExpected
        }
      }

      val mismatched = mismatchedBalancesB.result()
      val unexpected = unexpectedBalancesB.result()

      val errorMessage = new StringBuilder("Actual balances did not match expectations.")
      if (unmetExpectations.nonEmpty) {
        errorMessage.append("\nThe following expected balance updates were not found:")
        unmetExpectations.foreach { case ((address, asset), (before, after)) =>
          errorMessage.append(s"\n$address $asset: $before->$after")
        }
      }
      if (unexpected.nonEmpty) {
        errorMessage.append("\nThe following balance updates were not expected:")
        unexpected.foreach { b =>
          errorMessage.append(
            s"\n${Address.fromBytes(b.address.toByteArray).explicitGet()} ${toVanillaAssetId(b.amountAfter.value.assetId)}: ${b.amountBefore} -> ${b.amountAfter.value.amount}"
          )
        }
      }
      if (mismatched.nonEmpty) {
        errorMessage.append("The following balances did not match:")
        mismatched.foreach { case (address, asset, expected, actual) =>
          errorMessage.append(
            s"\n$address $asset: expected $expected != actual $actual"
          )
        }
      }

      MatchResult(
        unmetExpectations.isEmpty && mismatched.isEmpty && unexpected.isEmpty,
        errorMessage.toString(),
        "Actual balances did not differ from the expected balances"
      )
    }
  }

  def matchBalances(expected: Map[(Address, Asset), (Long, Long)]): Matcher[Seq[BalanceUpdate]] = new BalanceUpdateMatcher(expected)

  def checkBalances(actual: Seq[BalanceUpdate], expected: Map[(Address, Asset), (Long, Long)])(implicit pos: Position): Unit =
    actual should matchBalances(expected)

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

  def checkAssetsStateUpdates(actual: Option[AssetDetails], expected: Map[String, Any], assetId: Asset, issuer: Array[Byte]): Unit = {
    toVanillaAssetId(actual.get.assetId) shouldBe assetId
    actual.get.issuer.toByteArray shouldBe issuer
    actual.get.name shouldBe expected.apply("name")
    actual.get.description shouldBe expected.apply("description")
    actual.get.volume shouldBe expected.apply("amount")
    actual.get.decimals shouldBe expected.apply("decimals")
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

  def checkIndividualLeases(
      actual: Seq[LeaseUpdate],
      expected: Map[(LeaseStatus, Long), (Array[Byte], Array[Byte], Array[Byte], Array[Byte])]
  ): Unit = {
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
