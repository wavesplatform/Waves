package com.wavesplatform

import com.wavesplatform.account.Alias
import com.wavesplatform.api.http.requests.*
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto.*
import com.wavesplatform.transaction.assets.*
import org.scalacheck.Gen.{alphaNumChar, choose, listOfN, oneOf}
import org.scalacheck.{Arbitrary, Gen => G}
import org.scalatest.Suite

trait RequestGen extends TransactionGen { _: Suite =>
  val nonPositiveLong: G[Long] = choose(Long.MinValue, 0).label("non-positive value")
  val invalidDecimals: G[Byte] = oneOf(
    choose[Byte](Byte.MinValue, -1),
    choose((IssueTransaction.MaxAssetDecimals + 1).toByte, Byte.MaxValue)
  ).label("invalid decimals")

  val invalidBase58: G[String] = listOfN(50, oneOf(alphaNumChar, oneOf('O', '0', 'l')))
    .map(_.mkString)
    .label("invalid base58")
  val invalidName: G[String] = oneOf(
    genBoundedString(0, IssueTransaction.MinAssetNameLength - 1),
    genBoundedString(IssueTransaction.MaxAssetNameLength + 1, IssueTransaction.MaxAssetNameLength + 50)
  ).map(new String(_))

  val invalidAliasStringByLength: G[String] = oneOf(
    G.choose(0, Alias.MinLength - 1) flatMap { sz =>
      G.listOfN(sz, G.alphaNumChar)
    },
    G.choose(Alias.MaxLength + 1, Alias.MaxLength + 50) flatMap { sz =>
      G.listOfN(sz, G.alphaNumChar)
    }
  ).map(_.mkString)

  val longDescription: G[String] =
    genBoundedBytes(IssueTransaction.MaxAssetDescriptionLength + 1, IssueTransaction.MaxAssetDescriptionLength + 50)
      .map(Base58.encode)

  val addressGen: G[String] = listOfN(32, Arbitrary.arbByte.arbitrary).map(b => Base58.encode(b.toArray))
  val signatureGen: G[String] = listOfN(SignatureLength, Arbitrary.arbByte.arbitrary)
    .map(b => Base58.encode(b.toArray))
  private val assetIdStringGen = assetIdGen.map(_.map(_.toString))

  private val commonFields = for {
    _account <- addressGen
    _fee     <- smallFeeGen
  } yield (_account, _fee)

  val issueReq: G[IssueV1Request] = for {
    (account, fee) <- commonFields
    name           <- genBoundedString(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
    description    <- genBoundedString(0, IssueTransaction.MaxAssetDescriptionLength)
    quantity       <- positiveLongGen
    decimals       <- G.choose[Byte](0, IssueTransaction.MaxAssetDecimals.toByte)
    reissuable     <- G.oneOf(true, false)
  } yield IssueV1Request(account, new String(name), new String(description), quantity, decimals, reissuable, fee)

  val broadcastIssueReq: G[SignedIssueV1Request] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _ir        <- issueReq
  } yield SignedIssueV1Request(_ir.sender, _ir.name, _ir.description, _ir.quantity, _ir.decimals, _ir.reissuable, _ir.fee, _timestamp, _signature)

  private val reissueBurnFields = for {
    assetId  <- bytes32gen.map(Base58.encode)
    quantity <- positiveLongGen
  } yield (assetId, quantity)

  val reissueReq: G[ReissueV1Request] = for {
    (account, fee)      <- commonFields
    (assetId, quantity) <- reissueBurnFields
    reissuable          <- G.oneOf(true, false)
  } yield ReissueV1Request(account, assetId, quantity, reissuable, fee)

  val broadcastReissueReq: G[SignedReissueV1Request] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _rr        <- reissueReq
  } yield SignedReissueV1Request(_rr.sender, _rr.assetId, _rr.quantity, _rr.reissuable, _rr.fee, _timestamp, _signature)

  val burnReq: G[BurnV1Request] = for {
    (account, fee)      <- commonFields
    (assetId, quantity) <- reissueBurnFields
  } yield BurnV1Request(account, assetId, quantity, fee)

  val broadcastBurnReq: G[SignedBurnV1Request] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _br        <- burnReq
  } yield SignedBurnV1Request(_br.sender, _br.assetId, _br.amount, _br.fee, _timestamp, _signature)

  val transferReq: G[TransferV1Request] = for {
    (account, fee) <- commonFields
    recipient      <- accountOrAliasGen.map(_.toString)
    amount         <- positiveLongGen
    assetId        <- assetIdStringGen
    feeAssetId     <- assetIdStringGen
    attachment     <- genBoundedStringBytes(1, 20).map(b => Some(Base58.encode(b)))
  } yield TransferV1Request(assetId, feeAssetId, amount, fee, account, attachment, recipient)

  val broadcastTransferReq: G[SignedTransferV1Request] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _tr        <- transferReq
  } yield SignedTransferV1Request(_tr.sender, _tr.assetId, _tr.recipient, _tr.amount, _tr.fee, _tr.feeAssetId, _timestamp, _tr.attachment, _signature)

  val createAliasReq: G[SignedCreateAliasV1Request] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _alias     <- createAliasGen
  } yield SignedCreateAliasV1Request(_alias.sender.toString, _alias.fee.value, _alias.alias.name, _timestamp, _signature)

  val leaseReq: G[SignedLeaseV1Request] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _alias     <- leaseGen
  } yield SignedLeaseV1Request(_alias.sender.toString, _alias.amount.value, _alias.fee.value, _alias.recipient.toString, _timestamp, _signature)

  val leaseCancelReq: G[SignedLeaseCancelV1Request] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _cancel    <- leaseCancelGen
  } yield SignedLeaseCancelV1Request(_cancel.sender.toString, _cancel.leaseId.toString, _timestamp, _signature, _cancel.fee.value)
}
