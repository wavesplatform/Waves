package com.wavesplatform

import org.scalacheck.Gen.{alphaNumChar, choose, listOfN, oneOf}
import org.scalacheck.{Arbitrary, Gen => G}
import org.scalatest.Suite
import scorex.account.Alias
import scorex.api.http.alias.SignedCreateAliasRequest
import scorex.api.http.assets._
import scorex.api.http.leasing.{SignedLeaseCancelRequest, SignedLeaseRequest}
import scorex.crypto.encode.Base58
import scorex.transaction.TransactionParser
import scorex.transaction.assets.{IssueTransaction, TransferTransaction}

trait RequestGen extends TransactionGen { _: Suite =>
  val nonPositiveLong: G[Long] = choose(Long.MinValue, 0).label("non-positive value")
  val invalidDecimals: G[Byte] = oneOf(
    choose[Byte](Byte.MinValue, -1),
    choose((IssueTransaction.MaxDecimals + 1).toByte, Byte.MaxValue)
  ).label("invalid decimals")

  val longAttachment: G[String] =
    genBoundedBytes(TransferTransaction.MaxAttachmentSize + 10, TransferTransaction.MaxAttachmentSize + 50)
      .map(Base58.encode)
  val invalidBase58: G[String] = listOfN(50, oneOf(alphaNumChar, oneOf('O', '0', 'l')))
    .map(_.mkString)
    .label("invalid base58")
  val invalidName: G[String] = oneOf(
    genBoundedString(0, IssueTransaction.MinAssetNameLength - 1),
    genBoundedString(IssueTransaction.MaxAssetNameLength + 1, IssueTransaction.MaxAssetNameLength + 50)
  ).map(new String(_))

  val invalidAliasStringByLength: G[String] = oneOf(
    G.choose(0, Alias.MinLength - 1) flatMap { sz => G.listOfN(sz, G.alphaNumChar) },
    G.choose(Alias.MaxLength + 1, Alias.MaxLength + 50) flatMap { sz => G.listOfN(sz, G.alphaNumChar) }
  ).map(_.mkString)

  val longDescription: G[String] =
    genBoundedBytes(IssueTransaction.MaxDescriptionLength + 1, IssueTransaction.MaxDescriptionLength + 50)
      .map(Base58.encode)

  val addressGen: G[String] = listOfN(32, Arbitrary.arbByte.arbitrary).map(b => Base58.encode(b.toArray))
  val signatureGen: G[String] = listOfN(TransactionParser.SignatureLength, Arbitrary.arbByte.arbitrary)
    .map(b => Base58.encode(b.toArray))
  private val assetIdStringGen = assetIdGen.map(_.map(_.base58))

  private val commonFields = for {
    _account <- addressGen
    _fee <- smallFeeGen
  } yield (_account, _fee)

  val issueReq: G[IssueRequest] = for {
    (account, fee) <- commonFields
    name <- genBoundedString(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
    description <- genBoundedString(0, IssueTransaction.MaxDescriptionLength)
    quantity <- positiveLongGen
    decimals <- G.choose[Byte](0, IssueTransaction.MaxDecimals.toByte)
    reissuable <- G.oneOf(true, false)
  } yield IssueRequest(account, new String(name), new String(description), quantity, decimals, reissuable, fee)

  val broadcastIssueReq: G[SignedIssueRequest] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _ir <- issueReq
  } yield SignedIssueRequest(_ir.sender, _ir.name, _ir.description, _ir.quantity, _ir.decimals, _ir.reissuable, _ir.fee,
    _timestamp, _signature)

  private val reissueBurnFields = for {
    assetId <- bytes32gen.map(Base58.encode)
    quantity <- positiveLongGen
  } yield (assetId, quantity)

  val reissueReq: G[ReissueRequest] = for {
    (account, fee) <- commonFields
    (assetId, quantity) <- reissueBurnFields
    reissuable <- G.oneOf(true, false)
  } yield ReissueRequest(account, assetId, quantity, reissuable, fee)

  val broadcastReissueReq: G[SignedReissueRequest] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _rr <- reissueReq
  } yield SignedReissueRequest(_rr.sender, _rr.assetId, _rr.quantity, _rr.reissuable, _rr.fee, _timestamp, _signature)

  val burnReq: G[BurnRequest] = for {
    (account, fee) <- commonFields
    (assetId, quantity) <- reissueBurnFields
  } yield BurnRequest(account, assetId, quantity, fee)

  val broadcastBurnReq: G[SignedBurnRequest] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _br <- burnReq
  } yield SignedBurnRequest(_br.sender, _br.assetId, _br.quantity, _br.fee, _timestamp, _signature)

  val transferReq: G[TransferRequest] = for {
    (account, fee) <- commonFields
    recipient <- accountOrAliasGen.map(_.stringRepr)
    amount <- positiveLongGen
    assetId <- assetIdStringGen
    feeAssetId <- assetIdStringGen
    attachment <- genBoundedString(1, 20).map(b => Some(Base58.encode(b)))
  } yield TransferRequest(assetId, feeAssetId, amount, fee, account, attachment, recipient)

  val broadcastTransferReq: G[SignedTransferRequest] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _tr <- transferReq
  } yield SignedTransferRequest(_tr.sender, _tr.assetId, _tr.recipient, _tr.amount, _tr.fee, _tr.feeAssetId, _timestamp,
    _tr.attachment, _signature)

  val createAliasReq: G[SignedCreateAliasRequest] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _alias <- createAliasGen
  } yield SignedCreateAliasRequest(_alias.sender.toString, _alias.fee, _alias.alias.name, _timestamp, _signature)

  val leaseReq: G[SignedLeaseRequest] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _alias <- leaseGen
  } yield SignedLeaseRequest(_alias.sender.toString, _alias.amount, _alias.fee, _alias.recipient.toString, _timestamp, _signature)

  val leaseCancelReq: G[SignedLeaseCancelRequest] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _cancel <- leaseCancelGen
  } yield SignedLeaseCancelRequest(_cancel.sender.toString, _cancel.leaseId.base58, _cancel.timestamp, _signature, _cancel.fee)
}
