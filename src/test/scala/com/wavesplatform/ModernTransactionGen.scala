package com.wavesplatform

import org.scalacheck.Gen
import scorex.account.{AddressScheme, PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction.modern._
import scorex.transaction.modern.assets._
import scorex.transaction.modern.lease.{LeaseCancelPayload, LeaseCancelTx, LeasePayload, LeaseTx}
import scorex.transaction.modern.smart.{SetScriptPayload, SetScriptTx}
import scorex.transaction.{TransactionParser, validation}

trait ModernTransactionGen extends TransactionGenBase {

  def versionGen(p: TransactionParser): Gen[Byte] = Gen.oneOf(p.supportedVersions.toSeq)

  def issuePayloadGen: Gen[IssuePayload] = {
    for {
      (_, name, desc, amount, decimals, reissuable, _, _) <- issueParamGen
    } yield IssuePayload(AddressScheme.current.chainId, name, desc, amount, decimals, reissuable, None)
  }

  def modernIssueReissueBurnGen(sender: PrivateKeyAccount): Gen[(IssueTx, ReissueTx, BurnTx)] = {
    for {
      (_, name, desc, amount, decimals, reissuable, fee, timestamp) <- issueParamGen
      issV                                                          <- Gen.oneOf(IssueTx.supportedVersions.toSeq)
      rissV                                                         <- Gen.oneOf(ReissueTx.supportedVersions.toSeq)
      burnV                                                         <- Gen.oneOf(BurnTx.supportedVersions.toSeq)
    } yield {
      val issH  = TxHeader(IssueTx.typeId, issV, sender, fee, timestamp)
      val rissH = TxHeader(ReissueTx.typeId, rissV, sender, fee, timestamp)
      val burnH = TxHeader(BurnTx.typeId, burnV, sender, fee, timestamp)

      val issPl = IssuePayload(AddressScheme.current.chainId, name, desc, amount, decimals, reissuable, None)

      val issueTx   = IssueTx.selfSigned(issH, issPl).get
      val reissueTx = ReissueTx.selfSigned(rissH, ReissuePayload(issueTx.assetId(), amount, reissuable)).get
      val burnTx    = BurnTx.selfSigned(burnH, BurnPayload(issueTx.assetId(), amount)).get

      (issueTx, reissueTx, burnTx)
    }
  }

  def modernIssueReissueBurnGen: Gen[(IssueTx, ReissueTx, BurnTx)] = {
    for {
      sender <- accountGen
      txs    <- modernIssueReissueBurnGen(sender)
    } yield txs
  }

  def issueTxGen: Gen[IssueTx]     = modernIssueReissueBurnGen.map(_._1)
  def reissueTxGen: Gen[ReissueTx] = modernIssueReissueBurnGen.map(_._2)
  def burnTxGen: Gen[BurnTx]       = modernIssueReissueBurnGen.map(_._3)

  def modernLeaseCancelLeaseGen: Gen[(LeaseTx, LeaseCancelTx)] = {
    for {
      (sender, amount, _, _, recipient) <- leaseParamGen
      leaseVersion                      <- Gen.oneOf(LeaseTx.supportedVersions.toSeq)
      leaseCancelVersion                <- Gen.oneOf(LeaseCancelTx.supportedVersions.toSeq)
      leaseHeader                       <- txHeaderGen(sender, LeaseTx.typeId, leaseVersion)
      leaseCancelHeader                 <- txHeaderGen(sender, LeaseCancelTx.typeId, leaseCancelVersion)
      proofs                            <- proofsGen
    } yield {
      val leasePayload = LeasePayload(amount, recipient)
      val leaseTx      = LeaseTx(leaseHeader, leasePayload, proofs)

      val leaseCancelPayload = LeaseCancelPayload(leaseTx.leaseId())
      val leaseCancelTx      = LeaseCancelTx(leaseCancelHeader, leaseCancelPayload, proofs)

      (leaseTx, leaseCancelTx)
    }
  }

  def leaseTxGen: Gen[LeaseTx]             = modernLeaseCancelLeaseGen.map(_._1)
  def leaseCancelTxGen: Gen[LeaseCancelTx] = modernLeaseCancelLeaseGen.map(_._2)

  def createAliasTxGen: Gen[CreateAliasTx] = {
    for {
      header  <- txHeaderGen(CreateAliasTx)
      payload <- aliasGen.map(a => CreateAliasPayload(a))
      proofs  <- singleProofGen
    } yield CreateAliasTx(header, payload, proofs)
  }

  def transferTxGen: Gen[TransferTx] = {
    for {
      (assetId, sender, recipient, amount, timestamp, feeId, feeAmount, attachment) <- transferParamGen
      version                                                                       <- Gen.oneOf(TransferTx.supportedVersions.toSeq)
      header  = TxHeader(TransferTx.typeId, version, sender, feeAmount, timestamp)
      payload = TransferPayload(recipient, assetId, feeId, amount, attachment)
      proofs <- singleProofGen
    } yield TransferTx(header, payload, proofs)
  }

  def setScriptTxGen: Gen[SetScriptTx] = {
    for {
      header  <- txHeaderGen(SetScriptTx)
      payload <- scriptGen.map(s => SetScriptPayload(AddressScheme.current.chainId, Some(s)))
      proofs  <- singleProofGen
    } yield SetScriptTx(header, payload, proofs)
  }

  def dataTxGen: Gen[DataTx] = {
    for {
      header <- txHeaderGen(DataTx)
      payload <- for {
        size <- Gen.choose(0, validation.MaxEntryCount)
        data <- Gen.listOfN(size, dataEntryGen)
      } yield DataPayload(data)
      proofs <- singleProofGen
    } yield DataTx(header, payload, proofs)
  }

  def sponsorFeeCancelTxGen(sender: PrivateKeyAccount): Gen[(IssueTx, SponsorFeeTx, SponsorFeeTx, CancelFeeSponsorshipTx)] = {
    for {
      issueTx             <- modernIssueReissueBurnGen(sender).map(_._1)
      sponsorHeader       <- txHeaderGen(sender, SponsorFeeTx)
      cancelSponsorHeader <- txHeaderGen(sender, CancelFeeSponsorshipTx)
      minFee              <- smallFeeGen
      minFee1             <- smallFeeGen
      sponsorPayload       = SponsorFeePayload(issueTx.assetId(), minFee)
      sponsorPayload1      = SponsorFeePayload(issueTx.assetId(), minFee1)
      cancelSponsorPayload = CancelFeeSponsorshipPayload(issueTx.assetId())
    } yield
      (
        issueTx,
        SponsorFeeTx.selfSigned(sponsorHeader, sponsorPayload).get,
        SponsorFeeTx.selfSigned(sponsorHeader, sponsorPayload1).get,
        CancelFeeSponsorshipTx.selfSigned(cancelSponsorHeader, cancelSponsorPayload).get
      )
  }

  def sponsorFeeTxGen: Gen[SponsorFeeTx] =
    for {
      sender <- accountGen
      tx     <- sponsorFeeCancelTxGen(sender).map(_._2)
    } yield tx

  def cancelFeeSponsorshipTxGen: Gen[CancelFeeSponsorshipTx] =
    for {
      sender <- accountGen
      tx     <- sponsorFeeCancelTxGen(sender).map(_._4)
    } yield tx

  def txHeaderGen(p: TransactionParser): Gen[TxHeader] = {
    for {
      version   <- versionGen(p)
      sender    <- accountGen
      fee       <- smallFeeGen
      timestamp <- timestampGen
    } yield TxHeader(p.typeId, version, sender, fee, timestamp)
  }

  def txHeaderGen(sender: PublicKeyAccount, p: TransactionParser): Gen[TxHeader] = {
    for {
      version   <- versionGen(p)
      fee       <- smallFeeGen
      timestamp <- timestampGen
    } yield TxHeader(p.typeId, version, sender, fee, timestamp)
  }

  def txHeaderGen(txType: Byte, version: Byte): Gen[TxHeader] =
    for {
      sender    <- accountGen
      fee       <- smallFeeGen
      timestamp <- timestampGen
    } yield TxHeader(txType, version, sender, fee, timestamp)

  def txHeaderGen(sender: PublicKeyAccount, txType: Byte, version: Byte): Gen[TxHeader] = {
    for {
      fee       <- smallFeeGen
      timestamp <- timestampGen
    } yield TxHeader(txType, version, sender, fee, timestamp)
  }
}
