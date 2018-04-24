package scorex.transaction

import com.wavesplatform.TransactionGenBase
import org.scalacheck.Gen
import scorex.account.{AddressScheme, PublicKeyAccount}
import scorex.transaction.modern._
import scorex.transaction.modern.assets._
import scorex.transaction.modern.lease.{LeaseCancelPayload, LeaseCancelTx, LeasePayload, LeaseTx}
import scorex.transaction.modern.smart.{SetScriptPayload, SetScriptTx}

trait ModernTransactionGen extends TransactionGenBase {
  def issuePayloadGen: Gen[IssuePayload] = {
    for {
      (_, name, desc, amount, decimals, reissuable, _, _) <- issueParamGen
    } yield IssuePayload(AddressScheme.current.chainId, name, desc, amount, decimals, reissuable, None)
  }

  def modernIssueReissueBurnGen: Gen[(IssueTx, ReissueTx, BurnTx)] = {
    for {
      (sender, name, desc, amount, decimals, reissuable, fee, timestamp) <- issueParamGen
      issV                                                               <- Gen.oneOf(IssueTx.supportedVersions.toSeq)
      rissV                                                              <- Gen.oneOf(ReissueTx.supportedVersions.toSeq)
      burnV                                                              <- Gen.oneOf(BurnTx.supportedVersions.toSeq)
      proofs                                                             <- proofsGen
    } yield {
      val issH  = TxHeader(IssueTx.typeId, issV, sender, fee, timestamp)
      val rissH = TxHeader(ReissueTx.typeId, rissV, sender, fee, timestamp)
      val burnH = TxHeader(BurnTx.typeId, burnV, sender, fee, timestamp)

      val issPl = IssuePayload(AddressScheme.current.chainId, name, desc, amount, decimals, reissuable, None)

      val issueTx   = IssueTx(issH, issPl, proofs)
      val reissueTx = ReissueTx(rissH, ReissuePayload(issueTx.assetId(), amount, reissuable), proofs)
      val burnTx    = BurnTx(burnH, BurnPayload(issueTx.assetId(), amount), proofs)

      (issueTx, reissueTx, burnTx)
    }
  }

  def issueTxGen: Gen[IssueTx]     = modernIssueReissueBurnGen.map(_._1)
  def reissueTxGen: Gen[ReissueTx] = modernIssueReissueBurnGen.map(_._2)
  def burnTxGen: Gen[BurnTx]       = modernIssueReissueBurnGen.map(_._3)

  def modernLeaseCancelLeaseGen: Gen[(LeaseTx, LeaseCancelTx)] = {
    for {
      (sender, amount, _, _, recipient) <- leaseParamGen
      leaseVersion                                <- Gen.oneOf(LeaseTx.supportedVersions.toSeq)
      leaseCancelVersion                               <- Gen.oneOf(LeaseCancelTx.supportedVersions.toSeq)
      leaseHeader                                <- txHeaderGen(sender, LeaseTx.typeId, leaseVersion)
      leaseCancelHeader                               <- txHeaderGen(sender, LeaseCancelTx.typeId, leaseCancelVersion)
      proofs                            <- proofsGen
    } yield {
      val leasePayload     = LeasePayload(amount, recipient)
      val leaseTx = LeaseTx(leaseHeader, leasePayload, proofs)

      val leaseCancelPayload          = LeaseCancelPayload(leaseTx.leaseId())
      val leaseCancelTx = LeaseCancelTx(leaseCancelHeader, leaseCancelPayload, proofs)

      (leaseTx, leaseCancelTx)
    }
  }

  def leaseTxGen: Gen[LeaseTx]             = modernLeaseCancelLeaseGen.map(_._1)
  def leaseCancelTxGen: Gen[LeaseCancelTx] = modernLeaseCancelLeaseGen.map(_._2)

  def createAliasTxGen: Gen[CreateAliasTx] = {
    for {
      version <- Gen.oneOf(CreateAliasTx.supportedVersions.toSeq)
      header  <- txHeaderGen(CreateAliasTx.typeId, version)
      payload <- aliasGen.map(a => CreateAliasPayload(a))
      proofs  <- proofsGen
    } yield CreateAliasTx(header, payload, proofs)
  }

  def transferTxGen: Gen[TransferTx] = {
    for {
      (assetId, sender, recipient, amount, timestamp, feeId, feeAmount, attachment) <- transferParamGen
      version                                                                       <- Gen.oneOf(TransferTx.supportedVersions.toSeq)
      header  = TxHeader(TransferTx.typeId, version, sender, feeAmount, timestamp)
      payload = TransferPayload(recipient, assetId, feeId, amount, attachment)
      proofs <- proofsGen
    } yield TransferTx(header, payload, proofs)
  }

  def setScriptTxGen: Gen[SetScriptTx] = {
    for {
      version <- Gen.oneOf(SetScriptTx.supportedVersions.toSeq)
      header  <- txHeaderGen(SetScriptTx.typeId, version)
      payload <- scriptGen.map(s => SetScriptPayload(AddressScheme.current.chainId, Some(s)))
      proofs  <- proofsGen
    } yield SetScriptTx(header, payload, proofs)
  }

  def dataTxGen: Gen[DataTx] = {
    for {
      version <- Gen.oneOf(DataTx.supportedVersions.toSeq)
      header  <- txHeaderGen(DataTx.typeId, version)
      payload <- for {
        size <- Gen.choose(0, validation.MaxEntryCount)
        data <- Gen.listOfN(size, dataEntryGen)
      } yield DataPayload(data)
      proofs <- proofsGen
    } yield DataTx(header, payload, proofs)
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
