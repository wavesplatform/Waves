package com.wavesplatform

import monix.eval.TaskCircuitBreaker.Timestamp
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Suite
import scorex.account.{AddressOrAlias, AddressScheme, PrivateKeyAccount}
import scorex.transaction.AssetId
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.base._
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.modern.TxHeader
import scorex.transaction.modern.assets._
import scorex.transaction.modern.lease.{LeaseCancelPayload, LeaseCancelTx, LeasePayload, LeaseTx}

trait TransactionGen extends OldTransactionGen with ModernTransactionGen { self: Suite =>

  def anyIssueTxGen(sender: PrivateKeyAccount,
                    assetName: Array[Byte],
                    description: Array[Byte],
                    amount: Long,
                    decimals: Byte,
                    reissuable: Boolean,
                    fee: Long,
                    timestamp: Timestamp): Gen[IssueTxBase] = {
    val oldIssueTx = IssueTransaction
      .create(sender, assetName, description, amount, decimals, reissuable, fee, timestamp)
      .right
      .get

    val newIssueTx = IssueTx
      .selfSigned(
        TxHeader(IssueTx.typeId, 3, sender, fee, timestamp),
        IssuePayload(AddressScheme.current.chainId, assetName, description, amount, decimals, reissuable, None)
      )
      .get

    Gen.oneOf(oldIssueTx, newIssueTx)
  }

  def anyReissueTxGen(sender: PrivateKeyAccount,
                      assetId: AssetId,
                      amount: Long,
                      reissuable: Boolean,
                      fee: Long,
                      timestamp: Long): Gen[ReissueTxBase] = {
    val oldReissueTx = ReissueTransaction
      .create(sender, assetId, amount, reissuable, fee, timestamp)
      .right
      .get

    val newReissueTx = ReissueTx
      .selfSigned(
        TxHeader(ReissueTx.typeId, 3, sender, fee, timestamp),
        ReissuePayload(assetId, amount, reissuable)
      )
      .get

    Gen.oneOf(oldReissueTx, newReissueTx)
  }

  def anyBurnTxGen(sender: PrivateKeyAccount, assetId: AssetId, amount: Long, fee: Long, timestamp: Long): Gen[BurnTxBase] = {
    val oldBurnTx = BurnTransaction.create(sender, assetId, amount, fee, timestamp).right.get
    val newBurnTx = BurnTx
      .selfSigned(
        TxHeader(BurnTx.typeId, 3, sender, fee, timestamp),
        BurnPayload(assetId, amount)
      )
      .get

    Gen.oneOf(oldBurnTx, newBurnTx)
  }

  def anyIssueReissueBurnGenerator(issueQuantity: Long,
                                   reissueQuantity: Long,
                                   burnQuantity: Long,
                                   sender: PrivateKeyAccount): Gen[(IssueTxBase, ReissueTxBase, BurnTxBase)] = {
    for {
      (_, assetName, description, _, decimals, reissuable, iFee, timestamp) <- issueParamGen
      burnAmount                                                            <- Gen.choose(0L, burnQuantity)
      reissuable2                                                           <- Arbitrary.arbitrary[Boolean]
      fee                                                                   <- smallFeeGen
      issue                                                                 <- anyIssueTxGen(sender, assetName, description, issueQuantity, decimals, reissuable, iFee, timestamp)
      reissue                                                               <- anyReissueTxGen(sender, issue.assetId(), reissueQuantity, reissuable2, fee, timestamp)
      burn                                                                  <- anyBurnTxGen(sender, issue.assetId(), burnQuantity, fee, timestamp)
    } yield (issue, reissue, burn)
  }

  val anyIssueReissueBurnGenerator: Gen[(IssueTxBase, ReissueTxBase, BurnTxBase)] = {
    for {
      sender <- accountGen
      iq     <- Gen.choose(Long.MaxValue / 200, Long.MaxValue / 100)
      rq     <- Gen.choose(Long.MaxValue / 200, Long.MaxValue / 100)
      bq     <- Gen.choose(0, iq)
      result <- anyIssueReissueBurnGenerator(iq, rq, bq, sender)
    } yield result
  }

  def anyLeaseCancelLeaseGen(leaseSender: PrivateKeyAccount,
                             recipient: PrivateKeyAccount,
                             leaseCancelSender: PrivateKeyAccount): Gen[(LeaseTxBase, LeaseCancelTxBase)] = {
    for {
      (_, amount, fee, timestamp, _) <- leaseParamGen
      lease <- for {
        leaseHeader <- txHeaderGen(leaseSender, LeaseTx)
        leasePayload = LeasePayload(amount, recipient)
        tx <- Gen.oneOf(
          LeaseTransaction.create(leaseSender, amount, fee, timestamp, recipient).right.get,
          LeaseTx.selfSigned(leaseHeader, leasePayload).get
        )
      } yield tx
      leaseId = lease.id()
      leaseCancel <- for {
        leaseCancelHeader <- txHeaderGen(leaseCancelSender, LeaseCancelTx)
        leaseCancelPayload = LeaseCancelPayload(leaseId)
        tx <- Gen.oneOf(
          LeaseCancelTransaction.create(leaseCancelSender, leaseId, fee, timestamp + 1).right.get,
          LeaseCancelTx.selfSigned(leaseCancelHeader, leaseCancelPayload).get
        )
      } yield tx
    } yield (lease, leaseCancel)
  }

  def wavesAnyTransferGen(sender: PrivateKeyAccount, recipient: AddressOrAlias): Gen[TransferTxBase] = {
    for {
      (_, _, _, amount, timestamp, _, feeAmount, attachment) <- transferParamGen
      transfer <- for {
        header <- txHeaderGen(sender, TransferTx)
        payload = TransferPayload(recipient, None, None, amount, Array.emptyByteArray)
        tx <- Gen.oneOf(
          TransferTx.selfSigned(header, payload).get,
          TransferTransaction.create(None, sender, recipient, amount, timestamp, None, feeAmount, attachment).right.get
        )
      } yield tx
    } yield transfer
  }
}
