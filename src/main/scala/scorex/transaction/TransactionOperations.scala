package scorex.transaction

import scorex.account.{Account, PrivateKeyAccount}
import scorex.api.http.alias.AliasRequest
import scorex.api.http.assets._
import scorex.api.http.leasing.{LeaseCancelRequest, LeaseRequest}
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.wallet.Wallet
import scorex.waves.transaction.SignedPaymentRequest

trait TransactionOperations {
  def transferAsset(request: TransferRequest, wallet: Wallet): Either[ValidationError, TransferTransaction]
  def issueAsset(request: IssueRequest, wallet: Wallet): Either[ValidationError, IssueTransaction]
  def reissueAsset(request: ReissueRequest, wallet: Wallet): Either[ValidationError, ReissueTransaction]
  def burnAsset(request: BurnRequest, wallet: Wallet): Either[ValidationError, BurnTransaction]
  def lease(request: LeaseRequest, wallet: Wallet): Either[ValidationError, LeaseTransaction]
  def alias(request: AliasRequest, wallet: Wallet): Either[ValidationError, CreateAliasTransaction]
  def leaseCancel(request: LeaseCancelRequest, wallet: Wallet): Either[ValidationError, LeaseCancelTransaction]
  def broadcastPayment(payment: SignedPaymentRequest): Either[ValidationError, PaymentTransaction]

  def createPayment(payment: PaymentRequest, wallet: Wallet): Either[ValidationError, PaymentTransaction]
  def createPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long): Either[ValidationError, PaymentTransaction]
  def createPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long, timestamp: Long): Either[ValidationError, PaymentTransaction]
}
