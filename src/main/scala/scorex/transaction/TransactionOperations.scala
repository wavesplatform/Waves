package scorex.transaction

import scorex.account.{Account, PrivateKeyAccount}
import scorex.api.http.assets.{BurnRequest, IssueRequest, TransferRequest}
import scorex.api.http.leasing.{LeaseCancelRequest, LeaseRequest}
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.state.wallet.{Payment, ReissueRequest}
import scorex.wallet.Wallet
import scorex.waves.transaction.SignedPayment

trait TransactionOperations {
  def transferAsset(request: TransferRequest, wallet: Wallet): Either[ValidationError, TransferTransaction]
  def issueAsset(request: IssueRequest, wallet: Wallet): Either[ValidationError, IssueTransaction]
  def reissueAsset(request: ReissueRequest, wallet: Wallet): Either[ValidationError, ReissueTransaction]
  def burnAsset(request: BurnRequest, wallet: Wallet): Either[ValidationError, BurnTransaction]
  def lease(request: LeaseRequest, wallet: Wallet): Either[ValidationError, LeaseTransaction]
  def leaseCancel(request: LeaseCancelRequest, wallet: Wallet): Either[ValidationError, LeaseCancelTransaction]

  def createPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long): Either[ValidationError, PaymentTransaction]
  def createPayment(payment: Payment, wallet: Wallet): Either[ValidationError, PaymentTransaction]
  def createSignedPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long, timestamp: Long): Either[ValidationError, PaymentTransaction]
  def signPayment(payment: Payment, wallet: Wallet): Either[ValidationError, PaymentTransaction]
  def broadcastPayment(payment: SignedPayment): Either[ValidationError, PaymentTransaction]
}
